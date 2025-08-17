;;; oai-prompt.el --- Chains of requests to LLM -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;; oai-prompt.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; oai-prompt.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with oai-prompt.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Changelog

;;; Commentary:
;; Re1
;; Sys: You a helpful. Give plan of 3 parts to research for answer and do only first part.
;; user: How to make it?
;;
;; "choices": ["message": {"role": "assistant", "content": "To..."}}]
;;
;; Re2
;; Sys: You a helpful. Give plan of 3 parts to research for answer and do only first part.
;; user: How to make it?
;; Assist: Plan and solution for 1) step.
;; user: Research 2-th part and what was missed before.
;;
;; Re3
;; Sys: You a helpful. Give plan of 3 parts to research for answer and do only first part with summary.
;; user: How to make it?
;; Assist: Plan and solution for 1) step.
;; user: Research 2-th part and what was missed before.
;; Assist: sum for 1), new plan, 2) step.
;; user: Research 3-th part and what was missed before, summarize results give final answer.
;;
;;; Code:
;;; -=-= all
(require 'org-ai-block)
(require 'org-ai-openai)
(require 'async)

(defvar oai-prompt-chain-list
  (list "Plan how to solve this with 3 steps of research and do first step."
        "Do 2th part of plan and outline what was missed before."
        "Do 3th part of plan and outline what was missed before, summarize results and give a final answer."))


(defun oai-prompt-collect-chat-research-steps-prompt (commands ind block-content &optional default-system-prompt max-tokens)
  "Compose messages for LLM for IND step of COMMANDS.
Add to result of `org-ai-openai--collect-chat-messages' CoT prompts.
Compose IND request for COMMANDS and ind-1 response.
BLOCK-CONTENT is result of `org-ai-block-get-content'.
IND count from 0.  RESP-QUEST  is list of string  of lengh IND+1  - raw
content of ai block or answer from  LLM.  We assume that commands and AI
answers except of the first one are already in block-content."
  (let* ((bcont (org-ai-openai--collect-chat-messages block-content))
         (recom (if (and org-ai-default-max-tokens-add-recomendation max-tokens)
                    (org-ai-openai--get-lenght-recomendation max-tokens)))
         (comm0 (nth 0 commands))
         (comm0 (if (and (= ind 0) recom)
                    (concat comm0 " " recom)
                  comm0))
         (comm0 (if (and default-system-prompt (not (string-empty-p default-system-prompt)))
                         (concat default-system-prompt " " comm0)
                       ;; else
                       comm0))
         (comm (nth ind commands))
         (comm (if recom (concat comm " " recom) comm))
         (sys0 (list :role 'system :content
                     comm0)))
    (apply 'vector sys0 (append bcont
                                ;; command after AI answer
                                (when (> ind 0)
                                  (list (list :role 'system :content comm)))))))

;; - Test
(cl-assert
   (equal
    (let ((org-ai-default-max-tokens-add-recomendation t)
          (max-tokens 200))
      (oai-prompt-collect-chat-research-steps-prompt oai-prompt-chain-list
                                             0
                                             "[ME:]How to make coffe?\n[AI]: IDK."
                                             ""
                                             max-tokens))
      (vector (list :role 'system :content (concat (nth 0 oai-prompt-chain-list) " " (org-ai-openai--get-lenght-recomendation 200)))
                    (list :role 'user :content "How to make coffe?")
                    (list :role 'assistant :content "IDK."))))

(cl-assert
 (equal
  (oai-prompt-collect-chat-research-steps-prompt oai-prompt-chain-list
                               1
                               "[ME:]How to make coffe?\n[AI]: IDK."
                               "Be helpful.")
  (vector (list :role 'system :content (concat "Be helpful. " (nth 0 oai-prompt-chain-list)))
    (list :role 'user :content "How to make coffe?")
    (list :role 'assistant :content "IDK.")
    (list :role 'system :content (nth 1 oai-prompt-chain-list)))))

(cl-assert
 (let (org-ai-default-max-tokens-add-recomendation)
   (equal
    (oai-prompt-collect-chat-research-steps-prompt oai-prompt-chain-list
                                           2
                                           (concat "[ME:]How to make coffe?\n[AI]: IDK.\n[SYS]: " (nth 1 oai-prompt-chain-list) "\n[AI]: IDK."))
    (vector (list :role 'system :content (nth 0 oai-prompt-chain-list))
            (list :role 'user :content "How to make coffe?")
            (list :role 'assistant :content "IDK.")
            (list :role 'system :content (nth 1 oai-prompt-chain-list))
            (list :role 'assistant :content "IDK.")
            (list :role 'system :content (nth 2 oai-prompt-chain-list))))))


(cl-assert
 (let (org-ai-default-max-tokens-add-recomendation)
   (equal
    (oai-prompt-collect-chat-research-steps-prompt oai-prompt-chain-list
                                           0
                                           "[ME:]How to make coffe?\n[AI]: IDK.")
    (vector (list :role 'system :content (nth 0 oai-prompt-chain-list))
            (list :role 'user :content "How to make coffe?")
            (list :role 'assistant :content "IDK.")))))




(defun oai-prompt-request-chain (req-type element sys-prompt sys-prompt-for-all-messages model max-tokens top-p temperature frequency-penalty presence-penalty service stream)
  "Check :pag parameter and use :step to execute chain of prompt.
Aspects:
1) start and stop reporter at begining and at the end (final callback).
2) error handling: kill reporter, kill tmp buffer, kill timers"
  (org-ai--debug "oai-prompt-agent-request-prepare1 service, model: %s %s" service model)

  (if (string-equal (alist-get :pag (org-ai-block-get-info element)) "my")
      ;; - My request
      (let ((service (or service 'github))
            (end-marker (org-ai-block--get-content-end-marker element))
            (header-marker (org-ai-block-get-header-marker element))
            ;; (gap-between-requests 3) ; TODO
            (buffer-key (get-buffer-create "*org-ai--chain-tmp*" t)) ; use one buffer as for updating global notification timer
            (step (alist-get :step (org-ai-block-get-info element)))
            )

        (let (
            (callbackmy (lambda (data callback)
                          (when data ; if not data it is fail
                            (org-ai--debug "calbackmy")
                            (org-ai--insert-single-response end-marker (concat "[AI]: " data) nil 'final)
                            (run-at-time 0 nil callback data)
                            (org-ai-timers--progress-reporter-run #'org-ai-openai--stop-tracking-url-request))))
            (calbafin (lambda (data callback)
                        (when data ; if not data it is fail
                          (org-ai--debug "calbafin")
                          (org-ai--insert-single-response end-marker (concat "[AI]: " data))
                          (org-ai--insert-single-response end-marker nil 'insertrole 'final) ; finalize
                          (org-ai-timers--interrupt-current-request (org-ai-timers--get-keys-for-variable header-marker) #'org-ai-openai--stop-tracking-url-request)
                          (org-ai-timers--interrupt-current-request buffer-key #'org-ai-openai--stop-tracking-url-request))))
            (call (lambda (step)
                    (lambda (data callback)
                      (org-ai--debug "oai-prompt-agent-request-prepare-call step %s" step)
                      (org-ai--debug "oai-prompt-agent-request-prepare-call max-tokens %s header-marker %s sys-prompt %s" max-tokens header-marker sys-prompt )
                      (org-ai-api-request-llm-retries service
                                                      model
                                                      org-ai-timers-duration
                                                      callback
                                                      :retries 3
                                                      :messages (oai-prompt-collect-chat-research-steps-prompt oai-prompt-chain-list
                                                                                                       step
                                                                                                       (with-current-buffer (marker-buffer header-marker) (string-trim (org-ai-block-get-content (org-ai-block-element-by-marker header-marker))))
                                                                                                       sys-prompt
                                                                                                       max-tokens)
                                                      :max-tokens max-tokens
                                                      :header-marker header-marker
                                                      :temperature temperature
                                                      :top-p top-p
                                                      :frequency-penalty frequency-penalty
                                                      :presence-penalty presence-penalty)))))


        (org-ai--debug "oai-prompt-agent-request-prepare2 %s %s %s %s" header-marker service model org-ai-timers-duration)
        ;;
        (start-async-chain nil
                           (list (funcall call 0)
                                 callbackmy
                                 (funcall call 1)
                                 callbackmy
                                 (funcall call 2)
                                 calbafin
                                 ))
          ;; Global reporter uppdated and run all the time.
          ;; Every task have own timer for parallel requests to retry them.
          (org-ai-timers--set buffer-key header-marker)
          (org-ai-timers--progress-reporter-run #'org-ai-openai--stop-tracking-url-request (* org-ai-timers-duration 3) )))

      ;; - else - built-in
      (org-ai--debug "ELSE")
      (org-ai-api-request-prepare req-type element sys-prompt sys-prompt-for-all-messages model max-tokens top-p temperature frequency-penalty presence-penalty service stream)
  ))


;;; provide
(provide 'oai-prompt)
;;; oai-prompt.el ends here
