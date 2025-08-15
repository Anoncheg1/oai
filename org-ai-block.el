;;; org-ai-block.el --- org-ai special block helpers -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;; org-ai.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; org-ai.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with org-ai.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Changelog
;; - DONE: rename org-ai-special-block to org-ai-block-p
;; - DONE: complete is short for completion
;; - DONE: org-ai-block-get-info fail if there is nothing in block.
;; - DONE: rename all CONTEXT to ELEMENT because context cause confusing (Org terms).
;; - DONE: rename all except interface functions to "org-ai-block-" prefix.

;;; Commentary:

;; Defines functions for dealing with #+begin_ai..#+end_ai special blocks

;; None Org babel: We choose not to fake as babel source block and use
;; functionality because it require too much advices.

;; Note Org terms:
;; - element - "room" you are in (e.g., a paragraph) (TYPE PROPS) (org-element-at-point)
;; - context - "furniture" you are touching within that room (e.g., a bold word, a link). (TYPE PROPS) (org-element-context)
;; - org-dblock-start-re

;;; Code:
;;; -=-= all
(require 'org)
(require 'org-element)
(require 'org-macs)

(when (and (boundp 'org-protecting-blocks) (listp org-protecting-blocks))
  (add-to-list 'org-protecting-blocks "ai"))

(when (boundp 'org-structure-template-alist)
  (add-to-list 'org-structure-template-alist '("A" . "ai")))


;; `org-element-with-disabled-cache' is not available pre org-mode 9.6.6, i.e.
;; emacs 28 does not ship with it
(defmacro org-ai-block--org-element-with-disabled-cache (&rest body)
  "Run BODY without active org-element-cache."
  (declare (debug (form body)) (indent 0))
  `(cl-letf (((symbol-function #'org-element--cache-active-p) (lambda (&rest _) nil)))
     ,@body))

(defun org-ai-block-p ()
  "Are we inside a #+begin_ai...#+end_ai block?
Like `org-in-src-block-p'. Return element."
  (org-ai-block--org-element-with-disabled-cache ;; with cache enabled we get weird Cached element is incorrect warnings
    (cl-loop with context = (org-element-context)
             while (and context
                        (not (equal 'special-block (org-element-type context)))
                        (not (string-equal "ai" (org-element-property :type context))))
             do (setq context (org-element-property :parent context))
             finally return context)))

(defun org-ai-block-element-by-marker (marker)
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (org-ai-block-p))))

(defun org-ai-block-get-info (&optional element no-eval)
  "Parse the header of #+begin_ai...#+end_ai block.
`ELEMENT' is the element of the special block. Return an alist of
key-value pairs.
Like `org-babel-get-src-block-info' but instead of list return only
arguments.
To get value use: (alist-get :value (org-ai-block-get-info))
Use ELEMENT only in current moment."
  (org-babel-parse-header-arguments
   (org-element-property
    :parameters
    (or element (org-ai-block-p))) no-eval))

(defun org-ai-block--string-equal-ignore-case (string1 string2)
  "Helper for backwards compat.
STRING1 and STRING2 are strings. Return t if they are equal
ignoring case."
  (eq 't (compare-strings string1 0 nil string2 0 nil t)))

(defun org-ai-block-get-content (&optional element)
  "Extracts the text content of the #+begin_ai...#+end_ai block.
`ELEMENT' is the element of the special block.

Will expand noweb templates if an 'org-ai-noweb' property or
'noweb' header arg is \"yes\".
Use ELEMENT only in current moment, if buffer modified you will need new
ELEMENT."
  (let* ((element (or element (org-ai-block-p)))
         (content-start (org-element-property :contents-begin element))
         (content-end (org-element-property :contents-end element))
         (unexpanded-content (if (or (not content-start) (not content-end))
                                 (error "Empty block")
                               ;; else
                               (string-trim (buffer-substring-no-properties content-start content-end))))
         (info (org-ai-block-get-info element))
         (noweb-control (or (alist-get :noweb info nil)
                            (org-entry-get (point) "org-ai-noweb" 1)
                            "no"))
         (content (if (org-ai-block--string-equal-ignore-case "yes" noweb-control)
                      (org-babel-expand-noweb-references (list "markdown" unexpanded-content))
                    unexpanded-content)))
    content))

(defun org-ai-block--get-request-type (info)
  "Look at the header of the #+begin_ai...#+end_ai block.
returns the type of request. `INFO' is the alist of key-value
pairs from `org-ai-block-get-info'."
  (cond
   ((not (eql 'x (alist-get :chat info 'x))) 'chat)
   ((not (eql 'x (alist-get :completion info 'x))) 'completion)
   ((not (eql 'x (alist-get :complete info 'x))) 'completion)
   ((not (eql 'x (alist-get :image info 'x))) 'image)
   ((not (eql 'x (alist-get :sd-image info 'x))) 'sd-image)
   ((not (eql 'x (alist-get :local info 'x))) 'local-chat)
   (t 'chat)))

(cl-defun org-ai-block--get-sys (&key info default)
  "Check if :sys exist in #+begin_ai parameters.
If exist return nil or string, if not exist  return `default'."
  (let ((sys-raw  (alist-get :sys info 'x)))
    ;; if 'x - not resent
    (if (eql 'x sys-raw)
        default
      ;; else - nil or string
      sys-raw)))

(defmacro org-ai-block--let-params (info definitions &rest body)
  "A specialized `let*' macro for Org-AI parameters.
DEFINITIONS is a list of (VARIABLE &optional DEFAULT-FORM &key TYPE).
TYPE can be 'number or 'identity.
Parameters are sourced from:
1. From Org-AI block header `info' alist. (e.g., :model \"gpt-4\")
2. Org inherited property. (e.g., #+PROPERTY: model gpt-4)
3. DEFAULT-FORM."
  `(let* ,(cl-loop for def-item in definitions
                   collect
                   (let* ((sym (car def-item))
                          (default-form (cadr def-item))
                          ;; Look for the :type keyword in the rest of the list
                          (type (cadr (member :type def-item))))
                     `(,sym (or (alist-get ,(intern (format ":%s" (symbol-name sym))) info)
                                ,(cond
                                  ((string= (symbol-name sym) "model") ; Special: no conversion for model
                                   `(org-entry-get-with-inheritance ,(symbol-name sym)))
                                  ((eq type 'number)
                                   `(when-let ((prop (org-entry-get-with-inheritance ,(symbol-name sym))))
                                      (if (stringp prop) (string-to-number prop) prop)))
                                  (t ; Default: identity conversion
                                   `(org-entry-get-with-inheritance ,(symbol-name sym))))
                                ,@(when default-form `(,default-form))))))
     ,@body))


(defvar org-ai-block--roles-regex "\\[SYS\\]:\\|\\[ME\\]:\\|\\[ME:\\]\\|\\[AI\\]:\\|\\[AI_REASON\\]:")

(defun org-ai-block--chat-role-regions ()
  "Splits the special block by role prompts.
Return line begining positions of first line of content, roles, #+end_ai
line."
  (if-let* ((element (org-ai-block-p))
            (content-start (org-element-property :contents-begin element))
            (content-end (org-element-property :contents-end element)))
      (let ((result (save-match-data
                      (save-excursion
                        (goto-char content-start)
                        (cl-loop with result
                                 while (search-forward-regexp org-ai-block--roles-regex content-end t) ; todo, make as global variable
                                 do (push (match-beginning 0) result)
                                 finally return result)))))
        (if result
            (cl-concatenate 'list (list content-start) (reverse result) (list content-end))
          (list content-start content-end)))))



;;; -=-= Interactive

(defcustom org-ai-block-fontify-markdown t
  "fontinfy ```lang blocks."
  :type 'boolean
  :group 'org-ai)

(defun org-ai-mark-last-region ()
  "Marks the last prompt in an org-ai block."
  (interactive)
  (when-let* ((regions (reverse (org-ai-block--chat-role-regions)))
              (last-region-end (pop regions))
              (last-region-start (pop regions)))
        (goto-char last-region-end)
        (push-mark last-region-start t t)))

(defun org-ai-mark-region-at-point ()
  "Marks the prompt at point."
  (interactive)
  (when-let* ((regions (org-ai-block--chat-role-regions))
              (start (cl-find-if (lambda (x) (>= (point) x)) (reverse regions)))
              (end (cl-find-if (lambda (x) (<= (point) x)) regions)))
    (when (= start end)
      (setq end (cl-find-if (lambda (x) (< start x)) regions)))
    (when (not end)
      (setq end start)
      (setq start (cl-find-if (lambda (x) (> end x)) (reverse regions))))
    (when (and start end)
      (goto-char start)
      (push-mark end t t)
      (cons start end))))

(defun org-ai-forward-section (&optional arg)
  "Move forward to end of section.
A negative argument ARG = -N means move backward."
  (interactive "^p")
  ;;   TODO:
  ;; With argument ARG, do it ARG times;
  ;; a negative argument ARG = -N means move backward N paragraphs.
  (when-let* ((regions (org-ai-block--chat-role-regions))
              (start (cl-find-if (lambda (x) (>= (point) x)) (reverse regions)))
              (end (cl-find-if (lambda (x) (< (point) x)) regions)))
    (print (list start end))
    (or arg (setq arg 1))
    (if (> arg 0)
        (goto-char end)
      ;; else - backward
      (let ((prev (cl-find-if (lambda (x) (>= (1- start) x)) (reverse regions))))
        (print (list (> (point) start) prev))
        (if (and (> (point) start) ; if at the middle of first section
                 (not prev))
            (goto-char start)
          ;; else
          (goto-char (cl-find-if (lambda (x) (>= (1- start) x)) (reverse regions))))))))

(defun org-ai-kill-region-at-point (&optional arg)
  "Kills the prompt at point.
The numeric `ARG' can be used for killing the last n."
  (interactive "P")
  (cl-loop repeat (or arg 1)
           do (when-let ((region (org-ai-mark-region-at-point)))
                (cl-destructuring-bind (start . end) region
                  (kill-region end start)))))

;;; -=-= Markers

(defun org-ai-block--get-content-end-marker (&optional element)
  "Return a marker for the :contents-end property of ELEMENT.
Used in `org-ai-interface-step1'"
  (let ((el (or element (org-ai-block-p))))
    (let ((contents-end-pos (org-element-property :contents-end el)))
      (when contents-end-pos
        (copy-marker contents-end-pos)))))

(defun org-ai-block-get-header-marker (&optional element)
  "Return marker for ai block at current buffer at current positon.
Use ELEMENT only in current moment."
  (let ((el (or element (org-ai-block-p))))
    ;; (with-current-buffer (org-element-property :buffer el)
    (if el
        (save-excursion
          (goto-char (org-element-property :contents-begin el))
          (forward-line -1)
          (copy-marker (point))))))

;;; -=-= Result


(defun org-ai-insert-result (result &optional result-params hash exec-time)
  "Modified `org-babel-insert-result' function.
Insert RESULT into the current buffer.
TODO: EXEC-TIME."
  (when (stringp result)
    (setq result (substring-no-properties result)))
  (save-excursion
    (let* ((visible-beg (point-min-marker))
           (visible-end (copy-marker (point-max) t))
           (existing-result (org-ai-where-is-ai-result t nil hash))
           ;; When results exist outside of the current visible
           ;; region of the buffer, be sure to widen buffer to
           ;; update them.
           (outside-scope (and existing-result
                               (buffer-narrowed-p)
                               (or (> visible-beg existing-result)
                                   (<= visible-end existing-result))))
           beg end indent)
      (unwind-protect
          (progn
            (when outside-scope (widen)) ;; ---- WIDDEN
            (goto-char existing-result) ;; must be true
            (setq indent (current-indentation))
            (forward-line 1)
            (setq beg (point))
            (cond
             ((member "replace" result-params)
              (delete-region (point) (org-babel-result-end)))
             ((member "append" result-params)
              (goto-char (org-babel-result-end)) (setq beg (point-marker))))
            (goto-char beg) (insert result)
            (setq end (copy-marker (point) t))
            (when outside-scope (narrow-to-region visible-beg visible-end)) ;; ---- NARROW
            )))))

(defun org-ai-where-is-ai-result (&optional insert _info hash)
  "Modified `org-babel-where-is-src-block-result' function."
  (let ((context (org-ai-block-p)))
    (catch :found
      (org-with-wide-buffer
       (let* ((name (org-element-property :name context))
              (named-results (and name (org-babel-find-named-result name))))
         (goto-char (or named-results (org-element-end context)))
         (print (list "name" name named-results))
         (cond
          ;; Existing results named after the current source.
          (named-results
           (when (org-babel--clear-results-maybe hash)
             (org-babel--insert-results-keyword name hash))
           (throw :found (point)))
          ;; Named results expect but none to be found.
          (name)
          ;; No possible anonymous results at the very end of
          ;; buffer or outside CONTEXT parent.
          ((eq (point)
               (or (pcase (org-element-type (org-element-parent context))
                     ((or `section `org-data)
                      (org-element-end (org-element-parent context)))
                     (_ (org-element-contents-end
                         (org-element-parent context))))
                   (point-max))))
          ;; Check if next element is an anonymous result below
          ;; the current block.
          ((let* ((next (org-element-at-point))
                  (end (save-excursion
                         (goto-char
                          (org-element-post-affiliated next))
                         (line-end-position)))
                  (empty-result-re (concat org-babel-result-regexp "$"))
                  (case-fold-search t))
             (re-search-forward empty-result-re end t))
           (forward-line 0)
           (when (org-babel--clear-results-maybe hash)
             (org-babel--insert-results-keyword nil hash))
           (throw :found (point)))))
     ;; ;; Ignore other elements.
     ;; (_ (throw :found nil))
       )
      ;; No result found.  Insert a RESULTS keyword below element, if
      ;; appropriate.  In this case, ensure there is an empty line
      ;; after the previous element.
      (when insert
        (print "here")
        (save-excursion
          (goto-char (min (org-element-end context) (point-max)))
          (skip-chars-backward " \t\n")
          (forward-line)
          (unless (bolp) (insert "\n"))
          (insert "\n")
          (org-babel--insert-results-keyword
           (org-element-property :name context) hash)
          (point)))))
)
;;



;;; -=-= Markdown block
(defvar org-ai-block--markdown-begin-re "^```\\([^ \t\n[{]+\\)[\s-]?\n")
(defvar org-ai-block--markdown-end-re "^```[\s-]?$")

(defun org-ai-block--fontify-markdown-subblocks (start end)
  "Fontify ```language ... ``` fenced mardown code blocks.
Used to call `org-src-font-lock-fontify-block' on code subblock."
  (goto-char start)
  (let ((case-fold-search t))
    (while (and (< (point) end)
                (re-search-forward org-ai-block--markdown-begin-re end t))
      (let* ((lang (match-string 1))
             (block-begin (match-end 0)))
        ;; (print (list "re-search-forward4" (point) end))
        (when (re-search-forward org-ai-block--markdown-end-re end t)
          (let ((block-end (match-beginning 0)))
            (when (fboundp (org-src-get-lang-mode lang)) ; for org-src-font-lock-fontify-block
              (org-src-font-lock-fontify-block lang block-begin block-end)
              )))))))

(defun org-ai-block--font-lock-fontify-ai-subblocks (limit)
  "Fontify Org links inside #+begin_ai ... #+end_ai blocks up to LIMIT.
We insert advice right after `org-fontify-meta-lines-and-blocks-1' witch
called as a part of Org Font Lock mode configuration of keywords and
corresponding font-lock highlighting rules in `font-lock-defaults'
variable."
  (if org-ai-block-fontify-markdown
      (let ((case-fold-search t))
        (while (and (re-search-forward "^#\\+begin_ai[^\n]*\n" limit t)
                    (< (point) limit))
          (let ((beg (match-end 0)))
            (when (re-search-forward "^#\\+end_ai.*$" nil t)
              (let ((end (match-beginning 0)))
                (save-match-data
                  (org-ai-block--fontify-markdown-subblocks beg end))
                ))))))
  ;; required by font lock mode:
  (goto-char limit)
  t)

(defun org-ai-block--insert-after (list pos element)
  "Insert ELEMENT at after position POS in LIST."
  (nconc (take (1+ pos) list) (list element) (nthcdr (1+ pos) list)))

(defun org-ai-block--set-ai-keywords()
  "Insert ower function in Org font lock keywords."
  (setq org-font-lock-extra-keywords (org-ai-block--insert-after
                                      org-font-lock-extra-keywords
                                      (seq-position org-font-lock-extra-keywords '(org-fontify-meta-lines-and-blocks))
                                      '(org-ai-block--font-lock-fontify-ai-subblocks))))

;; not used now
(defun markdown-mark-fenced-code-body (&optional limit-begin limit-end)
  "Mark content inside Markdown fenced code block (```), excluding header/footer.
LIMIT-BEGIN and LIMIT-END restrict the search region around point.
Returns t if region was marked, nil otherwise."
    (let ((point-pos (point))
          (start nil)
          (end nil))
      (save-excursion
        ;; Find start fence
        (when (re-search-backward org-ai-block--markdown-begin-re (or limit-begin (point-min)) t)
          (setq start (match-end 0))
          (goto-char point-pos)
          ;; do we inside owr block?
          (when (and (re-search-backward org-ai-block--markdown-end-re  (or limit-begin (point-min)) t)
                     (> (match-beginning 0) start)
                     (setq start nil))))
        ;; Find end fence
        (goto-char point-pos)
        (when (and start
                   (re-search-forward org-ai-block--markdown-end-re (or limit-end (point-max)) t))
          (setq end (match-beginning 0))
          (goto-char point-pos)
          ;; do we inside owr block?
          (when (and (re-search-forward org-ai-block--markdown-begin-re (or limit-end (point-max)) t)
                     (< (match-end 0) end)
                     (setq end nil)))))
      ;; If point is inside fences, mark region
      ;; (print (list point-pos start end))
      (when (and start end (> point-pos start) (< point-pos end))
        (set-mark start)
        (print end)
        (goto-char end)
        (forward-line -1)
        (end-of-line)
        (activate-mark)
        t)))
;;; provide
(provide 'org-ai-block)
;;; org-ai-block.el ends here
