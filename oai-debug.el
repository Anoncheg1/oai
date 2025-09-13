;;; oai-debug.el --- Logging for oai in separate buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 github.com/Anoncheg1

;; This file is NOT part of GNU Emacs.

;; orai-debug.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; oai.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with oai.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Code
(defcustom oai-debug-buffer nil
  "If non-nil, enable debuging to a debug buffer.
  Set to something like *debug-oai*. to enable debugging."
  :type 'string
  :group 'oai)


(defun oai--debug-get-caller()
  "Return string with name of function of caller function.
Heavy to execute."
  (let* ((backtrace-line-length 20)
         (print-level 3)
         (print-length 10)
         (bt
          ;; (with-output-to-string (backtrace))
          (backtrace-to-string (backtrace-get-frames 'backtrace))
          )
         (caller))
         ;; (print bt)
         (seq-find
          ; - predicate
          (lambda (line)
            (let* ( (mpos (string-match "(" line))
                   (sline (substring line 0 mpos))
                   (tline (string-trim-right (string-trim-left sline))))
                   (if (and (not (string-empty-p tline))
                            (not (member tline '("oai--debug-get-caller" "oai--debug" ) )))
                       (setq caller tline)
                     nil ; else
                     )))
          ;; - lines
          (cdr (split-string bt "\n" t)))
         caller))

(defvar oai--debug-filter nil
  "Output only strings that contains this.")

(defun oai--debug (&rest args)
  "If firt argument of ARGS is a stringwith %s than behave like format.
Otherwise format every to string and concatenate."
  (when (and oai-debug-buffer args)

    (save-excursion
      (let* ((buf-exist (get-buffer oai-debug-buffer))
             (bu (or buf-exist (get-buffer-create oai-debug-buffer)))
             (current-window (selected-window))
             (bu-window (or (get-buffer-window bu)
                            (if (>= (count-windows) 2)
                                (display-buffer-in-direction ; exist but hidden
                                 bu
                                 '((direction . left)
                                   (window . new)
                                   (window-width . 0.2)))
                              ;; else
                              (display-buffer-in-direction ; exist but hidden
                               bu
                               '((direction . left)
                                 (window . new)
                                 )))
                            (select-window current-window)))
             result-string)

        (with-current-buffer bu
          ;; - move point to  to bottom
          (if buf-exist ; was not created
              (goto-char (point-max))
            ;; else buffer just created
            (local-set-key "q" #'quit-window)
            )
          ;; ;; - scroll debug buffer down
          (if bu-window
              (with-selected-window (get-buffer-window bu)
                (with-no-warnings
                  (end-of-buffer nil))
                ;; (recenter '(t))
                ))
          ;; ;; - output caller function ( working, but too heavy)
          ;; (let ((caller
          ;;        (oai--debug-get-caller)))
          ;;   (when caller
          ;;     (insert "Din ")
          ;;     (insert caller)
          ;;     (insert " :")))
          ;; - output args
          (save-match-data
            (if (and (equal (type-of (car args)) 'string)
                     (string-match "%s" (car args)))
                (setq result-string (concat (apply 'format (car args) (cdr args)) "\n"))

              ;; else
              (setq result-string (apply #'concat (mapcar (lambda (arg)
                                                (if (equal (type-of arg) 'string)
                                                    (format "%s\n" arg)
                                                  (concat (prin1-to-string arg) "\n"))
                                                ) args)))
              )
            (if oai--debug-filter
                (when (string-match-p (regexp-quote oai--debug-filter) result-string)
                    (insert result-string))
              ;; else
              (insert result-string))

            ))))))


(defun oai-debug--prettify-json-string (json-string)
  "Convert a compact JSON string to a prettified JSON string.
This function uses a temporary buffer to perform the prettification.
Returns the prettified JSON string.
Argument JSON-STRING string with json."
  (condition-case err
      (let* ((parsed-json (json-read-from-string json-string))
             ;; 1. First, encode the JSON object. This will be compact with your json-encode.
             (compact-json (json-encode parsed-json)))
        (with-temp-buffer
           (insert compact-json)
           (json-pretty-print-buffer)
           (buffer-string)))
    (error
        (message "Error formatting JSON: %S" err)
        (message "Input JSON: %S" json-string))))


;; (oai--debug "test %s" 2)
;; (oai--debug "test" 2 3 "sd")

(provide 'oai-debug)
;;; oai-debug.el ends here
