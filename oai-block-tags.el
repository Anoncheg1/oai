;;; oai-block-tags.el --- Logging for oai in separate buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 github.com/Anoncheg1

;;; License

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Licensed under the GNU Affero General Public License, version 3 (AGPLv3)
;; <https://www.gnu.org/licenses/agpl-3.0.en.html>
;;; Commentary:
;; How this works?
;; We appply `oai-block-tags-replace' to text of last user request.
;; Used in oai.el: (oai-restapi--modify-last-user-content expanded #'oai-block-tags-replace)
;; To check result use
;;
;; We support @links:
;; - @Backtrace
;; - #PATH - directory/file
;; - @name - same to Org [[target]]
;;
;; We support Org ol.el package links:
;; - [[PATH]]
;;
;; We support "org-links" package new links: (TODO)
;; - [[PATH::NUM::LINE]]
;; - [[PATH::NUM-NUM::LINE]] - range
;; - [[PATH::NUM-NUM]] - range
;; - [[PATH::NUM]] creating
;;
;; To check links use "C-c ?" key, or M-x oai-expand-block.
;; `oai-block-tags--get-org-content' - extract target from current position

;; *Position and line number*
;; - `line-number-at-pos'
;; - `oai-block-tags--position-at-line-beginning'
;;
;;; Code
;;; -=-= variables
(require 'org)
(require 'ol)
(require 'oai-debug)

;; (defvar oai-block-tags--regexes '(
;;                                ;; :backtrace "@Backtrace`?\\([^a-zA-Z]\\|$\\)"
;;                                ;; :backtrace "\\(`?@Backtrace`?\\)\\([^a-zA-Z\"']\\|$\\)"
;;                                :backtrace "\\(`?@\\(Backtrace\\|Bt\\)`?\\)\\([^a-zA-Z\"']\\|$\\)"
;;                                :path "`?@\\(\\.\\.?/\\|\\.\\.?\\\\\\|\\.\\.?\\|/\\|\\\\\\|[A-Za-z]:\\\\\\)[a-zA-Z0-9_./\\\\-]*`?"
;;                                           ))
(defvar oai-block-tags--regexes-backtrace "\\(`?@\\(Backtrace\\|Bt\\)`?\\)\\([^a-zA-Z\"']\\|$\\)")
(defvar oai-block-tags--regexes-path "`?@\\(\\.\\.?/\\|\\.\\.?\\\\\\|\\.\\.?\\|/\\|\\\\\\|[A-Za-z]:\\\\\\)[a-zA-Z0-9_./\\\\-]*`?")

(defvar oai-block-tags--markdown-prefixes '(:backtrace "```elisp-backtrace"
                                         :path-directory "```ls-output"
                                         :path-file  "```"))
(defvar oai-block-tags--markdown-postfix "\n```\n")

(defvar oai-block-tags--backtrace-max-lines 12
  "Max lines to get from Backtrace buffer from begining.
All lines are rarely required, first 4-8 are most imortant.")

(defvar oai-block-tags-use-simple-directory-content nil
  "If non-nil use `directory-files' with simple list of item.
Otherwise ls command used.  Also `directory-files-and-attributes' may be
used.")

(defvar oai-block-tags-error-on-missing-link t
  "If non-nil signal error for not found link.
Used to set `org-link-search-must-match-exact-headline' before
`org-link-search' function call.")

(defvar oai-block-tags--check-double-targets-found t
  "Signal error if link in ai block point to targets in same file.")

(cl-assert
  (equal (mapcar (lambda (s)
                   (when (string-match oai-block-tags--regexes-path s)
                     (substring s (match-beginning 0) (match-end 0))))
                 '("@/file-s_s"
                   "@/file.t_xt"
                   "@./file.txt"
                   "@/some/path/file.txt"
                   "@C:\\some\\file.txt"
                   "@L:\\folder\\file.txt"
                   "@\\network\\share"
                   "@.\\windowsfile"
                   "@/file/"
                   "@/file.txt/"
                   "@./file.txt/"
                   "@/some/path/file.txt/"
                   "@C:\\some\\file.txt\\"
                   "@L:\\folder\\file.txt\\"
                   "@\\network\\share\\"
                   "@.\\windowsfile\\"
                   "@Backtrace"
                   "@not/a/path"
                   "@Backtrace"
                   "@not/a/path"
                   "@not/a/path/"
                   "@../right"
                   "@../right/"
                   "@.."
                   "@."
                   "@/"))
         '("@/file-s_s" "@/file.t_xt" "@./file.txt" "@/some/path/file.txt" "@C:\\some\\file.txt" "@L:\\folder\\file.txt" "@\\network\\share" "@.\\windowsfile" "@/file/" "@/file.txt/" "@./file.txt/" "@/some/path/file.txt/" "@C:\\some\\file.txt\\" "@L:\\folder\\file.txt\\" "@\\network\\share\\" "@.\\windowsfile\\" nil nil nil nil nil "@../right" "@../right/" "@.." "@." "@/")))

(defvar oai-block-tags-org-blocks-types '(comment-block center-block dynamic-block example-block
                                                        export-block quote-block special-block
                                                        src-block verse-block))
;;; -=-= @Backtrace

(defun oai-block-tags--take-n-lines (string n)
  "Return a string with the first N lines from STRING.
If N exceeds the number of lines, return all lines. If N <= 0, return an empty string."
  (let* ((lines (split-string string "\n"))
         (lines-to-keep (cl-subseq lines 0 (min (max 0 n) (length lines)))))
    (mapconcat #'identity lines-to-keep "\n")))

;; (oai-block-tags--take-n-lines "a\nb\nc\nd" 2)
;; (oai-block-tags--take-n-lines "a\nb\nc\nd" 4)
;; (oai-block-tags--take-n-lines "a\nb\nc" 10)
;; (oai-block-tags--take-n-lines "a\nb\nc" 0) ;; ""
;; (oai-block-tags--take-n-lines "a\nb\nc" -3) ;; ""
;; (oai-block-tags--take-n-lines "" 4) ;; ""
;; (oai-block-tags--take-n-lines "x\ny\nz\n" 2)
;; (oai-block-tags--take-n-lines nil 2) ;; error


(defun oai-block-tags--get-backtrace-buffer-string ()
  "Return the contents of the *Backtrace* buffer as a string, or nil if it does not exist."
  (let ((buf (get-buffer "*Backtrace*")))
    (when buf
      (with-current-buffer buf
        (string-trim (substring-no-properties (buffer-string)))))))

;;; -=-= Links: Files & Directories

(defun oai-block-tags--get-directory-content (path-string)
  (if oai-block-tags-use-simple-directory-content
      (concat (apply #'mapconcat #'identity (directory-files path-string)  '("\n")))
    ;; else
    (let ((buf (dired-noselect path-string)))
      (unwind-protect
          (with-current-buffer buf
            (buffer-substring-no-properties (point-min) (point-max)))
        (kill-buffer buf)))))

(defun oai-block-tags--read-file-to-string-safe (path-string &optional coding)
  ;; check
  (when (not (and (file-exists-p path-string)
                  (file-regular-p path-string)
                  (file-readable-p path-string)))
           (user-error "File does not exist or not readable: %s" path-string))
  ;; read
  (condition-case err
      (with-temp-buffer
        (when coding
          (set-buffer-file-coding-system coding))
        (insert-file-contents path-string)
        (buffer-string))
    (error (message "Error reading file %s: %s" path-string err)
           nil)))

;; (oai-block-tags--read-file-to-string-safe

(defun oai-block-tags--filepath-to-language (path-string)
  "For path-string return Org babel source block language name."
  (let* ((mode-symbol (assoc-default path-string auto-mode-alist 'string-match))
        (mode-string (apply #'mapconcat #'identity (butlast (string-split (symbol-name mode-symbol) "-")) '("-"))))
    (car (rassq (intern mode-string) org-src-lang-modes))))

(cl-assert
 (string-equal (oai-block-tags--filepath-to-language "/tmp/a.el") "elisp"))

(defun oai-block-tags--compose-block-for-path (path-string content)
  "Return file/directory content in mardown block without last ```."
  (concat
   "\nHere " (file-name-nondirectory (directory-file-name path-string)) (if (file-directory-p path-string) " folder" "") ":\n"
   ;; prefix
   (if (file-directory-p path-string)
       (plist-get oai-block-tags--markdown-prefixes :path-directory)
     ;; else - not derectory
     (let* ((mode-symbol (assoc-default path-string auto-mode-alist 'string-match))
            (mode (if mode-symbol
                      (progn
                        (car (rassq mode-symbol org-src-lang-modes)) ; string to symbol, get string
                        (apply #'mapconcat #'identity (butlast (string-split (symbol-name mode-symbol) "-")) '("-"))) ;; emacs-elisp from emacs-elisp
                    ;; else
                    "")))
     (concat (plist-get oai-block-tags--markdown-prefixes :path-file) mode)))
   "\n"
   content
   "\n```\n"))

(cl-assert
 (string-equal (oai-block-tags--compose-block-for-path "a.el" "ss")
"\nHere a.el:
```emacs-lisp
ss
```\n"))

(defun oai-block-tags--compose-block-for-path-full (path-string)
  "Return file or directory in prepared mardown block."
  (oai-block-tags--compose-block-for-path path-string
                                          (if (file-directory-p path-string)
                                              (oai-block-tags--get-directory-content path-string)
                                            ;; else
                                            (oai-block-tags--read-file-to-string-safe path-string))))

;;; -=-= help functions: get content
(defun oai-block-tags--get-org-block-element ()
  "Return Org block element at current position in current buffer.
Same logic as in `oai-block-tags--get-org-block-region'."
  (when-let* ((element
               (cl-loop with context = (org-element-context)
                        while (and context
                                   (not (member (org-element-type context) oai-block-tags-org-blocks-types)))
                        do (setq context (org-element-property :parent context))
                        finally return context)))
    element))


(defun oai-block-tags--get-org-content-m-block ()
  "Return markdown block for LLM for current element at current position.
Move pointer to the end of block.
Steps: find max, min region of special-block/src-block/buffer
`org-babel-read-element' from ob-core.el"
  ;; (org-element-property :name (oai-block-p))
  ;; 1) enshure that we are inside some Org block
  (when-let* ((element (oai-block-tags--get-org-block-element))
              (region (oai-block-tags--get-org-block-region element))
              (beg (car region))
              (end (cadr region)))

    ;; Compose result block
    ;; (goto-char end) ; for return
    (print "oai-block-tags--get-org-content-m-block")
    (concat
     ;; - 0 - Name
     (when-let ((name (org-element-property :name element))) ; nil or string
       (concat "\nBlock name: " name))
     ;; - 1 - Header ```
     (if (eq (org-element-type element) 'src-block)
         (concat "\n```"  (org-element-property :language element) "\n")
       ;; else
       "\n```text\n")
     ;; - 2 - Body
     (string-trim (buffer-substring-no-properties beg end))
     ;; - 3 - Footer ```
     "\n```\n")
    ))

(defun oai-block-tags--get-m-block ()
  "Called for current point.
Return non-nil string of markdown block if exist at current position."
  (if-let ((range (oai-block-tags--markdown-block-range)))
      ;; else
      (oai-block-tags--position-in-markdown-block-str-p (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                                                        (- (point) (line-beginning-position)))))

;;; -=-= help function: line number for position

(defun oai-block-tags--position-at-line-beginning (line-num &optional end-flag buffer)
  "Return the buffer position at the beginning of LINE-NUM in BUFFER or nil.
LINE-NUM is 1-based. If BUFFER is nil, use the current buffer.
If END-FLAG is non-nil, then return end of line position.
Returns nil if LINE-NUM is out of range."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (when (zerop (forward-line (1- line-num)))
        (if end-flag
          (line-end-position)
          ;; else
          (point))))))
;; - test:
;; (print (list (line-beginning-position) (oai-block-tags--position-at-line-beginning (line-number-at-pos (point)))))

;;; -=-= help functions: find targets of Links
(defun oai-block-tags--get-org-block-region (&optional element)
  "Return (beg end) pair for any Org block ELEMENT or nil.
Works for ai block also."
  (when-let* ((element
               (or element
                   (cl-loop with context = (org-element-context)
                            while (and context
                                       (not (member (org-element-type context) oai-block-tags-org-blocks-types)))
                            do (setq context (org-element-property :parent context))
                            finally return context))))
    (let ((beg (or (org-element-property :contents-begin element)
                   (org-element-property :begin element)))
          (end (or (org-element-property :contents-end element)
                   (org-element-property :end element))))
      (when (and beg end)
        ;; - skip headers if begin at header
        (save-excursion (goto-char beg)
                        (when (or (looking-at "#\\+begin_")
                                  (search-forward "#+begin_" end t))
                          (forward-line)
                          (setq beg (point)))
                        (goto-char end)
                        (when (or (looking-at "#\\+end_")
                                  (search-backward "#+end_" beg t))
                          (forward-line -1)
                          (setq end (line-end-position)))))
      (list beg end))))


(defun oai-block-tags--path-is-current-buffer-p (path)
  "Return non-nil if PATH references the file currently visited by this buffer.
Handles symlinks, remote files (TRAMP), and buffers without files."
  (when buffer-file-name
    (ignore-errors
      (let ((buffer-file (file-truename buffer-file-name))
            (input-file  (file-truename (expand-file-name path))))
        (string= buffer-file input-file)))))


(defun oai-block-tags--get-org-content ()
  "Return prepared block at current position.
If at current position there is a Org block or markdown block
Return markdown block for LLM for current element at current position.
May return nil.
For Org buffer only.
Supported: blocks and headers.
- Org header - loop over elements and convert to markdown
- at markdown block header or inside markdown block
- at src header or inside src block
Move pointer to the end of block."
  (oai--debug "oai-block-tags--get-org-content")
  (let* ((element (org-element-context)) ; should not be changed
         (type (org-element-type element)))
    (oai--debug "oai-block-tags--get-org-content type %s" type)
    ;; - (1) case - headline
    (cond
     ((eq type 'headline)
      (let ((replacement-list)
             ;; (list "**" (org-element-property :title element) "**" "\n")) ; prepare header title
            el ; current element in loop
            type ; type of current element in loop
            )
        ;; Loop over headlines, to process every blocks and org elements to markdown for LLM
        (while (< (point) (org-element-property :end element))
          ;; supported sub-elements: headline, blocks
          (setq el (org-element-context))
          (setq type (org-element-type el))
          (push (cond
                 ;; 1. Sub: Headline
                 ((eq type 'headline)
                  ;; make string: #*level + title
                  (prog1 (concat (make-string (org-element-property :level el) ?#) " " (org-element-property :raw-value el) "\n")
                    (forward-line))) ; MOVE!
                 ;; 1. Sub: Block
                 ((member type  oai-block-tags-org-blocks-types)
                  (prog1 (oai-block-tags--get-org-content-m-block el)
                    ;; (condition-case nil
                    (org-forward-element) ; MOVE!
                    ;; (org-next-item)
                    ;; (error nil))
                    ))
                 (t ; others
                  (prog1
                      (concat (buffer-substring-no-properties (line-beginning-position) (org-element-property :end el) ) "\n")
                    ;; (condition-case nil
                    (org-forward-element)
                    ;; (org-next-item)
                    ;; (error nil))
                    ))) replacement-list)
          ) ; while
        (print (list "!!!!!!!!!" (reverse replacement-list)))
        (apply 'concat (reverse replacement-list))
        ))
     ;; - (2) case - Markdown block - TODO
     ((oai-block-tags--get-m-block))
     ;; (oai-block-tags--markdown-block-range
     ;; - (3) case -  Org block (or ai block)
     (t
      (oai-block-tags--get-org-content-m-block)))))

(when (featurep 'org-links)
    (defun oai-block-tags--get-org-links-content (link)
      "In current buffer get content for LINK.
Support for `org-links' package with additional links types.
Headlines not wrapped in markdown blocks.
LINK is string in format is what inside [[...]] or Plain link."
      (require 'org-links)
      (oai--debug "oai-block-tags--get-org-links-content1 %s %s" link (string-match org-links-num-num-regexp link))
      (if-let ((nums (org-links--local-get-target-position-for-link link)))
          (let ((num1 (car nums))
                (num2 (cadr nums))) ; may be nil
            (oai--debug "oai-block-tags--get-org-links-content2 %s %s" num1 num2)
            ;; 1) Case1: num1 and num2 - get range
            (if num2
              (if-let ((pos1 (oai-block-tags--position-at-line-beginning num1))
                       (pos2 (or (oai-block-tags--position-at-line-beginning num2 'end-of-line) (point-max))))
                  (progn (oai--debug "oai-block-tags--get-org-links-content3 %s %s" pos1 pos2)
                  (concat "```auto\n"
                          (string-replace "```" "\\`\\`\\`"
                                          (buffer-substring-no-properties
                                           pos1
                                           pos2))
                          "\n```"))
                ;; pos1 is nil
                (user-error "In link %s of NUM-NUM format was not possible to find first NUM in buffer %s" link (current-buffer)))
              ;; else - ;; 1) Case1: only num1, num2 is nil - get object at num1 or just line.
              (if-let ((pos1 (oai-block-tags--position-at-line-beginning num1)))
                  (save-excursion
                    (oai--debug "oai-block-tags--get-org-links-content4 %s" pos1)
                    (goto-char pos1)
                    (oai-block-tags--get-org-content))
                (user-error "In link %s of NUM format was not possible to find first position in buffer %s" link (current-buffer)))
            ))
        ;; else - not org-links type link.
        nil)))

;; (oai-block-tags--get-org-links-content "9-10")

(defun oai-block-tags--org-search-local (link type path)
  "Return type
  of matched result, which is either `dedicated' or `fuzzy'."
  (oai--debug "oai-block-tags--org-search-local %s %s %s" link type path)
  (if (equal type "radio")
      (org-link--search-radio-target path)
    ;; else - fuzzy, custom-di, coderef
    (let ((org-link-search-must-match-exact-headline oai-block-tags-error-on-missing-link)) ;; should found?
      (print (list "oai-block-tags--org-search-local" org-link-search-must-match-exact-headline))
      ;; Not working: :-(
      ;; (save-excursion
      ;;   (with-restriction (point-min) (point-max)
      (org-link-search
       (pcase type
	 ("custom-id" (concat "#" path))
	 ("coderef" (format "(%s)" path))
	 (_ path))
       ;; Prevent fuzzy links from matching themselves.
       (and (equal type "fuzzy")
	    (+ 2 (org-element-property :begin link)))))))

(defun oai-block-tags--get-replacement-for-org-link (link-string)
  "Return string for LLM for LINK-STRING string or nil.
Supported targets:
- Org block in current buffer \"file:\"
- file & directory `oai-block-tags--compose-block-for-path-full'
- local link.
Use current buffer, current position to output error to result of block if two targets found."
  ;; `org-link-open' for type and opening,  `org-link-search' for search in current buffer.

  ;; from `org-link-open-from-string'
  ;; - - 1) convert string to Org element
  (let ((link-el (with-temp-buffer
                   (let ((org-inhibit-startup nil))
                     (insert link-string)
                     (org-mode)
                     (goto-char (point-min))
                     (org-element-link-parser)))))
    (if (not link-el)
      (user-error "No valid link in %S" link-string))
    ;; from `org-link-open'
    ;; - - 2) extract path and type
    (let ((type (org-element-property :type link-el))
          (path (org-element-property :path link-el)))
      (print (list "type?" type path link-el))
      ;; - - 3) process link depending on type
      (pcase type
        ("file" ; org-link-search
         (let* ((option (org-element-property :search-option link-el))) ;; nil if no ::, may be "" if after :: there is empty last part
           ;; (print (list "option" option))
           ;; (print (list "check" (oai-block-tags--path-references-current-buffer-p path)))

           (if (and option
                    (not (string-empty-p option)))
               ;; case 1) path to current file with option
               (if (oai-block-tags--path-is-current-buffer-p path)
                   ;; recursion without path, to find target in current buffer
                   (oai-block-tags--get-replacement-for-org-link (concat "[[" option "]]")) ; recursive call
                 ;; - else  case 2) path to other file with option
                 (user-error "Links to targets in other files not supported for now")  ;; TODO
                 )
             ;; else - no ::, only path
             (oai-block-tags--compose-block-for-path-full path))
           ))

        ;; LOCAL LINKS!
        ;; ((or "coderef" "custom-id" "fuzzy" "radio")
        ((or "radio" "fuzzy")
         (save-excursion
           (org-with-wide-buffer
            (let ((ln-before (line-number-at-pos))
                   ;; - 1) find target of link-el & link-string
                  (found (oai-block-tags--org-search-local link-el type path))  ; <- Search!
                  target-pos)
              ;; - 2) move pointer to search result
              (setq target-pos (point))
              (print (list "oai-block-tags--get-replacement-for-org-link found" found (point)))
              ;; 2.1) several targets with same name exist? = error
              (when (and oai-block-tags--check-double-targets-found
                         (eq found 'dedicated)
                         (not (eq (line-number-at-pos) ln-before))) ; found?
                (let ((ln-found (line-number-at-pos))
                      oai-block-tags-error-on-missing-link)
                  (with-restriction (line-end-position) (point-max)
                    (condition-case nil
                        (setq found (oai-block-tags--org-search-local link-el type path))
                      (error nil)))
                  (when (and (not (eq (line-number-at-pos) ln-found)) ; found?
                             (eq found 'dedicated))
                    ;; (print (list (line-number-at-pos) ln-found (progn (forward-line ln-found)
                    ;;                                                   (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                    (user-error "Two targets found for link %s\n- %s: %s\n- %s: %s" link-string
                                (line-number-at-pos) (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                                ln-found (progn (forward-line (- ln-found (line-number-at-pos)))
                                                (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                    )))

              ;; - 4) Move to position of target - if we are here: No second target was found
              (print (list "oai-block-tags--get-replacement-for-org-link found2" (point) target-pos type link-string)) ; `(ref)'
              (goto-char target-pos)
              ;; - 5) Get content for link - org-links or ol.el link
              (print (list "vvvs" (require 'org-links nil 'noerror)  (string-equal type "fuzzy")))
              (if (and (require 'org-links nil 'noerror)
                       (string-equal type "fuzzy"))
                  ;; Type: org-links
                  (progn
                    (oai--debug "oai-block-tags--get-replacement-for-org-link %s" (org-element-property :raw-link link-el))
                    ;; :raw-link - "1::* headline"
                    (or (oai-block-tags--get-org-links-content (org-element-property :raw-link link-el)) ; NUM-NUM
                        ;; else el.el
                        (oai-block-tags--get-org-content)))
                ;; else Type: el.el
                 (oai-block-tags--get-org-content))))))))))
;; - test:
;; (if (not (let ((oai-block-tags-use-simple-directory-content t))
;;            (and
;;             (string-match "oai-block-tags.el" (oai-block-tags--get-replacement-for-org-link "file:./"))
;;             (string-match "oai-block-tags.el" (oai-block-tags--get-replacement-for-org-link "[[./]]"))
;;             (string-match "oai-block-tags.el" (oai-block-tags--get-replacement-for-org-link "[[file:./]]"))
;;             (string-match "oai-block-tags.el" (oai-block-tags--get-replacement-for-org-link "[[file:.]]"))
;;             )))
;;     (error "oai-block-tags-use-simple-directory-content dir ./"))

;; [[file:oai-block-tags.el::`(ref)']]]
;; (oai-block-tags--get-replacement-for-org-link  "[[xx]]")

;;; -=-= help functions: markdown blocks

(defvar oai-block--markdown-begin-re "^[\s-]*```\\([^ \t\n[{]+\\)[\s-]?\n")
(defvar oai-block--markdown-end-re "^[\s-]*```[\s-]?$")


(defun oai-block-tags--markdown-fenced-code-body-get-range (&optional limit-begin limit-end)
  "Return (begin end) if point is inside a Markdown fenced block.
Ensures there are no fences between block begin and point."
  (save-excursion
    (let ((point-pos (point))
          begin end)
      ;; Find nearest header backward
      (when (or (when (looking-at oai-block--markdown-begin-re nil)
                  (forward-line)
                  (setq point-pos (point)))
                (re-search-backward oai-block--markdown-begin-re (or limit-begin (point-min)) t))
        (setq begin (match-end 0))
        ;; Check there is no block footer between begin and point
        (let ((inter-fence-pos nil))
          (save-excursion
            (when (re-search-forward oai-block--markdown-end-re point-pos t)
              (setq inter-fence-pos t)))
          (unless inter-fence-pos
            ;; From header, find next closing fence
            (goto-char begin)
            (when (re-search-forward oai-block--markdown-end-re (or limit-end (point-max)) t)
              (setq end (match-beginning 0))
              (when (and (>= point-pos begin) (< point-pos end))
                (list begin end)))))))))


(defun oai-block-tags--markdown-mark-fenced-code-body (&optional limit-begin limit-end)
  "Mark content inside Markdown fenced code block (```), excluding header/footer.
LIMIT-BEGIN and LIMIT-END restrict the search region around point.
Returns t if was marked, nil otherwise.
Used in `oai-block-tags-mark-md-block-body'."
  ;; fill limit-begin and limit-end - if they was not profiled
  (if (or (not limit-begin) (not limit-end))
      (let ((element (oai-block-p)))
        (setq limit-begin (org-element-property :contents-begin element))
        (setq limit-end (org-element-property :contents-end element))))

  (when-let* ((r (oai-block-tags--markdown-fenced-code-body-get-range limit-begin limit-end))
              (beg (car r))
              (end (cadr r)))
    (set-mark beg)
    (goto-char end)
    (forward-line -1)
    (end-of-line)
    (activate-mark)
    t))

;; (defun oai-block-mark-src-block-body ()
;;   "Mark Org blocks content around cursor.
;; Excluding header and footer."
;;   (interactive)
;;   (let ((elem (org-element-at-point)))
;;     (goto-char (org-element-property :begin elem))
;;     (forward-line 1)
;;     (set-mark (point))
;;     (let ((case-fold-search t))
;;           (re-search-forward "#\\+end_" nil t))
;;     (beginning-of-line)
;;     ;; (goto-char (org-element-property :end elem))
;;     ;; (forward-line -2)
;;     ;; (end-of-line)
;;     (activate-mark)
;;     t))

;; (defun oai-block-tags--in-markdown-fences-p ()
;;   (let* ((element (oai-block-p))
;;          (limit-begin (org-element-property :contents-begin element))
;;          (limit-end (org-element-property :contents-end element)))
;;     (oai-block-tags--markdown-mark-fenced-code-body-get-range limit-begin limit-end)))

(defun oai-block-tags--position-in-markdown-block-str-p (str pos) ; TODO: rewrite as `oai-block-tags--markdown-fenced-code-body-get-range' to return range or implement new.
  "Return non-nil if POS (an index) is inside a '```' code block in STR."
  (let ((search-pos 0)
        (block-boundaries '()))
    ;; Find all the '```' positions
    (while (string-match "```" str search-pos)
      (push (match-beginning 0) block-boundaries)
      (setq search-pos (match-end 0)))
    ;; Sort and pair boundaries
    (setq block-boundaries (sort block-boundaries #'<))
    (catch 'inside
      (let ((bounds block-boundaries))
        (while bounds
          (let ((start (pop bounds))
                (end (and bounds (pop bounds))))
            (when (and end (>= pos start) (< pos end))
              (throw 'inside t)))))
      nil)))

(if (not (oai-block-tags--position-in-markdown-block-str-p "aaa```bbb```ccc" 5))   ;; => t   (inside first block)
    (error "oai-block-tags--position-in-markdown-block-str-p"))
(if (oai-block-tags--position-in-markdown-block-str-p "aaa```bbb```ccc" 10)  ;; => nil (outside any block)
    (error "oai-block-tags--position-in-markdown-block-str-p"))

(defun oai-block-tags--markdown-block-range ()
  "Return range if current position in current buffer in markdown block."
  ;; check that we are in Org block
  (when-let* ((region (oai-block-tags--get-org-block-region))
              (beg (car region))
              (end (cadr region)))
    ;; (let ((search-pos 0)
    ;;       (block-boundaries '()))
          (oai-block-tags--markdown-fenced-code-body-get-range beg end)))
      ;; ;; Find all the '```' positions
      ;; (while (string-match "```" str search-pos)
      ;;   (push (match-beginning 0) block-boundaries)
      ;;   (setq search-pos (match-end 0)))
      ;; ;; Sort and pair boundaries
      ;; (setq block-boundaries (sort block-boundaries #'<))
      ;; (catch 'inside
      ;;   (let ((bounds block-boundaries))
      ;;     (while bounds
      ;;       (let ((start (pop bounds))
      ;;             (end (and bounds (pop bounds))))
      ;;         (when (and end (>= pos start) (< pos end))
      ;;           (throw 'inside t)))))
      ;;   nil))))

;;; -=-= Replace links in text
;; Supported:
;; - @Backtrace
;; - @/path/file.txt
;; - @./name - file
;; - @name - <<target>> or #+NAME: name - in current file

(defun oai-block-tags--replace-last-regex-smart (string regexp &optional replacement subexp)
  "Replace the last match of REGEXP in STRING with REPLACEMENT,
preserving any extra captured groups.
Check that found regexp not in markdown block.
If REPLACEMENT not provided return found string for regexp."
  (let ((pos 0)
        (last-pos nil)
        (last-end nil)
        (last-group ""))
    (while (and pos
                (string-match regexp string pos)
                (not (oai-block-tags--position-in-markdown-block-str-p string (setq pos (match-beginning 0))))) ; not in markdonw block
      ;; (print (list "vv" (oai-block-tags--position-in-markdown-block-str-p string (setq pos (match-beginning 0)))))
      (setq last-pos pos) ; beg

      (setq last-end (match-end 0)) ; end
      ;; (print (list "last-group"   (match-string 0 string) (match-string 1 string) (match-string 2 string)))
      (setq pos last-end) ; move forward
      )
    (if last-pos
        (if replacement
            ;; (replace-match replacement 'fixedcase 'literal string)
            ;; (if (eq (aref string (1- last-end)) ?\s) ;; if space after match
                ;; (replace-match replacement 'fixedcase 'literal string 1)
              ;; else
              (concat (substring string 0 last-pos)
                      replacement
                      ;; last-group
                      (substring string last-end))
          ;; else - just return 0 group
          ;; remove leading and ending (` and space) characters
           (replace-regexp-in-string "^[` ]*" ""
                                     (replace-regexp-in-string "[` ]*\$" ""
                                                               (match-string 0 string))) ;; (substring string last-pos last-end)
          ) ; what was found
      string)))

(cl-assert
 (string-equal (oai-block-tags--replace-last-regex-smart "asdasd@Backtraceasdasdasd" "\\(@Backtrace\\)" "111")
        "asdasd111asdasdasd"))

(cl-assert
 (string-equal (oai-block-tags--replace-last-regex-smart "asda\n```\nvas@Backtraceasdasd\n```\nasd" "\\(@Backtrace\\)" "111")
               "asda\n```\nvas@Backtraceasdasd\n```\nasd"))

(cl-assert
 (equal (oai-block-tags--replace-last-regex-smart "asdasd@Backtraceasdasdasd" "@Backtrace")
        "@Backtrace"))

;; search without replace
(cl-assert
 (and
  (equal (let ((regex oai-block-tags--regexes-backtrace))
           (oai-block-tags--replace-last-regex-smart
            "foo `@Backtrace` bar `@Backtrace `@BacktraceX"
            regex)) "@Backtrace")

  (equal (let ((regex oai-block-tags--regexes-backtrace))
           (oai-block-tags--replace-last-regex-smart
            "foo `@Backtrace` bar `@Backtrace `@Backtrace`X"
            regex)) "@Backtrace")

  (equal (let ((regex oai-block-tags--regexes-backtrace))
           (oai-block-tags--replace-last-regex-smart
            "foo `@Backtrace` bar `@Backtrace `@Bt`X"
            regex)) "@Bt")
  ))

(cl-assert
 (equal (let ((regex oai-block-tags--regexes-backtrace))
          (oai-block-tags--replace-last-regex-smart
           "foo `@Backtrace` bar `@Backtrace `@Backtrace`X"
           regex
           "REPLACED"))
        "foo `@Backtrace` bar `@Backtrace REPLACEDX"))
;; with space
(cl-assert
 (equal (let ((regex oai-block-tags--regexes-backtrace))
          (oai-block-tags--replace-last-regex-smart
           "foo `@Backtrace` bar `@Backtrace `@BacktraceX"
           regex
           "REPLACED"))
        "foo `@Backtrace` bar REPLACED`@BacktraceX"))

(cl-assert
 (equal (let ((regex oai-block-tags--regexes-backtrace))
          (oai-block-tags--replace-last-regex-smart
           "foo `@Backtrace` bar `@Bt `@BacktraceX"
           regex
           "REPLACED"))
        "foo `@Backtrace` bar REPLACED`@BacktraceX"))

(cl-assert
 (equal (oai-block-tags--replace-last-regex-smart
         "foo `@/asd.txt` X"
         oai-block-tags--regexes-path
         "REPLACED")
        "foo REPLACED X"))

(cl-assert
 (equal (let ((regex oai-block-tags--regexes-backtrace))
          (oai-block-tags--replace-last-regex-smart "foo `@.` bar " oai-block-tags--regexes-path "REPLACED"))
        "foo REPLACED bar "))

(cl-assert
 (string-equal
  (oai-block-tags--replace-last-regex-smart "asd `@/tmp/t.txt` assd" oai-block-tags--regexes-path "path")
  "asd path assd"))
 ;; (oai-block-tags--replace-last-regex-smart "asd `[[/tmp][sd]]` assd" (plist-get oai-block-tags--regexes :path) "path")

(defun oai-block-tags-replace (string)
  "Replace links in STRING with their targets.
And return modified string or the same string."
  (print (list "vv" string))
  ;; - "@Backtrace" substring exist - replace the last one only
  ;; *Wrap in markdown*
  (when (string-match oai-block-tags--regexes-backtrace string)
    (if-let* ((bt (oai-block-tags--get-backtrace-buffer-string)) ; *Backtrace* buffer exist
              (bt (oai-block-tags--take-n-lines bt oai-block-tags--backtrace-max-lines))
              (bt (concat "\n" (plist-get oai-block-tags--markdown-prefixes :backtrace) "\n"
                          bt
                          oai-block-tags--markdown-postfix)) ; prepare string
              (new-string (oai-block-tags--replace-last-regex-smart string oai-block-tags--regexes-backtrace bt))) ; insert backtrace
        (setq string new-string)))

  ;; - Path @/path/file.txt - replace the last one only
  ;; *Wrap in markdown*
  (when (string-match oai-block-tags--regexes-path string)
    (if-let* ((path-string (oai-block-tags--replace-last-regex-smart string oai-block-tags--regexes-path)) ; find the last
              ;; remove first @ character from link
              (path-string (if (> (length path-string) 0)
                               (substring path-string 1)
                             ""))
              (replacement (oai-block-tags--compose-block-for-path-full path-string))
              (new-string (oai-block-tags--replace-last-regex-smart string
                                                                    oai-block-tags--regexes-path
                                                                    replacement)))
        (setq string new-string)))

  ;; - Org links [[link]]
  ;; We search  for link regex,  when found we check  if there
  ;; are double of  found substring after founded  one, if one
  ;; more exist we skip the first  one that found. if no other
  ;; exist we replace it.
  ;; *Dont Wrap in markdown*
  (when (string-match org-link-any-re string) ; exist in text?
    (if-let* ((link (oai-block-tags--replace-last-regex-smart string org-link-any-re)) ; find the last

              (replacement (concat "\n" (oai-block-tags--get-replacement-for-org-link link) "\n" )) ; add empty line after it.
              (new-string (oai-block-tags--replace-last-regex-smart string
                                                                    org-link-any-re
                                                                    replacement)))
        (setq string new-string)))
  ;; old
  ;; (when (not (string-match org-link-any-re string)) ; exist in text?
  ;;   (let ((new-string string) ; ai block content
  ;;         (path-string (oai-block-tags--replace-last-regex-smart string org-link-any-re))
  ;;         (replaced nil)
  ;;         (pos-end 0)
  ;;         (pos-beg 0)
  ;;         (link)
  ;;         (replacement))
  ;;     (print (list "oai-block-tags-replace 0" path-string))
  ;;     ;; 1) find link
  ;;     (while (or (= pos-end 0) ; skip first
  ;;                (string-match org-link-any-re new-string pos-end)) ; loop over links in text
  ;;       (print "found link")
  ;;       (setq pos-beg (match-beginning 0))
  ;;       (setq pos-end (match-end 0))
  ;;       (setq link (match-string 0 new-string))
  ;;       (print (list "oai-block-tags-replace 1" (oai-block-tags--position-in-markdown-block-str-p new-string pos-beg)))
  ;;       (when (not (oai-block-tags--position-in-markdown-block-str-p new-string pos-beg)) ; skip links inside markdown
  ;;         (print (list "oai-block-tags-replace 2"))


  ;;         ;; check that there is no clone of this below, hence we replace the last one.
  ;;         (print (list  (regexp-quote link) new-string pos-end (string-match (regexp-quote link) new-string pos-end)))
  ;;         (when (not (and (string-match (regexp-quote link) new-string pos-end)
  ;;                         (not (oai-block-tags--position-in-markdown-block-str-p new-string pos-beg))))
  ;;           (print (list "oai-block-tags-replace 3" link))
  ;;           ;; 2) get target for link
  ;;           (setq replacement (oai-block-tags--get-replacement-for-org-link link))
  ;;           (print (list "oai-block-tags-replace 4 replacement" replacement))
  ;;           (when replacement
  ;;             (setq new-string (concat (substring new-string 0 pos-beg)
  ;;                                      replacement
  ;;                                      ;; last-group
  ;;                                      (substring new-string pos-end)))
  ;;             (setq pos-end (+ pos-beg (length replacement)))
  ;;             ;; (print (list pos-end new-string))
  ;;             (setq replaced t)
  ;;             ;; (oai-block-tags--replace-last-regex-smart)
  ;;             ))))
  ;;     (when replaced
  ;;       (setq string new-string))))


  ;; (if-let* ((path-string (oai-block-tags--replace-last-regex-smart string org-link-any-re)))
  ;;     (progn
  ;;       (print (list "link" path-string))
  ;;       string))
  ;; - default
  string)

;; (oai-block-tags-replace  "[[./]]")
;; (oai-block-tags-replace  "11[[sas]]222[[bbbaa]]3333[[sas]]4444")
;; (oai-block-tags-replace  "11[[file:/mock/org.org::1::* headline]]4444")
  ;; [[file:/mock/org.org::1::* headline]]

;; (let* ((link (with-temp-buffer
;;                (let ((org-inhibit-startup nil))
;;                  (insert "[[file:~/docsmy_short/modified/emacsh::*graphiz - graphs][graphiz - graphs]]")
;;                  (org-mode)
;;                  (goto-char (point-min))
;;                  (org-element-link-parser))))
;;            (type (org-element-property :type link))
;;            (path (org-element-property :path link))
;;            (follow (org-link-get-parameter type :follow))
;;            (option (org-element-property :search-option link))) ;; after ::
;;       (print (list type path option follow)))

;; - just output test, too hard to compare with something.
(let* ((temp-dir (make-temp-file "my-tmp-dir-" t))     ;; Create temp directory
       (file1 (expand-file-name "file1.txt" temp-dir)) ;; Known file name
       (file2 (expand-file-name "file2.el" temp-dir))
       (file3 (expand-file-name "file3.py" temp-dir))
       )

      (with-temp-file file1
        (insert "Contents for file1"))
      (with-temp-file file2
        (insert "(defun aa() )"))
      (with-temp-file file3
        (insert "import os"))
      (print (oai-block-tags-replace (format "ssvv `@%s` bbb" temp-dir)))
      (if (not (string-match (regexp-quote "```text") (print (oai-block-tags-replace (format "ssvv `@%s` bbb" file1)))))
          (error "ss"))
      ;; (print (oai-block-tags-replace (format "ssvv `@%s` bbb" file2))))
      (if (not (string-match (regexp-quote "```emacs-lisp") (print (oai-block-tags-replace (format "ssvv `@%s` bbb" file2)))))
          (error "ss2"))
      (oai-block-tags-replace (format "ssvv [[%s]] bbb" file3)))

  ;; Return list of paths for later use
  ;; (list temp-dir file1 file2))

;; get folder content
(let ((path "/tmp/tttt1"))
                (if (not (file-exists-p path))
                    (make-directory path)
                  )
                ;; (temporary-file-directory
                (oai-block-tags-replace (format "ssvv `@%s` bbb" path)))
;; "ssvv
;; Here tttt1 folder:
;; ```ls-output
;;   /tmp/tttt1:

;; ```
;;  bbb")


;;; -=-= Fontify @Backtrace & @path & [[links]]

(defun oai-block-tags--font-lock-fontify-links (limit)
  "Fontify Org links in #+begin_ai ... #+end_ai blocks, up to LIMIT.
This is special fontify function, that return t when match found.
1) search for ai block begin and then end, 2) call fontify on range that goto to the begining firstly
`org-activate-links'."
  (if oai-block-fontify-markdown
      (let ((case-fold-search t)
            (ret))
        (while (and (re-search-forward "^#\\+begin_ai[^\n]*\n" limit t)
                    (< (point) limit))
          (let ((beg (match-end 0)))
            (when (re-search-forward "^#\\+end_ai.*$" nil t)
              (let ((end (match-beginning 0)))
                (save-match-data
                  ;; fontify Org links [[..]]
                  ;; (message beg)
                  ;; - [[link][]]
                  (progn
                    (goto-char beg)
                    (while (re-search-forward org-link-any-re end t)
                      (goto-char (match-beginning 0))
                      (setq ret (org-activate-links end))
                      ))
                  ;; - @Backtrace
                  (progn
                    (goto-char beg)
                    (while (re-search-forward oai-block-tags--regexes-backtrace limit t)
                      (add-face-text-property (match-beginning 0) (match-end 0) 'org-link)
                      (setq ret t)))
                  ;; - @/tmp/
                  (progn
                    (goto-char beg)
                    (while (re-search-forward oai-block-tags--regexes-path limit t)
                      (add-face-text-property (match-beginning 0) (match-end 0) 'org-link)
                      (setq ret t)))
                  ;; fontify markdown sub-blocks
                  ;; (oai-block--fontify-markdown-subblocks beg end)
                  )
                ))))
        ;; required by font lock mode:
        (goto-char limit)
        ret)))

;;; -=-= key to select block "C-c h" (similar to "M-h")

(defun oai-block-tags-mark-md-block-body ()
  "Mark content of Markdown code block, or fallback to org-mark-element.
Mark or select block content around cursor.

`oai-block-tags--get-org-block-region'  do  same thing,  but  we
make this function to no relay on oai-block."
  (interactive)
  (or (when-let* ((region (oai-block-tags--get-org-block-region))
                (beg (car region))
                (end (cadr region)))
          ;; Mardown in block
          (or (oai-block-tags--markdown-mark-fenced-code-body beg end)
              ;; else - no markdown - mark ai block only
              (progn
                (set-mark beg)
                (goto-char end)
                (forward-line -1)
                (end-of-line)
                (activate-mark)))
          t)
      (org-mark-element)))

(provide 'oai-block-tags)
;;; oai-block-tags.el ends here
