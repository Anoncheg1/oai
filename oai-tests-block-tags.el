;;; oai-tests-block-tags.el ---  -*- lexical-binding: t -*-
;; Copyright (c) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: tools, async, callback
;; URL: https://github.com/Anoncheg1/async1

;; (eval-buffer)
;; (ert t)

;;; License

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

;;; Code:

;;; - Help functions ------------------------------------------------

(require 'oai-block-tags)
(require 'ert)


;; (eval-buffer) or (load-file "path/to/async-tests.el")
;; Running Tests: Load the test file and run:
;; (eval-buffer)
;; (ert t)
;; to execute all tests. Individual tests can be run with (ert 'test-name).

;;; - Tests --------------------------------------------------------

(ert-deftest oai-block-tags--read-file-to-string-safe--read-ok ()
  "Should read a regular readable file and return its contents."
  (let ((tmpfile (make-temp-file "oai-test")))
    (unwind-protect
        (progn
          (write-region "Hello, test!" nil tmpfile)
          (should (equal (oai-block-tags--read-file-to-string-safe tmpfile)
                         "Hello, test!")))
      (delete-file tmpfile))))

(ert-deftest oai-block-tags--read-file-to-string-safe--file-missing ()
  "Should signal user-error if the file does not exist."
  (should-error (oai-block-tags--read-file-to-string-safe "/no/such/file")
                :type 'user-error))

(ert-deftest oai-block-tags--read-file-to-string-safe--nonregular ()
  "Should signal user-error if path is not a regular file."
  (let ((tmpdir (make-temp-file "oai-test-dir" t)))
    (unwind-protect
        (should-error (oai-block-tags--read-file-to-string-safe tmpdir)
                      :type 'user-error)
      (delete-directory tmpdir))))

(ert-deftest oai-block-tags--read-file-to-string-safe--unreadable ()
  "Should signal user-error if the file is not readable."
  (let ((tmpfile (make-temp-file "oai-test-unreadable")))
    (unwind-protect
        (progn
          (write-region "not readable" nil tmpfile)
          (set-file-modes tmpfile 0)
          (should-error (oai-block-tags--read-file-to-string-safe tmpfile)
                        :type 'user-error))
      ;; Restore permissions so we can delete it
      (set-file-modes tmpfile #o600)
      (delete-file tmpfile))))

(ert-deftest oai-block-tags--read-file-to-string-safe--with-coding ()
  "Should honor the coding argument; reading ASCII content as UTF-8 should work."
  (let ((tmpfile (make-temp-file "oai-test-coding")))
    (unwind-protect
        (progn
          (write-region "abc" nil tmpfile)
          (should (equal
                   (oai-block-tags--read-file-to-string-safe tmpfile 'utf-8)
                   "abc")))
      (delete-file tmpfile))))

;;; - Tests: oai-block-tags--markdown-fenced-code-body-get-range
(ert-deftest oai-test-block-markdown-mark-fenced-code-body-get-range1 ()
  "Test fenced code detection."
  (let ((payload "text before
```elisp
code block
line2
```
text after"))
    (with-temp-buffer
      (insert payload)
      ;; Move point to inside the code block
      (goto-char (point-min))
      (re-search-forward "code block")
      (beginning-of-line)
      (forward-line -1)
      (let* ((limit-begin (point-min))
             (limit-end (point-max))
             (range (oai-block-tags--markdown-fenced-code-body-get-range
                     limit-begin limit-end)))
        ;; (should
        (equal range (list 22 39)))))
  )

(ert-deftest oai-test-block-markdown-mark-fenced-code-body-get-range2 ()
  "Test fenced code detection."
(let ((payload "text before
```elisp
```
code block
line2
```
text after"))
    (with-temp-buffer
      (insert payload)
      ;; Move point to inside the code block
      (goto-char (point-min))
      (re-search-forward "code block")
      (let* ((limit-begin (point-min))
             (limit-end (point-max))
             (range (oai-block-tags--markdown-fenced-code-body-get-range
                     limit-begin limit-end)))
        (should
        (equal range nil)))))
)

;;; - Test: oai-block-tags--get-replacement-for-org-link - dir
(ert-deftest oai-test-oai-block-tags--get-replacement-for-org-link-dir ()
  ""
  (should (let ((oai-block-tags-use-simple-directory-content t))
            (and
             (string-match "oai-block-tags.el" (oai-block-tags--get-replacement-for-org-link "file:./"))
             (string-match "oai-block-tags.el" (oai-block-tags--get-replacement-for-org-link "[[./]]"))
             (string-match "oai-block-tags.el" (oai-block-tags--get-replacement-for-org-link "[[file:./]]"))
             (string-match "oai-block-tags.el" (oai-block-tags--get-replacement-for-org-link "[[file:.]]"))
             ))))
;;; - Test: oai-block-tags-replace
(ert-deftest oai-block-tags--replace-org-links-norm-header ()
  (let ((kill-buffer-query-functions))
    (with-temp-buffer
      (org-mode)
      (setq buffer-file-name "/mock/org.org")
      (insert "* headline\nasdas\n** sub-headline\n asd")
      (let (target)
        (setq target "11
# headline
asdas

## sub-headline
 asd

4444")
        (should (string-equal (oai-block-tags-replace  "11[[file:/mock/org.org::* headline]]4444")
                              target))


        (should (string-equal (oai-block-tags-replace  "11[[* headline]]4444")
                              target)))

      (set-buffer-modified-p nil))))


(when (featurep 'org-links)
  (ert-deftest oai-block-tags--replace-org-links-nn-header ()
    (let ((kill-buffer-query-functions)
          ;; (org-link-file-path-type 'absolute)
          ;; (org-link-search-must-match-exact-headline nil)
          )
      (with-temp-buffer
        (org-mode)
        (add-hook 'org-execute-file-search-functions #'org-links-additional-formats)
        (setq buffer-file-name "/mock/org.org")
        (insert "* headline\nasdas\n** sub-headline\n asd")
        (let (target)
          (setq target "11
# headline
asdas

## sub-headline
 asd

4444")
          (should (string-equal (oai-block-tags-replace  "11[[file:/mock/org.org::1::* headline]]4444")
                                target))

          (should (string-equal (oai-block-tags-replace  "11[[1::* headline]]4444")
                                target))
          )
        ;; (insert "[[file:/mock/org.org::1::* headline]]")

        (set-buffer-modified-p nil))))

(ert-deftest oai-block-tags--replace-org-links-num-num ()
  (let ((kill-buffer-query-functions))
    (with-temp-buffer
      (org-mode)
      (add-hook 'org-execute-file-search-functions #'org-links-additional-formats)
      (setq buffer-file-name "/mock/org.org")
      (insert "* headline\nasdas\n** sub-headline\n asd")
      (let (target)
        (setq target "11
```auto
* headline
asdas
```
4444")
      (should (string-equal (oai-block-tags-replace  "11[[file:/mock/org.org::1-2::* headline]]4444")
                            target))
      )
        (set-buffer-modified-p nil)))))

;; (ert-deftest oai-block-tags--replace-org-links-num-num ()
;;   (let ((kill-buffer-query-functions))
;;     (with-temp-buffer
;;       (org-mode)
;;       (setq buffer-file-name "/mock/org.org")
;;       (insert "* headline\nasdas\n** sub-headline\n asd")
;;       (let (target)
;;         (setq target "11
;; ```auto
;; * headline
;; asdas
;; ```
;; 4444")

;;       (should (string-equal (oai-block-tags-replace  "11[[file:/mock/org.org::1-2::* headline]]4444")
;;                             target))
;;       )
;;         (set-buffer-modified-p nil))))





;;; - tags tests
(ert-deftest oai-block-tags-replace-test ()
    (let* ((temp-file (make-temp-file "mytest"))
           (res
            (unwind-protect
                (progn
                  (with-temp-file temp-file
                    (insert "Hello, world test!"))
                  (print temp-file)

                  (prog1 (oai-block-tags-replace (format "aas `@%s`bb." temp-file))
                    ;; (should (string= (oai-block-tags-replace temp-file) "Expected result")))
                    (delete-file temp-file)))))
           (res (split-string res "\n")))
      (should (string-equal "aas " (nth 0 res)))
      (should (string-equal "```" (nth 2 res)))
      (should (string-equal "Hello, world test!" (nth 3 res)))
      (should (string-equal "```" (nth 4 res)))
      (should (string-equal "bb." (nth 5 res)))))


;;; provide
(provide 'oai-tests-block-tags)

;;; oai-tests-block-tags.el ends here
