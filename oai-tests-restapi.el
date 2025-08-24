(require 'ert)
(require 'oai-restapi)

;; (eval-buffer)
;; (ert t)
;;

(defun oai-tests--progress-reporter-stop-one ()
  "Start one request
Stop it with `oai-restapi-stop-url-request'.
"

  (let ((buf (generate-new-buffer "*oai-test-temp*")))
    (with-current-buffer buf
      (org-mode)
      (insert "#+begin_ai\n#+end_ai")
      (goto-char (point-min))
      (oai-block-p)
      ))
  )

;;; - For `oai-restapi--get-token' (old)

;; (require 'oai) ;; Assuming the function is defined in oai.el

(ert-deftest oai-restapi--get-token-string-test ()
  "Test when oai-restapi-con-token is a non-empty string."
  (let ((oai-restapi-con-token "test-token-123"))
    (should (equal (oai-restapi--get-token 'openai) "test-token-123")))) ; ignored

(ert-deftest oai-restapi--get-token-plist-valid-test ()
  "Test when oai-restapi-con-token is a plist with valid service token."
  (let ((oai-restapi-con-token '(:openai "test-token-openai" :anthropic "test-token-anthropic")))
    (should (equal (oai-restapi--get-token :openai) "test-token-openai"))))

(ert-deftest oai-restapi--get-token-plist-invalid-test ()
  "Test when oai-restapi-con-token is a plist without the service token."
  (let ((oai-restapi-con-token '(:anthropic "test-token-anthropic")))
    (let ((err (cadr
                (should-error (oai-restapi--get-token :openai) :type 'error))))
      (should (eql 0 (string-match "Token not found" err))))))

(ert-deftest oai-restapi--get-token-auth-source-test ()
  "Test when token is retrieved from auth-source."
  (let ((oai-restapi-con-token "")
        (auth-sources '((:host "api.openai.com" :user "user" :secret "auth-token-123"))))
    (fset 'oai-restapi--get-token-auth-source (lambda (service) "auth-token-123"))
    (should (equal (oai-restapi--get-token 'openai) "auth-token-123"))
    (fmakunbound 'oai-restapi--get-token-auth-source)))

(ert-deftest oai-restapi--get-token-no-valid-token-test ()
  "Test when no valid token is provided."
  (let ((oai-restapi-con-token "")
        (auth-sources nil))
    (fset 'oai-restapi--get-token-auth-source (lambda (service) nil))
    (let ((err (cadr
                (should-error (oai-restapi--get-token :openai) :type 'error))))
      ;; (print err)
      (should (eql 0 (string-match "Please set" err))))

    )
    (fmakunbound 'oai-restapi--get-token-auth-source))

;;;
;;; - For `oai-restapi--get-token'
;; Dummy function for auth-source behavior
(defun oai-restapi--get-token-auth-source (service) nil)

(ert-deftest oai-restapi--get-token/string ()
  "Single string in `oai-restapi-con-token` returns value."
  (let ((oai-restapi-con-token "tok123"))
    (should (equal (oai-restapi--get-token "foo") "tok123"))))

;; (ert-deftest oai-restapi--get-token/empty-string-error ()
;;   "Empty string errors out."
;;   (let ((oai-restapi-con-token ""))
;;     (let ((err (cadr
;;                 (should-error (oai-restapi--get-token :openai) :type 'error))))
;;       ;; (print err)
;;       (should (eql 0 (string-match "Please set" err)))
;;       )))

(ert-deftest oai-restapi--get-token/plist-string ()
  "Plist with symbol key, single string."
  (let ((oai-restapi-con-token '(:foo "tokfoo")))
    (should (equal (oai-restapi--get-token "foo") "tokfoo"))))

(ert-deftest oai-restapi--get-token/plist-list-by-index ()
  "Plist with key and list of strings, access by index."
  (cl-labels ((oai-restapi--split-dash-number (s) (cons "foo" 1))) ;; fake service splitting
    (let ((oai-restapi-con-token '(:foo ("tok0" "tok1"))))
      (should (equal (oai-restapi--get-token "foo--1") "tok1")))))

(ert-deftest oai-restapi--get-token/plist-list-car ()
  "Plist with key and list of strings, no index (get car)."
  (let ((oai-restapi-con-token '(:foo ("tok0" "tok1"))))
    (should (equal (oai-restapi--get-token "foo") "tok0"))))

(ert-deftest oai-restapi--get-token/plist-error-when-key-not-found ()
  "Plist with missing key errors."
  (let ((oai-restapi-con-token '(:foo "tokfoo")))
    (let ((err (cadr
                (should-error (oai-restapi--get-token "bar") :type 'error))))
      (print err)
      (should (eql 0 (string-match "Token not found" err))))))


(ert-deftest oai-restapi--get-token/plist-bad-config ()
  "Plist with invalid structure signals error."
  (let ((oai-restapi-con-token '(:foo 1234)))
    (should-error (oai-restapi--get-token "foo")
                  :type 'error)))

;; (ert-deftest oai-restapi--get-token/missing-errors ()
;;   "Neither string, plist nor auth-source: signals error."
;;   (let ((oai-restapi-con-token nil))
;;     (should-error (oai-restapi--get-token "foo")
;;                   :type 'error)))


;;; - oai-restapi--get-value-or-string
(ert-deftest oai-restapi--get-value-or-string-test ()
  ;; Example variables
  (defvar my-plist '(:foo "bar" :baz "qux"))
  (defvar my-string "hello")
  (defvar my-number 42)

  ;; Using oai-restapi--get-value-or-string
  (should (equal (oai-restapi--get-value-or-string my-plist "foo") "bar"))
  (should (equal (oai-restapi--get-value-or-string my-string "foo") "hello"))
  (should (equal (oai-restapi--get-value-or-string my-number "foo")  nil))
)
;;         (oai-block--set-variable

;; ;;     (with-current-buffer buf
;; ;;       (org-mode)
;;   (let ((buf (generate-new-buffer "*oai-test-temp*")))
;;     ))

;; (defun oai-tests--progress-reporter-start-two-and-stop-one ()
;;   "."
;;   (let ((buf (generate-new-buffer "*oai-test-temp*")))
;;     (with-current-buffer buf
;;       (org-mode)
;;       (setq-local org-export-with-properties t) ; Ensure properties are considered
;;       (when properties-alist
;;         (dolist (prop properties-alist)
;;           (insert (format "#+PROPERTY: %s %s\n" (car prop) (cdr prop)))))
;;       (insert block-content)
;;       (goto-char (point-min))
;;       ;; Move point to the start of the AI block to ensure `org-element-at-point` works
;;       ;; and `org-entry-get-with-inheritance` can find properties.
;;       (search-forward "#+begin_ai")
;;       (let* ((element (org-element-at-point))
;;              ;; org-element-property :parameters returns a plist, which alist-get works on.
;;              (info-alist (org-element-property :parameters element)))
;;         element))))
;;; others
(defun test-oai-restapi--strip-api-url ()
  "Runs tests for `oai-restapi--strip-api-url` explicitly for each case,
   without using a loop or an explicit assert function."

  (unless (string= (oai-restapi--strip-api-url "https://api.perplexity.ai/chat/completions") "api.perplexity.ai")
    (error "Test 1 Failed: https://api.perplexity.ai/chat/completions"))

  (unless (string= (oai-restapi--strip-api-url "http://www.example.com/path/to/file") "www.example.com")
    (error "Test 2 Failed: http://www.example.com/path/to/file"))

  ;; (unless (string= (oai-restapi--strip-api-url "ftp://some.server.org") "some.server.org")
  ;;   (error "Test 3 Failed: ftp://some.server.org"))

  (unless (string= (oai-restapi--strip-api-url "no-protocol.com/stuff") "no-protocol.com")
    (error "Test 4 Failed: no-protocol.com/stuff"))

  (unless (string= (oai-restapi--strip-api-url "http://www.google.com/search?q=elisp") "www.google.com")
    (error "Test 5 Failed: http://www.google.com/search?q=elisp"))

  (unless (string= (oai-restapi--strip-api-url "localhost:8080/app") "localhost:8080")
    (error "Test 6 Failed: localhost:8080/app"))

  (unless (string= (oai-restapi--strip-api-url "example.com") "example.com")
    (error "Test 7 Failed: example.com"))

  (unless (string= (oai-restapi--strip-api-url "https://sub.domain.co.uk") "sub.domain.co.uk")
    (error "Test 8 Failed: https://sub.domain.co.uk"))

  (unless (string= (oai-restapi--strip-api-url "domain.com/") "domain.com")
    (error "Test 9 Failed: domain.com/"))

  (unless (string= (oai-restapi--strip-api-url "localhost") "localhost")
    (error "Test 10 Failed: localhost"))

  ;; (unless (string= (oai-restapi--strip-api-url "") "")
  ;;   (error "Test 11 Failed: empty string"))

  (message "All individual tests passed for oai-restapi--strip-api-url!")
  t) ; Return t for success

(test-oai-restapi--strip-api-url)
