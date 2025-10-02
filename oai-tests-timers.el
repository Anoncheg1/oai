; -*- lexical-binding: t -*-
(require 'ert)             ; Testing framework
(require 'oai-timers)

;; (eval-buffer) or (load-file "path/to/async-tests.el")
;; Running Tests: Load the test file and run:
;; (eval-buffer)
;; (ert t)

;;; -=-= 1)
(ert-deftest oai-tests-timers--get-keys-for-variable ()
  "Should return current buffer as key for the marker present, only once (seq-uniq)."
  (let* ((marker (make-marker))
         (cb (current-buffer))
         ;; Only the first pair with cb will be considered for alist-get
         (oai-timers--element-marker-variable-dict `((,cb . ,marker)
                                                     (,cb . ,(make-marker))
                                                     (,cb . ,marker))))
    (let ((result (oai-timers--get-keys-for-variable marker)))
      ;; Result should just be a list containing cb, thanks to seq-uniq
      (should (equal result (list cb)))
      (should (= (length result) 1)))))


(ert-deftest oai-tests-timers--get-keys-for-variable-none ()
  "Should return nil when marker not present in the dict."
  (let* ((query-marker (make-marker))
         (cb (current-buffer))
         (marker1 (make-marker))
         (marker2 (make-marker)))
    (set-marker marker1 1)
    (set-marker marker2 2)
    (let ((oai-timers--element-marker-variable-dict `((,cb . ,marker1)
                                                     (,cb . ,marker2))))
      ;; query-marker is unpositioned, so not equal to marker1 or marker2
      (should (equal (oai-timers--get-keys-for-variable query-marker) nil)))))

(ert-deftest oai-tests-timers--get-keys-for-variable-none-symbol ()
  "Should return nil when symbol not present in the dict."
  (let* ((cb (current-buffer))
         (oai-timers--element-marker-variable-dict `((,cb . alpha)
                                                     (,cb . beta))))
    (should (equal (oai-timers--get-keys-for-variable 'gamma) nil))))

(ert-deftest oai-tests-timers--set-and-get-keys-for-variable ()
  "Testing set, get, and keys-for-variable."
  (let ((marker (make-marker))
        (cb (current-buffer))
        (oai-timers--element-marker-variable-dict nil))
    ;; Set mapping
    (oai-timers--set cb marker)
    (let ((result (oai-timers--get-keys-for-variable marker)))
      (should (equal result (list cb)))
      (should (= (length result) 1)))))

;;; -=-= 2)

(ert-deftest oai-tests-timers--set-and-get-variable1 ()
  "Test setting and getting variables by key."
  (let ((oai-timers--element-marker-variable-dict nil))
    (oai-timers--set 1 'foo)
    (should (equal (oai-timers--get-variable 1) 'foo))
    ;; Overwrite value
    (oai-timers--set 1 'bar)
    (should (equal (oai-timers--get-variable 1) 'bar))
    ;; Setting to nil removes the key
    (oai-timers--set 1 nil)
    (should-not (oai-timers--get-variable 1))))

(ert-deftest oai-tests-timers--get-keys-for-variable2 ()
  "Test retrieval of all keys mapped to a variable."
  (let ((oai-timers--element-marker-variable-dict nil))
    (oai-timers--set 'a 100)
    (oai-timers--set 'b 200)
    (oai-timers--set 'c 100)
    ;; Both 'a and 'c map to 100, 'b to 200
    (should (equal (sort (oai-timers--get-keys-for-variable 100) #'string<) '(a c)))
    (should (equal (oai-timers--get-keys-for-variable 200) '(b)))
    ;; Not present
    (should (equal (oai-timers--get-keys-for-variable 300) nil))))

(ert-deftest oai-tests-timers--remove-variable-removes-all-matching ()
  "Test removing all mappings by variable (eq)."
  (let ((oai-timers--element-marker-variable-dict nil))
    (oai-timers--set 'a 'marker1)
    (oai-timers--set 'b 'marker2)
    (oai-timers--set 'c 'marker1)
    ;; Remove marker1, leaving only marker2 mapping
    (oai-timers--remove-variable 'marker1)
    ;; (print oai-timers--element-marker-variable-dict))
    ;; (equal oai-timers--element-marker-variable-dict '((b . marker2))))
    (should (equal oai-timers--element-marker-variable-dict '((b . marker2))))
    ;; Remove marker2, dict is empty
    (oai-timers--remove-variable 'marker2)
    (should (equal oai-timers--element-marker-variable-dict nil))))

(ert-deftest oai-tests-timers--remove-key-removes-only-that-key ()
  "Test removing only the specified key."
  (let ((oai-timers--element-marker-variable-dict nil))
    (oai-timers--set 'alpha 'v1)
    (oai-timers--set 'beta 'v2)
    (oai-timers--remove-key 'beta)
    (should (equal oai-timers--element-marker-variable-dict '((alpha . v1))))
    ;; Removing non-existing key does nothing
    (oai-timers--remove-key 'gamma)
    (should (equal oai-timers--element-marker-variable-dict '((alpha . v1))))))

(ert-deftest oai-tests-timers--get-all-keys ()
  "Test getting all unique keys."
  (let ((oai-timers--element-marker-variable-dict nil))
    (oai-timers--set 10 'x)
    (oai-timers--set 20 'y)
    (oai-timers--set 30 'x)
    (should (equal (sort (oai-timers--get-all-keys) #'<) '(10 20 30)))
    ;; Remove one key
    (oai-timers--remove-key 20)
    (should (equal (sort (oai-timers--get-all-keys) #'<) '(10 30)))
    ;; Remove all
    (oai-timers--remove-key 10)
    (oai-timers--remove-key 30)
    (should (equal (oai-timers--get-all-keys) nil))))

;;; -=-= 3) special cases
(ert-deftest oai-tests-timers--buffer-key-type ()
  (let ((oai-timers--element-marker-variable-dict nil))
    (oai-timers--set (current-buffer) 'symbval)
    (should (equal (oai-timers--get-variable (current-buffer)) 'symbval))
    ;; Remove complex key
    (oai-timers--remove-key (current-buffer))
    (should-not (oai-timers--get-variable (current-buffer)))))


;; ### 3. **Test setting a key multiple times with different values**
;; Make sure only last value remains, test repeated setting.
(ert-deftest oai-tests-timers--repeated-set-override ()
  (let ((oai-timers--element-marker-variable-dict nil))
    (oai-timers--set 'x 'a)
    (oai-timers--set 'x 'b)
    (oai-timers--set 'x 'c)
    (should (equal (oai-timers--get-variable 'x) 'c))
    (should (equal (length oai-timers--element-marker-variable-dict) 1))))

;; ### 4. **Test `get-keys-for-variable` with no matches**
;; Should return `nil` or empty list.
(ert-deftest oai-tests-timers--get-keys-for-nonexistent-variable ()
  (let ((oai-timers--element-marker-variable-dict nil))
    (oai-timers--set 'a 'one)
    (should (equal (oai-timers--get-keys-for-variable 'missing) nil))))

;; ### 5. **Test with empty dictionary**
;; All retrieval functions should handle empty dict gracefully.
(ert-deftest oai-tests-timers--empty-dict-behavior ()
  (let ((oai-timers--element-marker-variable-dict nil))
    (should-not (oai-timers--get-variable 'a))
    (should (equal (oai-timers--get-keys-for-variable 'x) nil))
    (should (equal (oai-timers--get-all-keys) nil))))

;; ### 6. **Test removing key that does not exist**
;; Should be a no-op.
(ert-deftest oai-tests-timers--remove-nonexistent-key-noop ()
  (let ((oai-timers--element-marker-variable-dict nil))
    (oai-timers--set 'a 'xx)
    (oai-timers--remove-key 'nonexistent)
    (should (equal (oai-timers--get-all-keys) '(a)))))

;; ### 7. **Test that `set`ing a key to nil really removes it, not just assigns nil**
;; Ensure the pair is gone (not key with nil value).
(ert-deftest oai-tests-timers--set-key-to-nil-actually-removes ()
  (let ((oai-timers--element-marker-variable-dict nil))
    (oai-timers--set 'foo 'bar)
    (oai-timers--set 'foo nil)
    (should-not (assoc 'foo oai-timers--element-marker-variable-dict))))

;; ### 8. **Test `get-all-keys` with duplicate keys (should be unique)**
(ert-deftest oai-tests-timers--get-all-keys-uniqueness ()
  (let ((oai-timers--element-marker-variable-dict nil))
    (oai-timers--set 'dup 'a)
    (oai-timers--set 'dup 'b) ; overwrite
    (should (equal (oai-timers--get-all-keys) '(dup)))))
;;; To run these tests:
;; 1. Save the code to an .el file (e.g., `org-ai-params-test.el`).
;; 2. Open Emacs and load the file: `M-x load-file RET org-ai-tests2.el RET`.
;; 3. Run all tests: `M-x ert RET t RET`.
;;    Or run specific tests: `M-x ert RET oai-block--let-params-all-from-info RET`.
;; OR
;; to run: emacs -batch -l ert -l pinyin-isearch.el -l pinyin-isearch-pinyin-tests.el -f ert-run-tests-batch-and-exit 2> out.log
;;  OR
;; eval-buffer
;; M-x ert RET t RET
