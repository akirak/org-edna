;;; org-edna-tests.el --- Tests for org-edna

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Author: Ian Dunn <dunni@gnu.org>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'org-edna)
(require 'ert)
(require 'org-id)

(defvar org-edna-test-inhibit-messages nil
  "Whether to inhibit messages (apart from ERT messages).")

(defconst org-edna-test-dir
  (expand-file-name (file-name-directory (or load-file-name buffer-file-name))))

(defconst org-edna-test-file
  (expand-file-name "org-edna-tests.org" org-edna-test-dir))

(defconst org-edna-tests-el
  (expand-file-name "org-edna-tests.el" org-edna-test-dir))

;; Jan 15, 2000; chosen at random
(defconst org-edna-test-time
  (encode-time 0 0 0 15 1 2000))

(defconst org-edna-test-sibling-one-id   "82a4ac3d-9565-4f94-bc84-2bbfd8d7d96c")
(defconst org-edna-test-sibling-two-id   "72534efa-e932-460b-ae2d-f044a0074815")
(defconst org-edna-test-sibling-three-id "06aca55e-ce09-46df-80d7-5b52e55d6505")
(defconst org-edna-test-parent-id        "21b8f1f5-14e8-4677-873d-69e0389fdc9e")
(defconst org-edna-test-id-heading-one   "0d491588-7da3-43c5-b51a-87fbd34f79f7")
(defconst org-edna-test-id-heading-two   "b010cbad-60dc-46ef-a164-eb155e62cbb2")
(defconst org-edna-test-id-heading-three "97e6b0f0-40c4-464f-b760-6e5ca9744eb5")
(defconst org-edna-test-id-heading-four  "7d4d564b-18b2-445c-a0c8-b1b3fb9ad29e")
(defconst org-edna-test-archive-heading  "d7668277-f959-43ba-8e85-8a3c76996862")

(defconst org-edna-test-relative-grandparent "c07cf4c1-3693-443a-9d79-b581f7cbd62c")
(defconst org-edna-test-relative-parent-one  "5a35daf7-4957-4588-9a68-21d8763a9e0d")
(defconst org-edna-test-relative-parent-two  "4fe67f03-2b35-4708-8c38-54d2c4dfab81")
(defconst org-edna-test-relative-standard-child "7c542695-8165-4c8b-b44d-4c12fa009548")
(defconst org-edna-test-relative-child-with-children "c7a986df-8d89-4509-b086-6db429b5607b")
(defconst org-edna-test-relative-grandchild-one "588bbd29-2e07-437f-b74d-f72459b545a1")
(defconst org-edna-test-relative-grandchild-two "a7047c81-21ec-46cd-8289-60ad515900ff")
(defconst org-edna-test-relative-child-with-todo "8c0b31a1-af49-473c-92ea-a5c1c3bace33")
(defconst org-edna-test-relative-commented-child "0a1b9508-17ce-49c5-8ff3-28a0076374f5")
(defconst org-edna-test-relative-archived-child "a4b6131e-0560-4201-86d5-f32b36363431")
(defconst org-edna-test-relative-child-with-done "4a1d74a2-b032-47da-a823-b32f5cab0aae")

(defun org-edna-test-restore-test-file ()
  "Restore the test file back to its original state."
  (with-current-buffer (get-file-buffer org-edna-test-file)
    (revert-buffer nil t)))

(defmacro org-edna-protect-test-file (&rest body)
  (declare (indent 0))
  `(unwind-protect
       (progn ,@body)
     ;; Change the test file back to its original state.
     (org-edna-test-restore-test-file)))

(defmacro org-edna-test-setup (&rest body)
  "Common settings for tests."
  (declare (indent 0))
  ;; Override `current-time' so we can get a deterministic value
  `(cl-letf* (((symbol-function 'current-time) (lambda () org-edna-test-time))
              ;; Only use the test file in the agenda
              (org-agenda-files `(,org-edna-test-file))
              ;; Ensure interactive modification of TODO states works.
              (org-todo-keywords '((sequence "TODO" "|" "DONE")))
              ;; Only block based on Edna
              (org-blocker-hook 'org-edna-blocker-function)
              ;; Only trigger based on Edna
              (org-trigger-hook 'org-edna-trigger-function)
              ;; Inhibit messages if indicated
              (inhibit-message org-edna-test-inhibit-messages))
     ,@body))

(defmacro org-edna-with-point-at-test-heading (heading-id &rest body)
  (declare (indent 1))
  `(org-with-point-at (org-edna-find-test-heading ,heading-id)
     ,@body))

(defmacro org-edna-with-test-heading (heading-id &rest body)
  "Establish a test case with test heading HEADING-ID.

HEADING-ID is a UUID string of a heading to use.

Moves point to the heading, protects the test file, sets default
test settings, then runs BODY."
  (declare (indent 1))
  `(org-edna-test-setup
     (org-edna-protect-test-file
       (org-edna-with-point-at-test-heading ,heading-id
         ,@body))))

(defun org-edna-find-test-heading (id)
  "Find the test heading with id ID.

This avoids org-id digging into its internal database."
  (org-id-find-id-in-file id org-edna-test-file t))

;; _test exists to give more detailed reports in ERT output.
(defun org-edna-test-compare-todos (pom expected-state _test)
  (string-equal (org-entry-get pom "TODO") expected-state))

(defun org-edna-test-change-todo-state (pom new-state)
  (org-with-point-at pom (org-todo new-state)))

(defun org-edna-test-check-block (pom _test)
  "Check if the heading at point-or-marker POM is blocked."
  (org-edna-test-change-todo-state pom "DONE")
  (org-edna-test-compare-todos pom "TODO" _test))

(defun org-edna-test-mark-done (&rest poms)
  "Mark all points-or-markers in POMS as DONE."
  (dolist (pom poms)
    (org-edna-test-change-todo-state pom "DONE")))

(defun org-edna-test-mark-todo (&rest poms)
  "Mark all points-or-markers in POMS as TODO."
  (dolist (pom poms)
    (org-edna-test-change-todo-state pom "TODO")))

(defun org-edna-test-children-marks ()
  (org-edna-collect-descendants nil))


;;; Parser Tests

(ert-deftest org-edna-parse-form-no-arguments ()
  (let* ((input-string "test-string")
         (parsed       (org-edna-parse-string-form input-string)))
    (should parsed)
    (should (= (length parsed) 2))
    (pcase-let* ((`((,key . ,args) ,pos) parsed))
      (should (eq key 'test-string))
      (should (not args))
      (should (= pos 11)))))

(ert-deftest org-edna-parse-form-no-arguments-modifier ()
  (let* ((input-string "!test-string")
         (parsed       (org-edna-parse-string-form input-string)))
    (should parsed)
    (should (= (length parsed) 2))
    (pcase-let* ((`((,key . ,args) ,pos) parsed))
      (should (eq key '!test-string))
      (should (not args))
      (should (= pos 12)))))

(ert-deftest org-edna-parse-form-single-argument ()
  (let* ((input-string "test-string(abc)")
         (parsed       (org-edna-parse-string-form input-string)))
    (should parsed)
    (should (= (length parsed) 2))
    (pcase-let* ((`((,key . ,args) ,pos) parsed))
      (should (eq key 'test-string))
      (should (= (length args) 1))
      (should (symbolp (nth 0 args)))
      (should (eq (nth 0 args) 'abc))
      (should (= pos (length input-string))))))

(ert-deftest org-edna-parse-form-string-argument ()
  (let* ((input-string "test-string(abc \"def (ghi)\")")
         (parsed       (org-edna-parse-string-form input-string)))
    (should parsed)
    (should (= (length parsed) 2))
    (pcase-let* ((`((,key . ,args) ,pos) parsed))
      (should (eq key 'test-string))
      (should (= (length args) 2))
      (should (symbolp (nth 0 args)))
      (should (eq (nth 0 args) 'abc))
      (should (stringp (nth 1 args)))
      (should (string-equal (nth 1 args) "def (ghi)"))
      (should (= pos (length input-string))))))

(ert-deftest org-edna-parse-form-multiple-forms ()
  (let ((input-string "test-string1 test-string2")
        pos)
    (pcase-let* ((`((,key1 . ,args1) ,pos1) (org-edna-parse-string-form input-string)))
      (should (eq key1 'test-string1))
      (should (not args1))
      (should (= pos1 13))
      (setq pos pos1))
    (pcase-let* ((`((,key2 . ,args2) ,pos2) (org-edna-parse-string-form (substring input-string pos))))
      (should (eq key2 'test-string2))
      (should (not args2))
      (should (= pos2 12)))))

(ert-deftest org-edna-parse-form-empty-argument-list ()
  (let ((input-string "test-string1()"))
    (pcase-let* ((`((,key1 ,args1) ,pos1) (org-edna-parse-string-form input-string)))
      (should (eq key1 'test-string1))
      (should (not args1))
      (should (= pos1 (length input-string))))))

(ert-deftest org-edna-parse-form-condition ()
  (let ((input-string "variable-set?()"))
    (pcase-let* ((`((,key1 . ,args1) ,pos1) (org-edna-parse-string-form input-string))
                 (`(,modifier1 . ,key1) (org-edna-break-modifier key1))
                 (`(,type . ,func) (org-edna--function-for-key key1)))
      (should (eq key1 'variable-set?))
      (should (not args1))
      (should (not modifier1))
      (should (= pos1 (length input-string)))
      (should (eq type 'condition))
      (should (eq func 'org-edna-condition/variable-set?)))))

(ert-deftest org-edna-form-to-sexp-no-arguments ()
  (let* ((input-string "self")
         (sexp (org-edna-string-form-to-sexp-form input-string 'condition)))
    (should (equal
             sexp
             '(((self)
                (!done?)))))))

(ert-deftest org-edna-form-to-sexp-negation ()
  (let* ((input-string "self !done?")
         (sexp (org-edna-string-form-to-sexp-form input-string 'condition)))
    (should (equal
             sexp
             '(((self)
                (!done?)))))))

(ert-deftest org-edna-form-to-sexp-arguments ()
  (let* ((input-string "match(\"checklist\") todo!(TODO)")
         (sexp (org-edna-string-form-to-sexp-form input-string 'action)))
    (should (equal
             sexp
             '(((match "checklist")
               (todo! TODO)))))))

(ert-deftest org-edna-form-to-sexp-if-no-else ()
  (let* ((input-string "if match(\"checklist\") done? then self todo!(TODO) endif")
         (sexp (org-edna-string-form-to-sexp-form input-string 'action)))
    (should (equal
             sexp
             '(((if (((match "checklist")
                      (done?)))
                    (((self)
                      (todo! TODO)))
                  nil)))))))

(ert-deftest org-edna-form-to-sexp-if-else ()
  (let* ((input-string "if match(\"checklist\") done? then self todo!(TODO) else siblings todo!(DONE) endif")
         (sexp (org-edna-string-form-to-sexp-form input-string 'action)))
    (should (equal
             sexp
             '(((if (((match "checklist")
                      (done?)))
                    (((self)
                      (todo! TODO)))
                  (((siblings)
                    (todo! DONE))))))))))

(ert-deftest org-edna-form-to-sexp-if-multiple-thens ()
  (let* ((input-string "if match(\"checklist\") done? then self next-sibling todo!(TODO) self set-property!(\"COUNTER\" \"0\") endif")
         (sexp (org-edna-string-form-to-sexp-form input-string 'action)))
    (should (equal
             sexp
             '(((if (((match "checklist")
                      (done?)))
                    (((self)
                      (next-sibling)
                      (todo! TODO))
                     ((self)
                      (set-property! "COUNTER" "0")))
                  nil)))))))

(ert-deftest org-edna-form-to-sexp-if-multiple-elses ()
  (let* ((input-string "if match(\"checklist\") done? then self todo!(TODO) else siblings todo!(DONE) self todo!(TODO) endif")
         (sexp (org-edna-string-form-to-sexp-form input-string 'action)))
    (should (equal
             sexp
             '(((if (((match "checklist")
                      (done?)))
                    (((self)
                      (todo! TODO)))
                  (((siblings)
                    (todo! DONE))
                   ((self)
                    (todo! TODO))))))))))

(ert-deftest org-edna-form-to-sexp-failed-if ()
  (pcase-let* ((input-string "if match(\"checklist\") done?")
               (`(,error . ,data) (should-error (org-edna-string-form-to-sexp-form
                                                input-string 'action)
                                               :type 'invalid-read-syntax)))
    (should (eq error 'invalid-read-syntax))
    (should (listp data))
    (should (eq (length data) 6))
    (should (string-equal (plist-get data :msg) "Malformed if-construct; expected then terminator"))
    ;; Error should point to the start of the if-statement
    (should (eq (plist-get data :error-pos) 0))))

(ert-deftest org-edna-form-to-sexp-failed-if-then ()
  (pcase-let* ((input-string "if match(\"checklist\") done? then")
               (`(,error . ,data) (should-error (org-edna-string-form-to-sexp-form
                                                input-string 'action)
                                               :type 'invalid-read-syntax)))
    (should (eq error 'invalid-read-syntax))
    (should (listp data))
    (should (eq (length data) 6))
    (should (string-equal (plist-get data :msg)
                          "Malformed if-construct; expected else or endif terminator"))
    ;; Error should point to the start of the if-statement
    (should (eq (plist-get data :error-pos) 28))))

(ert-deftest org-edna-form-to-sexp-failed-if-then-else ()
  (pcase-let* ((input-string "if match(\"checklist\") done? then todo!(TODO) else todo!(TODO)")
               (`(,error . ,data) (should-error (org-edna-string-form-to-sexp-form
                                                 input-string 'action)
                                                :type 'invalid-read-syntax)))
    (should (eq error 'invalid-read-syntax))
    (should (listp data))
    (should (eq (length data) 6))
    (should (string-equal (plist-get data :msg)
                          "Malformed if-construct; expected endif terminator"))
    ;; Error should point to the start of the if-statement
    (should (eq (plist-get data :error-pos) 45))))


;;; Finders

(defsubst org-edna-heading (pom)
  (org-with-point-at pom
    (org-get-heading t t t t)))

(ert-deftest org-edna-finder/match-single-arg ()
  (org-edna-test-setup
    (let* ((targets (org-edna-finder/match "test&1")))
      (should (= (length targets) 2))
      (should (string-equal (org-edna-heading (nth 0 targets)) "Tagged Heading 1"))
      (should (string-equal (org-edna-heading (nth 1 targets)) "Tagged Heading 2")))))

(ert-deftest org-edna-finder/ids-single ()
  (org-edna-test-setup
    (let* ((test-id "caccd0a6-d400-410a-9018-b0635b07a37e")
           (targets (org-edna-finder/ids test-id)))
      (should (= (length targets) 1))
      (should (string-equal (org-edna-heading (nth 0 targets)) "Blocking Test"))
      (should (string-equal (org-entry-get (nth 0 targets) "ID") test-id)))))

(ert-deftest org-edna-finder/ids-multiple ()
  (org-edna-test-setup
    (let* ((test-ids '("0d491588-7da3-43c5-b51a-87fbd34f79f7"
                       "b010cbad-60dc-46ef-a164-eb155e62cbb2"))
           (targets (apply 'org-edna-finder/ids test-ids)))
      (should (= (length targets) 2))
      (should (string-equal (org-edna-heading (nth 0 targets)) "ID Heading 1"))
      (should (string-equal (org-entry-get (nth 0 targets) "ID") (nth 0 test-ids)))
      (should (string-equal (org-edna-heading (nth 1 targets)) "ID Heading 2"))
      (should (string-equal (org-entry-get (nth 1 targets) "ID") (nth 1 test-ids))))))

(ert-deftest org-edna-finder/match-blocker ()
  (org-edna-test-setup
    (let* ((heading (org-edna-find-test-heading "caccd0a6-d400-410a-9018-b0635b07a37e"))
           (blocker (org-entry-get heading "BLOCKER"))
           blocking-entry)
      (should (string-equal "match(\"test&1\")" blocker))
      (org-with-point-at heading
        (setq blocking-entry (org-edna-process-form blocker 'condition)))
      (should (string-equal (substring-no-properties blocking-entry)
                            "TODO Tagged Heading 1 :1:test:")))))

(ert-deftest org-edna-finder/file ()
  (let* ((targets (org-edna-finder/file org-edna-test-file)))
    (should (= (length targets) 1))
    (should (markerp (nth 0 targets)))
    (org-with-point-at (nth 0 targets)
      (should (equal (current-buffer) (find-buffer-visiting org-edna-test-file)))
      (should (equal (point) 1)))))

(ert-deftest org-edna-finder/org-file ()
  (let* ((org-directory (file-name-directory org-edna-test-file))
         (targets (org-edna-finder/org-file (file-name-nondirectory org-edna-test-file))))
    (should (= (length targets) 1))
    (should (markerp (nth 0 targets)))
    (org-with-point-at (nth 0 targets)
      (should (equal (current-buffer) (find-buffer-visiting org-edna-test-file)))
      (should (equal (point) 1)))))

(ert-deftest org-edna-finder/self ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading "82a4ac3d-9565-4f94-bc84-2bbfd8d7d96c"))
         (targets (org-with-point-at current (org-edna-finder/self))))
    (should (= (length targets) 1))
    (should (equal current (nth 0 targets)))))

(ert-deftest org-edna-finder/siblings ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading org-edna-test-sibling-one-id))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    `(,org-edna-test-sibling-one-id
                      ,org-edna-test-sibling-two-id
                      ,org-edna-test-sibling-three-id)))
         (targets (org-with-point-at current
                    (org-edna-finder/siblings))))
    (should (equal siblings targets))))

(ert-deftest org-edna-finder/siblings-wrap ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading "72534efa-e932-460b-ae2d-f044a0074815"))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    '("06aca55e-ce09-46df-80d7-5b52e55d6505"
                      "82a4ac3d-9565-4f94-bc84-2bbfd8d7d96c")))
         (targets (org-with-point-at current
                    (org-edna-finder/siblings-wrap))))
    (should (= (length targets) 2))
    (should (equal siblings targets))))

(ert-deftest org-edna-finder/rest-of-siblings ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading "72534efa-e932-460b-ae2d-f044a0074815"))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    '("06aca55e-ce09-46df-80d7-5b52e55d6505")))
         (targets (org-with-point-at current
                    (org-edna-finder/rest-of-siblings))))
    (should (= (length targets) 1))
    (should (equal siblings targets))))

(ert-deftest org-edna-finder/next-sibling ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading "72534efa-e932-460b-ae2d-f044a0074815"))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    '("06aca55e-ce09-46df-80d7-5b52e55d6505")))
         (targets (org-with-point-at current
                    (org-edna-finder/next-sibling))))
    (should (= (length targets) 1))
    (should (equal siblings targets))))

(ert-deftest org-edna-finder/next-sibling-wrap-next ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading org-edna-test-sibling-two-id))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    `(,org-edna-test-sibling-three-id)))
         (targets (org-with-point-at current
                    (org-edna-finder/next-sibling-wrap))))
    (should (= (length targets) 1))
    (should (equal siblings targets))))

(ert-deftest org-edna-finder/next-sibling-wrap-wrap ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading org-edna-test-sibling-three-id))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    `(,org-edna-test-sibling-one-id)))
         (targets (org-with-point-at current
                    (org-edna-finder/next-sibling-wrap))))
    (should (= (length targets) 1))
    (should (equal siblings targets))))

(ert-deftest org-edna-finder/previous-sibling ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading "06aca55e-ce09-46df-80d7-5b52e55d6505"))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    '("72534efa-e932-460b-ae2d-f044a0074815")))
         (targets (org-with-point-at current
                    (org-edna-finder/previous-sibling))))
    (should (= (length targets) 1))
    (should (equal siblings targets))))

(ert-deftest org-edna-finder/first-child ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading org-edna-test-parent-id))
         (first-child (list (org-edna-find-test-heading org-edna-test-sibling-one-id)))
         (targets (org-with-point-at current
                    (org-edna-finder/first-child))))
    (should (= (length targets) 1))
    (should (equal first-child targets))))

(ert-deftest org-edna-finder/children ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading org-edna-test-parent-id))
         (children (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    `(,org-edna-test-sibling-one-id
                      ,org-edna-test-sibling-two-id
                      ,org-edna-test-sibling-three-id)))
         (targets (org-with-point-at current
                    (org-edna-finder/children))))
    (should (= (length targets) 3))
    (should (equal children targets))))

(ert-deftest org-edna-finder/parent ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading org-edna-test-sibling-one-id))
         (parent (list (org-edna-find-test-heading org-edna-test-parent-id)))
         (targets (org-with-point-at current
                    (org-edna-finder/parent))))
    (should (= (length targets) 1))
    (should (equal parent targets))))

(ert-deftest org-edna-relatives/from-top ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading org-edna-test-sibling-one-id))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    `(,org-edna-test-sibling-one-id)))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives 'from-top 1))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/from-bottom ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading org-edna-test-sibling-one-id))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    `(,org-edna-test-sibling-three-id)))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives 'from-bottom 1))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/forward-wrap-no-wrap ()
  (let* ((start-marker org-edna-test-sibling-one-id)
         (target-list `(,org-edna-test-sibling-two-id))
         (arg 'forward-wrap)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg 1))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/forward-wrap-wrap ()
  (let* ((start-marker org-edna-test-sibling-three-id)
         (target-list `(,org-edna-test-sibling-one-id))
         (arg 'forward-wrap)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg 1))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/forward-no-wrap-no-wrap ()
  (let* ((start-marker org-edna-test-sibling-one-id)
         (target-list `(,org-edna-test-sibling-two-id))
         (arg 'forward-no-wrap)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg 1))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/forward-no-wrap-wrap ()
  (let* ((start-marker org-edna-test-sibling-three-id)
         (target-list nil)
         (arg 'forward-no-wrap)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/backward-wrap-no-wrap ()
  (let* ((start-marker org-edna-test-sibling-three-id)
         (target-list `(,org-edna-test-sibling-two-id))
         (arg 'backward-wrap)
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg size))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/backward-wrap-wrap ()
  (let* ((start-marker org-edna-test-sibling-one-id)
         (target-list `(,org-edna-test-sibling-three-id))
         (arg 'backward-wrap)
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg size))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/backward-no-wrap-no-wrap ()
  (let* ((start-marker org-edna-test-sibling-three-id)
         (target-list `(,org-edna-test-sibling-two-id))
         (arg 'backward-no-wrap)
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg size))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/backward-no-wrap-wrap ()
  (let* ((start-marker org-edna-test-sibling-one-id)
         (target-list nil)
         (arg 'backward-no-wrap)
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg size))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/walk-up ()
  (let* ((start-marker org-edna-test-sibling-one-id)
         (target-list `(,org-edna-test-parent-id))
         (arg 'walk-up)
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg size))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/walk-up-with-self ()
  (let* ((start-marker org-edna-test-sibling-one-id)
         (target-list `(,org-edna-test-sibling-one-id))
         (arg 'walk-up-with-self)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg 1))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/walk-down ()
  (let* ((start-marker org-edna-test-parent-id)
         (target-list `(,org-edna-test-sibling-one-id))
         (arg 'walk-down)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg 1))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/walk-down-with-self ()
  (let* ((start-marker org-edna-test-parent-id)
         (target-list `(,org-edna-test-parent-id))
         (arg 'walk-down-with-self)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg 1))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/walk-down ()
  (let* ((start-marker org-edna-test-parent-id)
         (target-list `(,org-edna-test-sibling-one-id))
         (arg 'walk-down)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg 1))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/walk-down-full ()
  (let* ((start-marker org-edna-test-relative-parent-one)
         (target-list `(,org-edna-test-relative-standard-child
                        ,org-edna-test-relative-child-with-children
                        ,org-edna-test-relative-grandchild-one
                        ,org-edna-test-relative-grandchild-two
                        ,org-edna-test-relative-child-with-todo
                        ,org-edna-test-relative-commented-child
                        ,org-edna-test-relative-archived-child
                        ,org-edna-test-relative-child-with-done))
         (arg 'walk-down)
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg size))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/step-down-full ()
  (let* ((start-marker org-edna-test-relative-parent-one)
         (target-list `(,org-edna-test-relative-standard-child
                        ,org-edna-test-relative-child-with-children
                        ,org-edna-test-relative-child-with-todo
                        ,org-edna-test-relative-commented-child
                        ,org-edna-test-relative-archived-child
                        ,org-edna-test-relative-child-with-done))
         (arg 'step-down)
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg size))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/filter-todo-only ()
  (let* ((start-marker org-edna-test-relative-parent-one)
         (target-list `(,org-edna-test-relative-child-with-todo))
         (arg 'step-down)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg 'todo-only))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/filter-todo-and-done-only ()
  (let* ((start-marker org-edna-test-relative-parent-one)
         (target-list `(,org-edna-test-relative-child-with-todo
                        ,org-edna-test-relative-child-with-done))
         (arg 'step-down)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg 'todo-and-done-only))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/filter-no-comments ()
  (let* ((start-marker org-edna-test-relative-parent-one)
         (target-list `(,org-edna-test-relative-standard-child
                        ,org-edna-test-relative-child-with-children
                        ,org-edna-test-relative-child-with-todo
                        ,org-edna-test-relative-archived-child
                        ,org-edna-test-relative-child-with-done))
         (arg 'step-down)
         (filter 'no-comment)
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg filter size))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/filter-no-archive ()
  (let* ((start-marker org-edna-test-relative-parent-one)
         (target-list `(,org-edna-test-relative-standard-child
                        ,org-edna-test-relative-child-with-children
                        ,org-edna-test-relative-child-with-todo
                        ,org-edna-test-relative-commented-child
                        ,org-edna-test-relative-child-with-done))
         (arg 'step-down)
         (filter 'no-archive)
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg filter size))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/filter-has-tag ()
  (let* ((start-marker org-edna-test-relative-parent-one)
         (target-list `(,org-edna-test-relative-archived-child))
         (arg 'step-down)
         (filter "+ARCHIVE")
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg filter size))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/filter-no-tag ()
  (let* ((start-marker org-edna-test-relative-parent-one)
         (target-list `(,org-edna-test-relative-standard-child
                        ,org-edna-test-relative-child-with-children
                        ,org-edna-test-relative-child-with-todo
                        ,org-edna-test-relative-commented-child
                        ,org-edna-test-relative-child-with-done))
         (arg 'step-down)
         (filter "-ARCHIVE")
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg filter size))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/filter-matches-regexp ()
  (let* ((start-marker org-edna-test-relative-parent-one)
         (target-list `(,org-edna-test-relative-child-with-children
                        ,org-edna-test-relative-child-with-todo
                        ,org-edna-test-relative-child-with-done))
         (arg 'step-down)
         (filter "Child Heading With .*")
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg filter size))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/sort-reverse ()
  (let* ((start-marker org-edna-test-relative-parent-one)
         (target-list `(,org-edna-test-relative-child-with-done
                        ,org-edna-test-relative-archived-child
                        ,org-edna-test-relative-commented-child
                        ,org-edna-test-relative-child-with-todo
                        ,org-edna-test-relative-child-with-children
                        ,org-edna-test-relative-standard-child))
         (arg 'step-down)
         (sort 'reverse-sort)
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg sort size))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/sort-priority ()
  (let* ((start-marker org-edna-test-relative-parent-one)
         (target-list `(,org-edna-test-relative-child-with-todo
                        ,org-edna-test-relative-archived-child
                        ,org-edna-test-relative-child-with-children
                        ,org-edna-test-relative-commented-child
                        ,org-edna-test-relative-standard-child
                        ,org-edna-test-relative-child-with-done))
         (arg 'step-down)
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list))
         (targets ))
    (should (equal siblings
                   (org-with-point-at current
                     (org-edna-finder/relatives arg 'priority-up size))))
    (should (equal (nreverse siblings)
                   (org-with-point-at current
                     (org-edna-finder/relatives arg 'priority-down size))))))

(ert-deftest org-edna-relatives/sort-effort ()
  (let* ((start-marker org-edna-test-relative-parent-one)
         (target-list `(,org-edna-test-relative-child-with-done
                        ,org-edna-test-relative-commented-child
                        ,org-edna-test-relative-archived-child
                        ,org-edna-test-relative-child-with-children
                        ,org-edna-test-relative-child-with-todo
                        ,org-edna-test-relative-standard-child))
         (arg 'step-down)
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list)))
    (should (equal siblings
                   (org-with-point-at current
                     (org-edna-finder/relatives arg 'effort-up size))))
    (should (equal (nreverse siblings)
                   (org-with-point-at current
                     (org-edna-finder/relatives arg 'effort-down size))))))

(ert-deftest org-edna-relatives/sort-scheduled ()
  (let* ((start-marker org-edna-test-relative-parent-one)
         (target-list `(,org-edna-test-relative-child-with-todo
                        ,org-edna-test-relative-child-with-done
                        ,org-edna-test-relative-commented-child
                        ,org-edna-test-relative-child-with-children
                        ,org-edna-test-relative-standard-child
                        ,org-edna-test-relative-archived-child))
         (arg 'step-down)
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list)))
    (should (equal siblings
                   (org-with-point-at current
                     (org-edna-finder/relatives arg 'scheduled-up size))))
    (should (equal (nreverse siblings)
                   (org-with-point-at current
                     (org-edna-finder/relatives arg 'scheduled-down size))))))

(ert-deftest org-edna-relatives/sort-deadline ()
  (let* ((start-marker org-edna-test-relative-parent-one)
         (target-list `(,org-edna-test-relative-commented-child
                        ,org-edna-test-relative-standard-child
                        ,org-edna-test-relative-child-with-done
                        ,org-edna-test-relative-child-with-children
                        ,org-edna-test-relative-archived-child
                        ,org-edna-test-relative-child-with-todo))
         (arg 'step-down)
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list)))
    (should (equal siblings
                   (org-with-point-at current
                     (org-edna-finder/relatives arg 'deadline-up size))))
    (should (equal (nreverse siblings)
                   (org-with-point-at current
                     (org-edna-finder/relatives arg 'deadline-down size))))))

(ert-deftest org-edna-relatives/sort-timestamp ()
  (let* ((start-marker org-edna-test-relative-parent-one)
         (target-list `(,org-edna-test-relative-child-with-todo
                        ,org-edna-test-relative-child-with-done
                        ,org-edna-test-relative-commented-child
                        ,org-edna-test-relative-child-with-children
                        ,org-edna-test-relative-standard-child
                        ,org-edna-test-relative-archived-child))
         (arg 'step-down)
         (size (length target-list))
         (org-agenda-files `(,org-edna-test-file))
         (current (org-edna-find-test-heading start-marker))
         (siblings (mapcar
                    (lambda (uuid) (org-edna-find-test-heading uuid))
                    target-list)))
    (should (equal siblings
                   (org-with-point-at current
                     (org-edna-finder/relatives arg 'timestamp-up size))))
    (should (equal (nreverse siblings)
                   (org-with-point-at current
                     (org-edna-finder/relatives arg 'timestamp-down size))))))

(ert-deftest org-edna-cache/no-entry ()
  (let* ((org-edna-finder-use-cache t)
         (org-edna--finder-cache (make-hash-table :test 'equal)))
    ;; Empty, so `org-edna--get-cache-entry' should return nil.
    (should (not (org-edna--get-cache-entry 'org-edna-finder/match '("test&1"))))))

(ert-deftest org-edna-cache/added-new-entry ()
  (let* ((org-edna-finder-use-cache t)
         (org-edna--finder-cache (make-hash-table :test 'equal))
         (org-agenda-files `(,org-edna-test-file))
         (targets (org-edna--handle-finder 'org-edna-finder/match '("test&1"))))
    (should (= (length targets) 2))
    (should (string-equal (org-edna-heading (nth 0 targets)) "Tagged Heading 1"))
    (should (string-equal (org-edna-heading (nth 1 targets)) "Tagged Heading 2"))
    (should (= (hash-table-count org-edna--finder-cache) 1))
    ;; Verify that we've got a valid cache entry.
    (should (org-edna--get-cache-entry 'org-edna-finder/match '("test&1")))
    ;; Verify that any other signature returns nil.
    (should (not (org-edna--get-cache-entry 'org-edna-finder/match '("test&2"))))
    (let ((cache-entry (gethash (make-org-edna--finder-input :func-sym 'org-edna-finder/match
                                                             :args '("test&1"))
                                org-edna--finder-cache)))
      (should cache-entry)
      (should (equal (org-edna--finder-cache-entry-input cache-entry)
                     (make-org-edna--finder-input :func-sym 'org-edna-finder/match
                                                  :args '("test&1"))))
      (should (equal (org-edna--finder-cache-entry-results cache-entry)
                     targets)))))

(ert-deftest org-edna-cache/timed-out ()
  (let* ((org-edna-finder-use-cache t)
         (org-edna--finder-cache (make-hash-table :test 'equal))
         (org-edna-finder-cache-timeout 1) ;; Set timeout to 1 second
         (org-agenda-files `(,org-edna-test-file))
         (targets (org-edna--handle-finder 'org-edna-finder/match '("test&1")))
         ;; Time increment required to invalidate a cache entry
         (time-increment `(0 ,org-edna-finder-cache-timeout)))
    (should (org-edna--get-cache-entry 'org-edna-finder/match '("test&1")))
    ;; Validate the cache entry
    (let ((cache-entry (gethash (make-org-edna--finder-input :func-sym 'org-edna-finder/match
                                                             :args '("test&1"))
                                org-edna--finder-cache)))
      (should cache-entry)
      (should (equal (org-edna--finder-cache-entry-input cache-entry)
                     (make-org-edna--finder-input :func-sym 'org-edna-finder/match
                                                  :args '("test&1"))))
      (should (equal (org-edna--finder-cache-entry-results cache-entry)
                     targets))
      ;; Override `current-time' so we can get a deterministic value
      ;; The value invalidates the cache entry
      (cl-letf* (((symbol-function 'current-time)
                  (lambda () (time-add (org-edna--finder-cache-entry-last-run-time cache-entry)
                                  time-increment))))
        (should (not (org-edna--get-cache-entry 'org-edna-finder/match '("test&1"))))))))


;;; Actions

(ert-deftest org-edna-action/todo-test ()
  (org-edna-with-test-heading "0d491588-7da3-43c5-b51a-87fbd34f79f7"
    (org-edna-action/todo! nil "DONE")
    (should (string-equal (org-entry-get nil "TODO") "DONE"))
    (org-edna-action/todo! nil "TODO")
    (should (string-equal (org-entry-get nil "TODO") "TODO"))
    (org-edna-action/todo! nil 'DONE)
    (should (string-equal (org-entry-get nil "TODO") "DONE"))
    (org-edna-action/todo! nil 'TODO)
    (should (string-equal (org-entry-get nil "TODO") "TODO"))))

;; Scheduled

(ert-deftest org-edna-action-scheduled/wkdy ()
  (org-edna-with-test-heading "0d491588-7da3-43c5-b51a-87fbd34f79f7"
    (org-edna-action/scheduled! nil "Mon")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-17 Mon>"))
    (org-edna-action/scheduled! nil 'rm)
    (should (not (org-entry-get nil "SCHEDULED")))
    (org-edna-action/scheduled! nil "Mon 9:00")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-17 Mon 09:00>"))
    (org-edna-action/scheduled! nil 'rm)
    (should (not (org-entry-get nil "SCHEDULED")))))

(ert-deftest org-edna-action-scheduled/cp ()
  (org-edna-with-test-heading "0d491588-7da3-43c5-b51a-87fbd34f79f7"
    (let* ((source (org-edna-find-test-heading "97e6b0f0-40c4-464f-b760-6e5ca9744eb5"))
           (pairs '((cp . rm) (copy . remove) ("cp" . "rm") ("copy" . "remove"))))
      (dolist (pair pairs)
        (org-edna-action/scheduled! source (car pair))
        (should (string-equal (org-entry-get nil "SCHEDULED")
                              "<2000-01-15 Sat 00:00>"))
        (org-edna-action/scheduled! source (cdr pair))
        (should (not (org-entry-get nil "SCHEDULED")))))))

(ert-deftest org-edna-action-scheduled/inc ()
  (org-edna-with-test-heading "97e6b0f0-40c4-464f-b760-6e5ca9744eb5"
    ;; Time starts at Jan 15, 2000
    (org-edna-action/scheduled! nil "2000-01-15 Sat 00:00")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-15 Sat 00:00>"))
    ;; Increment 1 minute
    (org-edna-action/scheduled! nil "+1M")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-15 Sat 00:01>"))
    ;; Decrement 1 minute
    (org-edna-action/scheduled! nil "-1M")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-15 Sat 00:00>"))
    ;; +1 day
    (org-edna-action/scheduled! nil "+1d")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-16 Sun 00:00>"))
    ;; +1 hour from current time
    (org-edna-action/scheduled! nil "++1h")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-15 Sat 01:00>"))
    ;; Back to Saturday
    (org-edna-action/scheduled! nil "2000-01-15 Sat 00:00")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-15 Sat 00:00>"))
    ;; -1 day to Friday
    (org-edna-action/scheduled! nil "-1d")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-14 Fri 00:00>"))
    ;; Increment two days to the next weekday
    (org-edna-action/scheduled! nil "+2wkdy")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-17 Mon 00:00>"))
    ;; Increment one day, expected to land on a weekday
    (org-edna-action/scheduled! nil "+1wkdy")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-18 Tue 00:00>"))
    ;; Move forward 8 days, then backward until we find a weekend
    (org-edna-action/scheduled! nil "+8d -wknd")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-23 Sun 00:00>"))
    ;; Move forward one week, then forward until we find a weekday
    ;; (org-edna-action/scheduled! nil "+1w +wkdy")
    ;; (should (string-equal (org-entry-get nil "SCHEDULED")
    ;;                       "<2000-01-31 Mon 00:00>"))
    ;; Back to Saturday for other tests
    (org-edna-action/scheduled! nil "2000-01-15 Sat 00:00")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-15 Sat 00:00>"))))

(ert-deftest org-edna-action-scheduled/landing ()
  "Test landing arguments to scheduled increment."
  (org-edna-with-test-heading "97e6b0f0-40c4-464f-b760-6e5ca9744eb5"
    ;; Time starts at Jan 15, 2000
    (org-edna-action/scheduled! nil "2000-01-15 Sat 00:00")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-15 Sat 00:00>"))
    ;; Move forward 10 days, then backward until we find a weekend
    (org-edna-action/scheduled! nil "+10d -wknd")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-23 Sun 00:00>"))
    ;; Move forward one week, then forward until we find a weekday
    (org-edna-action/scheduled! nil "+1w +wkdy")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-31 Mon 00:00>"))
    ;; Back to Saturday for other tests
    (org-edna-action/scheduled! nil "2000-01-15 Sat 00:00")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-15 Sat 00:00>"))))

(ert-deftest org-edna-action-scheduled/landing-no-hour ()
  "Test landing arguments to scheduled increment, without hour."
  (org-edna-with-test-heading "caf27724-0887-4565-9765-ed2f1edcfb16"
    ;; Time starts at Jan 1, 2017
    (org-edna-action/scheduled! nil "2017-01-01 Sun")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2017-01-01 Sun>"))
    ;; Move forward 10 days, then backward until we find a weekend
    (org-edna-action/scheduled! nil "+10d -wknd")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2017-01-08 Sun>"))
    ;; Move forward one week, then forward until we find a weekday
    (org-edna-action/scheduled! nil "+1w +wkdy")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2017-01-16 Mon>"))
    ;; Back to Saturday for other tests
    (org-edna-action/scheduled! nil "2017-01-01 Sun")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2017-01-01 Sun>"))))

(ert-deftest org-edna-action-scheduled/float ()
  (org-edna-with-test-heading "97e6b0f0-40c4-464f-b760-6e5ca9744eb5"
    ;; Time starts at Jan 15, 2000
    (org-edna-action/scheduled! nil "2000-01-15 Sat 00:00")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-01-15 Sat 00:00>"))
    ;; The third Tuesday of next month (Feb 15th)
    (org-edna-action/scheduled! nil "float 3 Tue")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-02-15 Tue 00:00>"))
    ;; The second Friday of the following May (May 12th)
    (org-edna-action/scheduled! nil "float 2 5 May")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-05-12 Fri 00:00>"))
    ;; Move forward to the second Wednesday of the next month (June 14th)
    (org-edna-action/scheduled! nil "float 2 Wednesday")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-06-14 Wed 00:00>"))
    ;; Move forward to the first Thursday in the following Jan (Jan 4th, 2001)
    (org-edna-action/scheduled! nil "float 1 4 Jan")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2001-01-04 Thu 00:00>"))
    ;; The fourth Monday in Feb, 2000 (Feb 28th)
    (org-edna-action/scheduled! nil "float ++4 monday")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-02-28 Mon 00:00>"))
    ;; The second Monday after Mar 12th, 2000 (Mar 20th)
    (org-edna-action/scheduled! nil "float 2 monday Mar 12")
    (should (string-equal (org-entry-get nil "SCHEDULED")
                          "<2000-03-20 Mon 00:00>"))))

(ert-deftest org-edna-action-deadline/wkdy ()
  (org-edna-with-test-heading "0d491588-7da3-43c5-b51a-87fbd34f79f7"
    (org-edna-action/deadline! nil "Mon")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-17 Mon>"))
    (org-edna-action/deadline! nil 'rm)
    (should (not (org-entry-get nil "DEADLINE")))
    (org-edna-action/deadline! nil "Mon 9:00")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-17 Mon 09:00>"))
    (org-edna-action/deadline! nil 'rm)
    (should (not (org-entry-get nil "DEADLINE")))))

(ert-deftest org-edna-action-deadline/cp ()
  (org-edna-with-test-heading "0d491588-7da3-43c5-b51a-87fbd34f79f7"
    (let* ((source (org-edna-find-test-heading "97e6b0f0-40c4-464f-b760-6e5ca9744eb5"))
           (pairs '((cp . rm) (copy . remove) ("cp" . "rm") ("copy" . "remove"))))
      (dolist (pair pairs)
        (org-edna-action/deadline! source (car pair))
        (should (string-equal (org-entry-get nil "DEADLINE")
                              "<2000-01-15 Sat 00:00>"))
        (org-edna-action/deadline! source (cdr pair))
        (should (not (org-entry-get nil "DEADLINE")))))))

(ert-deftest org-edna-action-deadline/inc ()
  (org-edna-with-test-heading "97e6b0f0-40c4-464f-b760-6e5ca9744eb5"
    ;; Time starts at Jan 15, 2000
    (org-edna-action/deadline! nil "2000-01-15 Sat 00:00")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-15 Sat 00:00>"))
    ;; Increment 1 minute
    (org-edna-action/deadline! nil "+1M")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-15 Sat 00:01>"))
    ;; Decrement 1 minute
    (org-edna-action/deadline! nil "-1M")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-15 Sat 00:00>"))
    ;; +1 day
    (org-edna-action/deadline! nil "+1d")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-16 Sun 00:00>"))
    ;; +1 hour from current time
    (org-edna-action/deadline! nil "++1h")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-15 Sat 01:00>"))
    ;; Back to Saturday
    (org-edna-action/deadline! nil "2000-01-15 Sat 00:00")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-15 Sat 00:00>"))
    ;; -1 day to Friday
    (org-edna-action/deadline! nil "-1d")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-14 Fri 00:00>"))
    ;; Increment two days to the next weekday
    (org-edna-action/deadline! nil "+2wkdy")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-17 Mon 00:00>"))
    ;; Increment one day, expected to land on a weekday
    (org-edna-action/deadline! nil "+1wkdy")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-18 Tue 00:00>"))
    ;; Move forward 8 days, then backward until we find a weekend
    (org-edna-action/deadline! nil "+8d -wknd")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-23 Sun 00:00>"))
    ;; Move forward one week, then forward until we find a weekday
    ;; (org-edna-action/deadline! nil "+1w +wkdy")
    ;; (should (string-equal (org-entry-get nil "DEADLINE")
    ;;                       "<2000-01-31 Mon 00:00>"))
    ;; Back to Saturday for other tests
    (org-edna-action/deadline! nil "2000-01-15 Sat 00:00")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-15 Sat 00:00>"))))

(ert-deftest org-edna-action-deadline/landing ()
  "Test landing arguments to deadline increment."
  (org-edna-with-test-heading "97e6b0f0-40c4-464f-b760-6e5ca9744eb5"
    ;; Time starts at Jan 15, 2000
    (org-edna-action/deadline! nil "2000-01-15 Sat 00:00")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-15 Sat 00:00>"))
    ;; Move forward 10 days, then backward until we find a weekend
    (org-edna-action/deadline! nil "+10d -wknd")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-23 Sun 00:00>"))
    ;; Move forward one week, then forward until we find a weekday
    (org-edna-action/deadline! nil "+1w +wkdy")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-31 Mon 00:00>"))
    ;; Back to Saturday for other tests
    (org-edna-action/deadline! nil "2000-01-15 Sat 00:00")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-15 Sat 00:00>"))))

(ert-deftest org-edna-action-deadline/landing-no-hour ()
  "Test landing arguments to deadline increment, without hour."
  (org-edna-with-test-heading "caf27724-0887-4565-9765-ed2f1edcfb16"
    ;; Time starts at Jan 1, 2017
    (org-edna-action/deadline! nil "2017-01-01 Sun")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2017-01-01 Sun>"))
    ;; Move forward 10 days, then backward until we find a weekend
    (org-edna-action/deadline! nil "+10d -wknd")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2017-01-08 Sun>"))
    ;; Move forward one week, then forward until we find a weekday
    (org-edna-action/deadline! nil "+1w +wkdy")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2017-01-16 Mon>"))
    ;; Back to Saturday for other tests
    (org-edna-action/deadline! nil "2017-01-01 Sun")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2017-01-01 Sun>"))))

(ert-deftest org-edna-action-deadline/float ()
  (org-edna-with-test-heading "97e6b0f0-40c4-464f-b760-6e5ca9744eb5"
    ;; Time starts at Jan 15, 2000
    (org-edna-action/deadline! nil "2000-01-15 Sat 00:00")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-01-15 Sat 00:00>"))
    ;; The third Tuesday of next month (Feb 15th)
    (org-edna-action/deadline! nil "float 3 Tue")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-02-15 Tue 00:00>"))
    ;; The second Friday of the following May (May 12th)
    (org-edna-action/deadline! nil "float 2 5 May")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-05-12 Fri 00:00>"))
    ;; Move forward to the second Wednesday of the next month (June 14th)
    (org-edna-action/deadline! nil "float 2 Wednesday")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-06-14 Wed 00:00>"))
    ;; Move forward to the first Thursday in the following Jan (Jan 4th, 2001)
    (org-edna-action/deadline! nil "float 1 4 Jan")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2001-01-04 Thu 00:00>"))
    ;; The fourth Monday in Feb, 2000 (Feb 28th)
    (org-edna-action/deadline! nil "float ++4 monday")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-02-28 Mon 00:00>"))
    ;; The second Monday after Mar 12th, 2000 (Mar 20th)
    (org-edna-action/deadline! nil "float 2 monday Mar 12")
    (should (string-equal (org-entry-get nil "DEADLINE")
                          "<2000-03-20 Mon 00:00>"))))

(ert-deftest org-edna-action-tag ()
  (org-edna-with-test-heading org-edna-test-id-heading-one
    (org-edna-action/tag! nil "tag")
    (should (equal (org-get-tags) '("tag")))
    (org-edna-action/tag! nil "")
    (should (equal (org-get-tags) nil))))

(ert-deftest org-edna-action-property ()
  (org-edna-with-test-heading org-edna-test-id-heading-one
    (org-edna-action/set-property! nil "TEST" "1")
    (should (equal (org-entry-get nil "TEST") "1"))
    (org-edna-action/delete-property! nil "TEST")
    (should-not (org-entry-get nil "TEST"))))

(ert-deftest org-edna-action-property/inc-dec ()
  (org-edna-with-test-heading org-edna-test-id-heading-one
    (org-edna-action/set-property! nil "TEST" "1")
    (should (equal (org-entry-get nil "TEST") "1"))
    (org-edna-action/set-property! nil "TEST" 'inc)
    (should (equal (org-entry-get nil "TEST") "2"))
    (org-edna-action/set-property! nil "TEST" 'dec)
    (should (equal (org-entry-get nil "TEST") "1"))
    (org-edna-action/delete-property! nil "TEST")
    (should-not (org-entry-get nil "TEST"))
    (should-error (org-edna-action/set-property! nil "TEST" 'inc))
    (should-error (org-edna-action/set-property! nil "TEST" 'dec))
    (org-edna-action/set-property! nil "TEST" "a")
    (should (equal (org-entry-get nil "TEST") "a"))
    (should-error (org-edna-action/set-property! nil "TEST" 'inc))
    (should-error (org-edna-action/set-property! nil "TEST" 'dec))
    (org-edna-action/delete-property! nil "TEST")
    (should-not (org-entry-get nil "TEST"))))

(ert-deftest org-edna-action-property/next-prev ()
  (org-edna-with-test-heading org-edna-test-id-heading-one
    (org-edna-action/set-property! nil "TEST" "a")
    (should (equal (org-entry-get nil "TEST") "a"))
    (should-error (org-edna-action/set-property! nil "TEST" 'next))
    (should-error (org-edna-action/set-property! nil "TEST" 'prev))
    (should-error (org-edna-action/set-property! nil "TEST" 'previous))
    (org-edna-action/delete-property! nil "TEST")
    (should-not (org-entry-get nil "TEST"))
    ;; Test moving forwards
    (org-edna-action/set-property! nil "COUNTER" "a")
    (should (equal (org-entry-get nil "COUNTER") "a"))
    (org-edna-action/set-property! nil "COUNTER" 'next)
    (should (equal (org-entry-get nil "COUNTER") "b"))
    ;; Test moving forwards past the last one
    (org-edna-action/set-property! nil "COUNTER" "d")
    (should (equal (org-entry-get nil "COUNTER") "d"))
    (org-edna-action/set-property! nil "COUNTER" 'next)
    (should (equal (org-entry-get nil "COUNTER") "a"))
    ;; Test moving backwards past the first one
    (org-edna-action/set-property! nil "COUNTER" 'prev)
    (should (equal (org-entry-get nil "COUNTER") "d"))
    ;; Test moving backwards normally
    (org-edna-action/set-property! nil "COUNTER" 'previous)
    (should (equal (org-entry-get nil "COUNTER") "c"))
    (org-edna-action/delete-property! nil "COUNTER")
    (should-not (org-entry-get nil "COUNTER"))))

(ert-deftest org-edna-action-clock ()
  (org-edna-with-test-heading org-edna-test-id-heading-one
    (org-edna-action/clock-in! nil)
    (should (org-clocking-p))
    (should (equal org-clock-hd-marker (point-marker)))
    (org-edna-action/clock-out! nil)
    (should-not (org-clocking-p))))

(ert-deftest org-edna-action-priority ()
  (org-edna-with-test-heading org-edna-test-id-heading-one
    (let ((org-lowest-priority  ?C)
          (org-highest-priority ?A)
          (org-default-priority ?B))
      (org-edna-action/set-priority! nil "A")
      (should (equal (org-entry-get nil "PRIORITY") "A"))
      (org-edna-action/set-priority! nil 'down)
      (should (equal (org-entry-get nil "PRIORITY") "B"))
      (org-edna-action/set-priority! nil 'up)
      (should (equal (org-entry-get nil "PRIORITY") "A"))
      (org-edna-action/set-priority! nil ?C)
      (should (equal (org-entry-get nil "PRIORITY") "C"))
      (org-edna-action/set-priority! nil 'remove)
      (should (equal (org-entry-get nil "PRIORITY") "B")))))

(ert-deftest org-edna-action-effort ()
  (org-edna-with-test-heading org-edna-test-id-heading-one
    (org-edna-action/set-effort! nil "0:01")
    (should (equal (org-entry-get nil "EFFORT") "0:01"))
    (org-edna-action/set-effort! nil 'increment)
    (should (equal (org-entry-get nil "EFFORT") "0:02"))
    (org-entry-delete nil "EFFORT")))

(ert-deftest org-edna-action-archive ()
  (org-edna-with-test-heading org-edna-test-archive-heading
    (let* ((org-archive-save-context-info '(todo))
           ;; Archive it to the same location
           (org-archive-location "::** Archive")
           ;; We're non-interactive, so no prompt.
           (org-edna-prompt-for-archive nil))
      (org-edna-action/archive! nil)
      (should (equal (org-entry-get nil "ARCHIVE_TODO") "TODO"))
      (org-entry-delete nil "ARCHIVE_TODO"))))

(ert-deftest org-edna-action-chain ()
  (org-edna-test-setup
    (let ((old-pom (org-edna-find-test-heading org-edna-test-id-heading-one))
          (new-pom (org-edna-find-test-heading org-edna-test-id-heading-two)))
      (org-edna-protect-test-file
        (org-entry-put old-pom "TEST" "1")
        (org-with-point-at new-pom
          (org-edna-action/chain! old-pom "TEST")
          (should (equal (org-entry-get nil "TEST") "1")))
        (org-entry-delete old-pom "TEST")
        (org-entry-delete new-pom "TEST")))))


;;; Conditions

(defun org-edna-test-condition-form (func-sym pom-true pom-false block-true block-false &rest args)
  (org-edna-test-setup
    (let* ((block-true (or block-true (org-with-point-at pom-true (org-get-heading))))
           (block-false (or block-false (org-with-point-at pom-false (org-get-heading)))))
      (org-with-point-at pom-true
        (should-not (apply func-sym t args))
        (should     (equal (apply func-sym nil args) block-true)))
      (org-with-point-at pom-false
        (should     (equal (apply func-sym t args) block-false))
        (should-not (apply func-sym nil args))))))

(ert-deftest org-edna-condition-done ()
  (let* ((pom-done (org-edna-find-test-heading org-edna-test-id-heading-four))
         (pom-todo (org-edna-find-test-heading org-edna-test-id-heading-one))
         (block-done (org-with-point-at pom-done (org-get-heading)))
         (block-todo (org-with-point-at pom-todo (org-get-heading))))
    (org-edna-test-condition-form 'org-edna-condition/done?
                                  pom-done pom-todo
                                  block-done block-todo)))

(ert-deftest org-edna-condition-todo-state-string ()
  (let* ((pom-done (org-edna-find-test-heading org-edna-test-id-heading-four))
         (pom-todo (org-edna-find-test-heading org-edna-test-id-heading-one))
         (block-done (org-with-point-at pom-done (org-get-heading)))
         (block-todo (org-with-point-at pom-todo (org-get-heading))))
    (org-edna-test-condition-form 'org-edna-condition/todo-state?
                                  pom-todo pom-done
                                  block-todo block-done
                                  "TODO")))

(ert-deftest org-edna-condition-todo-state-symbol ()
  (let* ((pom-done (org-edna-find-test-heading org-edna-test-id-heading-four))
         (pom-todo (org-edna-find-test-heading org-edna-test-id-heading-one))
         (block-done (org-with-point-at pom-done (org-get-heading)))
         (block-todo (org-with-point-at pom-todo (org-get-heading))))
    (org-edna-test-condition-form 'org-edna-condition/todo-state?
                                  pom-todo pom-done
                                  block-todo block-done
                                  'TODO)))

(ert-deftest org-edna-condition-headings ()
  (pcase-let* ((`(,pom-headings ,block-headings)
                (with-current-buffer (find-file-noselect org-edna-test-file)
                  (list (point-min-marker) (buffer-name))))
               (`(,pom-no-headings ,block-no-headings)
                (with-current-buffer (find-file-noselect org-edna-tests-el)
                  (list (point-min-marker) (buffer-name)))))
    (org-edna-test-condition-form 'org-edna-condition/headings?
                                  pom-headings pom-no-headings
                                  block-headings block-no-headings)))

(ert-deftest org-edna-condition-variable-set ()
  (let* ((temp-var t))
    (should-not (org-edna-condition/variable-set? t 'temp-var t))
    (should     (equal (org-edna-condition/variable-set? nil 'temp-var t)
                       "temp-var == t"))
    (should     (equal (org-edna-condition/variable-set? t 'temp-var nil)
                       "temp-var != nil"))
    (should-not (org-edna-condition/variable-set? nil 'temp-var nil))))

(ert-deftest org-edna-condition-has-property ()
  (let* ((pom-true (org-edna-find-test-heading org-edna-test-id-heading-four))
         (pom-false (org-edna-find-test-heading org-edna-test-id-heading-one))
         (block-true (org-with-point-at pom-true (org-get-heading)))
         (block-false (org-with-point-at pom-false (org-get-heading))))
    (org-edna-test-condition-form 'org-edna-condition/has-property?
                                  pom-true pom-false
                                  block-true block-false
                                  "ID" org-edna-test-id-heading-four)))

(ert-deftest org-edna-condition-re-search ()
  (pcase-let* ((case-fold-search nil)
               (string "require")
               (`(,pom-true ,block-true)
                (with-current-buffer (find-file-noselect org-edna-tests-el)
                  (list (point-min-marker)
                        (format "Found %s in %s" string (buffer-name)))))
               (`(,pom-false ,block-false)
                (with-current-buffer (find-file-noselect org-edna-test-file)
                  (list (point-min-marker)
                        (format "Did Not Find %s in %s" string (buffer-name))))))
    (org-edna-test-condition-form 'org-edna-condition/re-search?
                                  pom-true pom-false
                                  block-true block-false
                                  string)))

(ert-deftest org-edna-condition/has-tags ()
  (let* ((pom-true (org-edna-find-test-heading "0fa0d4dd-40f2-4251-a558-4c6e2898c2df"))
         (pom-false (org-edna-find-test-heading org-edna-test-id-heading-one))
         (block-true (org-with-point-at pom-true (org-get-heading)))
         (block-false (org-with-point-at pom-false (org-get-heading))))
    (org-edna-test-condition-form 'org-edna-condition/has-tags?
                                  pom-true pom-false
                                  block-true block-false
                                  "test")))

(ert-deftest org-edna-condition/matches-tags ()
  (org-edna-test-condition-form
   'org-edna-condition/matches?
   (org-edna-find-test-heading "0fa0d4dd-40f2-4251-a558-4c6e2898c2df")
   (org-edna-find-test-heading org-edna-test-id-heading-one)
   nil nil
   "1&test")
  (org-edna-test-condition-form
   'org-edna-condition/matches?
   (org-edna-find-test-heading org-edna-test-id-heading-four)
   (org-edna-find-test-heading "0fa0d4dd-40f2-4251-a558-4c6e2898c2df")
   nil nil
   "TODO==\"DONE\""))


;;; Consideration

(ert-deftest org-edna-consideration/any ()
  (let ((blocks-all-blocking `("a" "c" "b"))
        (blocks-some-blocking `("a" nil "b"))
        (blocks-no-blocking `(nil nil nil)))
    (should (string-equal (org-edna-handle-consideration 'any blocks-all-blocking) "a"))
    (should (string-equal (org-edna-handle-consideration 'any blocks-some-blocking) "a"))
    (should (not (org-edna-handle-consideration 'any blocks-no-blocking)))))

(ert-deftest org-edna-consideration/all ()
  (let ((blocks-all-blocking `("a" "c" "b"))
        (blocks-some-blocking `(nil "c" nil))
        (blocks-no-blocking `(nil nil nil)))
    (should (string-equal (org-edna-handle-consideration 'all blocks-all-blocking) "a"))
    (should (not (org-edna-handle-consideration 'all blocks-some-blocking)))
    (should (not (org-edna-handle-consideration 'all blocks-no-blocking)))))

(ert-deftest org-edna-consideration/integer ()
  (let ((blocks-all-blocking `("a" "c" "b"))
        (blocks-some-blocking `("a" nil "b"))
        (blocks-no-blocking `(nil nil nil)))
    (should (string-equal (org-edna-handle-consideration 2 blocks-all-blocking) "a"))
    (should (string-equal (org-edna-handle-consideration 2 blocks-some-blocking) "a"))
    (should (not (org-edna-handle-consideration 2 blocks-no-blocking)))))

(ert-deftest org-edna-consideration/float ()
  (let ((blocks-all-blocking `("a" "c" "b"))
        (blocks-some-blocking `("a" nil "b"))
        (blocks-no-blocking `(nil nil nil)))
    (should (string-equal (org-edna-handle-consideration 0.25 blocks-all-blocking) "a"))
    (should (string-equal (org-edna-handle-consideration 0.25 blocks-some-blocking) "a"))
    (should (not (org-edna-handle-consideration 0.25 blocks-no-blocking)))))


;;; Full Run-through Tests from the Documentation

(defmacro org-edna-doc-test-setup (heading-id &rest body)
  (declare (indent 1))
  `(org-edna-with-test-heading ,heading-id
     (save-restriction
       ;; Only allow operating on the current tree
       (org-narrow-to-subtree)
       ;; Show the entire subtree
       (outline-show-all)
       ,@body)))

(ert-deftest org-edna-doc-test/ancestors ()
  (org-edna-doc-test-setup "24a0c3bb-7e69-4e9e-bb98-5aba2ff17bb1"
    (pcase-let* ((`(,heading1-pom ,heading2-pom ,heading3-pom ,heading4-pom ,heading5-pom)
                  (org-edna-test-children-marks)))
      ;; Verify that we can't change the TODO state to DONE
      (should (org-edna-test-check-block heading5-pom "Initial state of heading 5"))
      ;; Change the state at 4 to DONE
      (org-edna-test-mark-done heading4-pom)
      ;; Verify that ALL ancestors need to be changed
      (should (org-edna-test-check-block heading5-pom "Heading 5 after parent changed"))
      (org-edna-test-mark-done heading1-pom heading3-pom)
      ;; Only need 1, 3, and 4 to change 5
      (should (not (org-edna-test-check-block heading5-pom
                                            "Heading 5 after all parents changed")))
      ;; Change the state back to TODO on all of them
      (org-edna-test-mark-todo heading1-pom heading3-pom heading4-pom heading5-pom))))

(ert-deftest org-edna-doc-test/ancestors-cache ()
  (let ((org-edna-finder-use-cache t))
    (org-edna-doc-test-setup "24a0c3bb-7e69-4e9e-bb98-5aba2ff17bb1"
      (pcase-let* ((`(,heading1-pom ,heading2-pom ,heading3-pom ,heading4-pom ,heading5-pom)
                    (org-edna-test-children-marks)))
        ;; Verify that we can't change the TODO state to DONE
        (should (org-edna-test-check-block heading5-pom "Initial state of heading 5"))
        ;; Change the state at 4 to DONE
        (org-edna-test-mark-done heading4-pom)
        ;; Verify that ALL ancestors need to be changed
        (should (org-edna-test-check-block heading5-pom "Heading 5 after parent changed"))
        (org-edna-test-mark-done heading1-pom heading3-pom)
        ;; Only need 1, 3, and 4 to change 5
        (should (not (org-edna-test-check-block heading5-pom
                                              "Heading 5 after all parents changed")))
        ;; Change the state back to TODO on all of them
        (org-edna-test-mark-todo heading1-pom heading3-pom heading4-pom heading5-pom)))))

(ert-deftest org-edna-doc-test/descendants ()
  (org-edna-doc-test-setup "cc18dc74-00e8-4081-b46f-e36800041fe7"
    (pcase-let* ((`(,heading1-pom ,heading2-pom ,heading3-pom ,heading4-pom ,heading5-pom)
                  (org-edna-test-children-marks)))
      (should (org-edna-test-check-block heading1-pom "Heading 1 initial state"))
      ;; Change the state at 2 to DONE
      (org-edna-test-mark-done heading2-pom)
      ;; Verify that ALL descendants need to be changed
      (should (org-edna-test-check-block heading1-pom "Heading 1 after changing 2"))
      ;; Try 3
      (org-edna-test-mark-done heading3-pom)
      ;; Verify that ALL descendants need to be changed
      (should (org-edna-test-check-block heading1-pom "Heading 1 after changing 3"))
      ;; Try 4
      (org-edna-test-mark-done heading4-pom)
      ;; Verify that ALL descendants need to be changed
      (should (org-edna-test-check-block heading1-pom "Heading 1 after changing 4"))
      ;; Try 5
      (org-edna-test-mark-done heading5-pom)
      ;; Verify that ALL descendants need to be changed
      (should (not (org-edna-test-check-block heading1-pom "Heading 1 after changing 5"))))))

(ert-deftest org-edna-doc-test/descendants-cache ()
  (let ((org-edna-finder-use-cache t))
    (org-edna-doc-test-setup "cc18dc74-00e8-4081-b46f-e36800041fe7"
      (pcase-let* ((`(,heading1-pom ,heading2-pom ,heading3-pom ,heading4-pom ,heading5-pom)
                    (org-edna-test-children-marks)))
        (should (org-edna-test-check-block heading1-pom "Heading 1 initial state"))
        ;; Change the state at 2 to DONE
        (org-edna-test-mark-done heading2-pom)
        ;; Verify that ALL descendants need to be changed
        (should (org-edna-test-check-block heading1-pom "Heading 1 after changing 2"))
        ;; Try 3
        (org-edna-test-mark-done heading3-pom)
        ;; Verify that ALL descendants need to be changed
        (should (org-edna-test-check-block heading1-pom "Heading 1 after changing 3"))
        ;; Try 4
        (org-edna-test-mark-done heading4-pom)
        ;; Verify that ALL descendants need to be changed
        (should (org-edna-test-check-block heading1-pom "Heading 1 after changing 4"))
        ;; Try 5
        (org-edna-test-mark-done heading5-pom)
        ;; Verify that ALL descendants need to be changed
        (should (not (org-edna-test-check-block heading1-pom "Heading 1 after changing 5")))))))

(ert-deftest org-edna-doc-test/laundry ()
  "Test for the \"laundry\" example in the documentation."
  (org-edna-doc-test-setup "e57ce099-9f37-47f4-a6bb-61a84eb1fbbe"
    (pcase-let* ((`(,heading1-pom ,heading2-pom ,heading3-pom ,heading4-pom)
                  (org-edna-test-children-marks)))
      ;; Verify that headings 2, 3, and 4 are all blocked
      (should (org-edna-test-check-block heading2-pom
                                         "Initial attempt to change heading 2"))
      (should (org-edna-test-check-block heading3-pom
                                         "Initial attempt to change heading 3"))
      (should (org-edna-test-check-block heading4-pom
                                         "Initial attempt to change heading 4"))
      ;; Mark heading 1 as DONE
      (should (not (org-edna-test-check-block heading1-pom
                                            "Set heading 1 to DONE")))
      ;; Only heading 2 should have a scheduled time
      (should (string-equal (org-entry-get heading2-pom "SCHEDULED")
                            "<2000-01-15 Sat 01:00>"))
      (should (not (org-entry-get heading3-pom "SCHEDULED")))
      (should (not (org-entry-get heading4-pom "SCHEDULED")))
      ;; The others should still be blocked.
      (should (org-edna-test-check-block heading3-pom
                                         "Second attempt to change heading 3"))
      (should (org-edna-test-check-block heading4-pom
                                         "Second attempt to change heading 4"))
      ;; Try changing heading 2
      (should (not (org-edna-test-check-block heading2-pom
                                            "Set heading 2 to DONE")))
      (should (string-equal (org-entry-get heading3-pom "SCHEDULED")
                            "<2000-01-16 Sun 09:00>"))
      ;; 4 should still be blocked
      (should (org-edna-test-check-block heading4-pom
                                         "Second attempt to change heading 4")))))

(ert-deftest org-edna-doc-test/laundry-cache ()
  "Test for the \"laundry\" example in the documentation.

This version enables cache, ensuring that the repeated calls to
the relative finders all still work while cache is enabled."
  (let ((org-edna-finder-use-cache t))
    (org-edna-doc-test-setup "e57ce099-9f37-47f4-a6bb-61a84eb1fbbe"
      (pcase-let* ((`(,heading1-pom ,heading2-pom ,heading3-pom ,heading4-pom)
                    (org-edna-test-children-marks)))
        ;; Verify that headings 2, 3, and 4 are all blocked
        (should (org-edna-test-check-block heading2-pom
                                           "Initial attempt to change heading 2"))
        (should (org-edna-test-check-block heading3-pom
                                           "Initial attempt to change heading 3"))
        (should (org-edna-test-check-block heading4-pom
                                           "Initial attempt to change heading 4"))
        ;; Mark heading 1 as DONE
        (should (not (org-edna-test-check-block heading1-pom
                                              "Set heading 1 to DONE")))
        ;; Only heading 2 should have a scheduled time
        (should (string-equal (org-entry-get heading2-pom "SCHEDULED")
                              "<2000-01-15 Sat 01:00>"))
        (should (not (org-entry-get heading3-pom "SCHEDULED")))
        (should (not (org-entry-get heading4-pom "SCHEDULED")))
        ;; The others should still be blocked.
        (should (org-edna-test-check-block heading3-pom
                                           "Second attempt to change heading 3"))
        (should (org-edna-test-check-block heading4-pom
                                           "Second attempt to change heading 4"))
        ;; Try changing heading 2
        (should (not (org-edna-test-check-block heading2-pom
                                              "Set heading 2 to DONE")))
        (should (string-equal (org-entry-get heading3-pom "SCHEDULED")
                              "<2000-01-16 Sun 09:00>"))
        ;; 4 should still be blocked
        (should (org-edna-test-check-block heading4-pom
                                           "Second attempt to change heading 4"))))))

(ert-deftest org-edna-doc-test/nightly ()
  (org-edna-doc-test-setup "8b6d9820-d943-4622-85c9-4a346e033453"
    (pcase-let* ((`(,nightly-pom ,lunch-pom ,door-pom ,dog-pom)
                  (org-edna-test-children-marks)))
      ;; Verify that Nightly is blocked
      (should (org-edna-test-check-block nightly-pom "Initial Nightly Check"))
      ;; Check off Lunch, and verify that nightly is still blocked
      (org-edna-test-mark-done lunch-pom)
      (should (org-edna-test-check-block nightly-pom "Nightly after Lunch"))
      ;; Check off Door, and verify that nightly is still blocked
      (org-edna-test-mark-done door-pom)
      (should (org-edna-test-check-block nightly-pom "Nightly after Door"))
      ;; Check off Dog.  This should trigger the others.
      (org-edna-test-mark-done dog-pom)
      (should (org-edna-test-compare-todos lunch-pom "TODO" "Lunch after Nightly Trigger"))
      (should (org-edna-test-compare-todos door-pom "TODO" "Door after Nightly Trigger"))
      (should (org-edna-test-compare-todos dog-pom "TODO" "Dog after Nightly Trigger"))
      (should (string-equal (org-entry-get nightly-pom "DEADLINE")
                            "<2000-01-16 Sun +1d>")))))

(ert-deftest org-edna-doc-test/nightly-cache ()
  (let ((org-edna-finder-use-cache t))
    (org-edna-doc-test-setup "8b6d9820-d943-4622-85c9-4a346e033453"
      (pcase-let* ((`(,nightly-pom ,lunch-pom ,door-pom ,dog-pom)
                    (org-edna-test-children-marks)))
        ;; Verify that Nightly is blocked
        (should (org-edna-test-check-block nightly-pom "Initial Nightly Check"))
        ;; Check off Lunch, and verify that nightly is still blocked
        (org-edna-test-mark-done lunch-pom)
        (should (org-edna-test-check-block nightly-pom "Nightly after Lunch"))
        ;; Check off Door, and verify that nightly is still blocked
        (org-edna-test-mark-done door-pom)
        (should (org-edna-test-check-block nightly-pom "Nightly after Door"))
        ;; Check off Dog.  This should trigger the others.
        (org-edna-test-mark-done dog-pom)
        (should (org-edna-test-compare-todos lunch-pom "TODO" "Lunch after Nightly Trigger"))
        (should (org-edna-test-compare-todos door-pom "TODO" "Door after Nightly Trigger"))
        (should (org-edna-test-compare-todos dog-pom "TODO" "Dog after Nightly Trigger"))
        (should (string-equal (org-entry-get nightly-pom "DEADLINE")
                              "<2000-01-16 Sun +1d>"))))))

(ert-deftest org-edna-doc-test/daily ()
  (org-edna-doc-test-setup "630805bb-a864-4cdc-9a6f-0f126e887c66"
    (pcase-let* ((`(,daily-pom ,lunch-pom ,door-pom ,dog-pom)
                  (org-edna-test-children-marks)))
      ;; Check off Lunch.  This should trigger the others.
      (org-edna-test-mark-done lunch-pom)
      (should (org-edna-test-compare-todos lunch-pom "TODO" "Lunch after Daily Trigger"))
      (should (org-edna-test-compare-todos door-pom "TODO" "Door after Daily Trigger"))
      (should (org-edna-test-compare-todos dog-pom "TODO" "Dog after Daily Trigger"))
      (should (string-equal (org-entry-get daily-pom "DEADLINE")
                            "<2000-01-16 Sun +1d>"))
      ;; Check off Door.  This should trigger the others.
      (org-edna-test-mark-done door-pom)
      (should (org-edna-test-compare-todos lunch-pom "TODO" "Lunch after Door Trigger"))
      (should (org-edna-test-compare-todos door-pom "TODO" "Door after Door Trigger"))
      (should (org-edna-test-compare-todos dog-pom "TODO" "Dog after Door Trigger"))
      (should (string-equal (org-entry-get daily-pom "DEADLINE")
                            "<2000-01-17 Mon +1d>"))
      ;; Check off Dog.  This should trigger the others.
      (org-edna-test-mark-done dog-pom)
      (should (org-edna-test-compare-todos lunch-pom "TODO" "Lunch after Dog Trigger"))
      (should (org-edna-test-compare-todos door-pom "TODO" "Door after Dog Trigger"))
      (should (org-edna-test-compare-todos dog-pom "TODO" "Dog after Dog Trigger"))
      (should (string-equal (org-entry-get daily-pom "DEADLINE")
                            "<2000-01-18 Tue +1d>")))))

(ert-deftest org-edna-doc-test/weekly ()
  (org-edna-doc-test-setup "cf529a5e-1b0c-40c3-8f85-fe2fc4df0ffd"
    (pcase-let* ((`(,weekly-pom ,lunch-pom ,door-pom ,dog-pom)
                  (org-edna-test-children-marks)))
      ;; Check off Lunch.  This should trigger the others.
      (org-edna-test-mark-done lunch-pom)
      (should (org-edna-test-compare-todos lunch-pom "TODO" "Lunch after Weekly Trigger"))
      (should (org-edna-test-compare-todos door-pom "TODO" "Door after Weekly Trigger"))
      (should (org-edna-test-compare-todos dog-pom "TODO" "Dog after Weekly Trigger"))
      (should (string-equal (org-entry-get weekly-pom "DEADLINE")
                            "<2000-01-16 Sun +1d>")))))

(ert-deftest org-edna-doc-test/basic-shower ()
  (org-edna-doc-test-setup "34d67756-927b-4a21-a62d-7989bd138946"
    (pcase-let* ((`(,shower-pom ,towels-pom) (org-edna-test-children-marks)))
      ;; Verify towels is blocked
      (should (org-edna-test-check-block towels-pom "Initial Towels Check"))
      ;; Check off "Take Shower" and verify that it incremented the property
      (org-edna-test-mark-done shower-pom)
      (should (string-equal (org-entry-get shower-pom "COUNT") "1"))
      ;; Verify towels is blocked
      (should (org-edna-test-check-block towels-pom "Towels Check, Count=1"))
      ;; Check off "Take Shower" and verify that it incremented the property
      (org-edna-test-mark-done shower-pom)
      (should (string-equal (org-entry-get shower-pom "COUNT") "2"))
      ;; Verify towels is blocked
      (should (org-edna-test-check-block towels-pom "Towels Check, Count=2"))
      ;; Check off "Take Shower" and verify that it incremented the property
      (org-edna-test-mark-done shower-pom)
      (should (string-equal (org-entry-get shower-pom "COUNT") "3"))
      ;; Verify that towels is no longer blocked.
      (should (not (org-edna-test-check-block towels-pom "Towels Check, Count=3")))
      ;; Verify that the property was reset.
      (should (string-equal (org-entry-get shower-pom "COUNT") "0")))))

(ert-deftest org-edna-doc-test/snow-shoveling ()
  (org-edna-doc-test-setup "b1d89bd8-db96-486e-874c-98e2b3a8cbf2"
    (pcase-let* ((`(,monday-pom ,tuesday-pom ,wednesday-pom ,shovel-pom)
                  (org-edna-test-children-marks)))
      ;; Verify shovels is blocked
      (should (org-edna-test-check-block shovel-pom "Initial Shovel Check"))

      ;; Mark Monday as done
      (org-edna-test-mark-done monday-pom)
      (should (not (org-edna-test-check-block shovel-pom "Shovel after changing Monday")))
      ;; Reset
      (org-edna-test-mark-todo monday-pom tuesday-pom wednesday-pom shovel-pom)

      ;; Mark Tuesday as done
      (org-edna-test-mark-done tuesday-pom)
      (should (not (org-edna-test-check-block shovel-pom "Shovel after changing Tuesday")))

      ;; Reset
      (org-edna-test-mark-todo monday-pom tuesday-pom wednesday-pom shovel-pom)
      ;; Mark Wednesday as done
      (org-edna-test-mark-done wednesday-pom)
      (should (not (org-edna-test-check-block shovel-pom "Shovel after changing Wednesday"))))))

(ert-deftest org-edna-doc-test/consider-fraction ()
  (org-edna-doc-test-setup "7de5af8b-a226-463f-8360-edd88b99462a"
    (pcase-let* ((`(,shovel-pom ,room-pom ,vacuum-pom ,lunch-pom ,edna-pom)
                  (org-edna-test-children-marks)))
      ;; Verify Edna is blocked
      (should (org-edna-test-check-block edna-pom "Initial Edna Check"))

      ;; Mark Shovel snow as done
      (org-edna-test-mark-done shovel-pom)
      ;; Verify Edna is still blocked
      (should (org-edna-test-check-block edna-pom "Edna Check after Shovel"))

      ;; Mark Vacuum as done
      (org-edna-test-mark-done vacuum-pom)
      ;; Verify Edna is still blocked
      (should (org-edna-test-check-block edna-pom "Edna Check after Vacuum"))

      ;; Mark Room as done
      (org-edna-test-mark-done room-pom)
      ;; Verify Edna is no longer blocked
      (should (not (org-edna-test-check-block edna-pom "Edna Check after Room"))))))

(ert-deftest org-edna-doc-test/consider-number ()
  (org-edna-doc-test-setup "b79279f7-be3c-45ac-96dc-6e962a5873d4"
    (pcase-let* ((`(,shovel-pom ,room-pom ,vacuum-pom ,lunch-pom ,edna-pom)
                  (org-edna-test-children-marks)))
      ;; Verify Edna is blocked
      (should (org-edna-test-check-block edna-pom "Initial Edna Check"))

      ;; Mark Shovel snow as done
      (org-edna-test-mark-done shovel-pom)
      ;; Verify Edna is still blocked
      (should (org-edna-test-check-block edna-pom "Edna Check after Shovel"))

      ;; Mark Vacuum as done
      (org-edna-test-mark-done vacuum-pom)
      ;; Verify Edna is still blocked
      (should (org-edna-test-check-block edna-pom "Edna Check after Vacuum"))

      ;; Mark Room as done
      (org-edna-test-mark-done room-pom)
      ;; Verify Edna is no longer blocked
      (should (not (org-edna-test-check-block edna-pom "Edna Check after Room"))))))

(ert-deftest org-edna-doc-test/has-tags ()
  (org-edna-doc-test-setup "6885e932-2c3e-4f20-ac22-5f5a0e791d67"
    (pcase-let* ((`(,first-pom ,second-pom ,third-pom)
                  (org-edna-test-children-marks)))
      ;; Verify that 3 is blocked
      (should (org-edna-test-check-block third-pom "Initial Check"))

      ;; Remove the tag from Task 1
      (org-with-point-at first-pom
        (org-set-tags-to ""))

      ;; Verify that 3 is still blocked
      (should (org-edna-test-check-block third-pom "Check after removing tag1"))

      ;; Remove the tag from Task 2
      (org-with-point-at second-pom
        (org-set-tags-to ""))

      ;; Verify that 3 is no longer blocked
      (should (not (org-edna-test-check-block third-pom "Check after removing tag2"))))))

(ert-deftest org-edna-doc-test/matches ()
  (org-edna-doc-test-setup "8170bf82-c2ea-49e8-bd79-97a95176783f"
    (pcase-let* ((`(,first-pom ,second-pom ,third-pom) (org-edna-test-children-marks)))
      ;; Verify that 3 is blocked
      (should (org-edna-test-check-block third-pom "Initial Check"))

      ;; Set 1 to DONE
      (org-edna-test-mark-done first-pom)

      ;; Verify that 3 is still blocked
      (should (org-edna-test-check-block third-pom "Check after First"))

      ;; Set 2 to DONE
      (org-edna-test-mark-done second-pom)

      ;; Verify that 3 is no longer blocked
      (should (not (org-edna-test-check-block third-pom "Check after Second"))))))

(ert-deftest org-edna-doc-test/chain ()
  (org-edna-doc-test-setup "1bd282ea-9238-47ea-9b4d-dafba19d278b"
    (pcase-let* ((`(,first-pom ,second-pom) (org-edna-test-children-marks)))
      ;; Set 1 to DONE
      (org-edna-test-mark-done first-pom)
      (should (string-equal (org-entry-get second-pom "COUNT") "2")))))

(ert-deftest org-edna-doc-test/multiple-blockers ()
  (org-edna-doc-test-setup "61e754c2-f292-42b5-8166-e4298dc190d6"
    (pcase-let* ((`(,first-pom ,second-pom ,third-pom) (org-edna-test-children-marks)))
      ;; Verify that 3 is blocked
      (should (org-edna-test-check-block third-pom "Initial Check"))

      ;; Set 1 to DONE
      (org-edna-test-mark-done first-pom)

      ;; Verify that 3 is still blocked
      (should (org-edna-test-check-block third-pom "Check after First"))

      ;; Reset 1
      (org-edna-test-mark-todo first-pom)

      ;; Set 2 to DONE
      (org-edna-test-mark-done second-pom)

      ;; Verify that 3 is still blocked
      (should (org-edna-test-check-block third-pom "Check after Second"))

      ;; Set 1 to DONE
      (org-edna-test-mark-done first-pom)

      ;; Verify that 3 is no longer blocked.
      (should (not (org-edna-test-check-block third-pom "Check after Both"))))))

(ert-deftest org-edna-user-test/time-spec ()
  (org-edna-doc-test-setup "5b63293c-23ef-40e7-ad8e-093e4c1e1464"
    (pcase-let* ((`(,first-pom ,second-pom ,third-pom) (org-edna-test-children-marks)))
      (org-edna-test-mark-done first-pom)
      ;; Test time is 2000-01-15, so this should be a week later
      (should (string-equal (org-entry-get second-pom "SCHEDULED")
                            "<2000-01-22 Sat>")))))

(provide 'org-edna-tests)

;;; org-edna-tests.el ends here
