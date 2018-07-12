;;; org-edna-tests.el --- Tests for org-edna

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Author: Ian Dunn <dunni@gnu.org>
;; Keywords: convenience, text, org
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (seq "2.19") (org "8.0"))

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

(defun org-edna-find-test-heading (id)
  "Find the test heading with id ID."
  (org-id-find-id-in-file id org-edna-test-file t))

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

(ert-deftest org-edna-expand-sexp-form ()
  ;; Override cl-gentemp so we have a repeatable test
  (cl-letf* (((symbol-function 'cl-gentemp) (lambda (&optional prefix) (intern (format "%s1" prefix))))
             (input-sexp '((self)
                           (!done?)))
             (output-form (org-edna--expand-sexp-form input-sexp)))
    (should (equal
             output-form
             '(let ((targets1 nil)
                    (consideration1 nil)
                    (blocking-entry1 nil))
                (setq targets1 (org-edna--add-targets targets1 (org-edna-finder/self)))
                (setq blocking-entry1
                      (or blocking-entry1
                          (org-edna--handle-condition 'org-edna-condition/done?
                                                      '! 'nil targets1
                                                      consideration1))))))))

(ert-deftest org-edna-expand-sexp-form-multiple ()
  (cl-letf* ((target-ctr 0)
             (consideration-ctr 0)
             (blocking-entry-ctr 0)
             ((symbol-function 'cl-gentemp)
              (lambda (&optional prefix)
                (let ((ctr (pcase prefix
                             ("targets" (cl-incf target-ctr))
                             ("consideration" (cl-incf consideration-ctr))
                             ("blocking-entry" (cl-incf blocking-entry-ctr))
                             (_ 0))))
                  (intern (format "%s%s" prefix ctr)))))
             (input-sexp '(((match "checklist")
                            (todo! DONE))
                           ((siblings)
                            (todo! TODO))))
             (expected-form
              '(let ((targets1 nil)
                     (consideration1 nil)
                     (blocking-entry1 nil))
                 ;; Don't need a new set of variables
                 (let ((targets2 targets1)
                       (consideration2 consideration1)
                       (blocking-entry2 blocking-entry1))
                   (setq targets2
                         (org-edna--add-targets targets2
                                                (org-edna-finder/match "checklist")))
                   (org-edna--handle-action 'org-edna-action/todo!
                                            targets2
                                            (point-marker)
                                            '(DONE)))
                 (let ((targets5 targets1)
                       (consideration5 consideration1)
                       (blocking-entry5 blocking-entry1))
                   (setq targets5
                         (org-edna--add-targets targets5
                                                (org-edna-finder/siblings)))
                   (org-edna--handle-action 'org-edna-action/todo!
                                            targets5
                                            (point-marker)
                                            '(TODO)))))
             (output-form (org-edna--expand-sexp-form input-sexp)))
    (should (equal output-form expected-form))))

(ert-deftest org-edna-expand-sexp-form-if-else ()
  (cl-letf* ((target-ctr 0)
             (consideration-ctr 0)
             (blocking-entry-ctr 0)
             ((symbol-function 'cl-gentemp)
              (lambda (&optional prefix)
                (let ((ctr (pcase prefix
                             ("targets" (cl-incf target-ctr))
                             ("consideration" (cl-incf consideration-ctr))
                             ("blocking-entry" (cl-incf blocking-entry-ctr))
                             (_ 0))))
                  (intern (format "%s%s" prefix ctr)))))
             (input-sexp '((if
                               ((match "checklist")
                                (done\?))
                               ((self)
                                (todo! TODO))
                             ((siblings)
                              (todo! DONE)))))
             (expected-form
              '(let
                   ((targets1 nil)
                    (consideration1 nil)
                    (blocking-entry1 nil))
                 (if
                     ;; No inheritance in the conditional scope
                     (not
                      (let
                          ((targets3 nil)
                           (consideration3 nil)
                           (blocking-entry3 nil))
                        ;; Add targets for checklist match
                        (setq targets3
                              (org-edna--add-targets targets3
                                                     (org-edna-finder/match "checklist")))
                        ;; Handle condition
                        (setq blocking-entry3
                              (or blocking-entry3
                                  (org-edna--handle-condition 'org-edna-condition/done\? 'nil 'nil targets3 consideration3)))))
                     ;; Use the top-level scope for then case
                     (progn
                       ;; Add targets for self finder
                       (setq targets1
                             (org-edna--add-targets targets1
                                                    (org-edna-finder/self)))
                       ;; Mark as TODO
                       (org-edna--handle-action 'org-edna-action/todo! targets1
                                                (point-marker)
                                                '(TODO)))
                   ;; Use the top-level scope for the else case
                   (progn
                     ;; Find siblings
                     (setq targets1
                           (org-edna--add-targets targets1
                                                  (org-edna-finder/siblings)))
                     ;; Mark as DONE
                     (org-edna--handle-action 'org-edna-action/todo! targets1
                                              (point-marker)
                                              '(DONE))))))

             (output-form (org-edna--expand-sexp-form input-sexp)))
    (should (equal output-form expected-form))))

(ert-deftest org-edna-expand-sexp-form-if-no-else ()
  (cl-letf* ((target-ctr 0)
             (consideration-ctr 0)
             (blocking-entry-ctr 0)
             ((symbol-function 'cl-gentemp)
              (lambda (&optional prefix)
                (let ((ctr (pcase prefix
                             ("targets" (cl-incf target-ctr))
                             ("consideration" (cl-incf consideration-ctr))
                             ("blocking-entry" (cl-incf blocking-entry-ctr))
                             (_ 0))))
                  (intern (format "%s%s" prefix ctr)))))
             (input-sexp '((if
                               ((match "checklist")
                                (done\?))
                               ((self)
                                (todo! TODO)))))
             (expected-form
              '(let
                   ((targets1 nil)
                    (consideration1 nil)
                    (blocking-entry1 nil))
                 (if
                     ;; No inheritance in the conditional scope
                     (not
                      (let
                          ((targets3 nil)
                           (consideration3 nil)
                           (blocking-entry3 nil))
                        ;; Add targets for checklist match
                        (setq targets3
                              (org-edna--add-targets targets3
                                                     (org-edna-finder/match "checklist")))
                        ;; Handle condition
                        (setq blocking-entry3
                              (or blocking-entry3
                                  (org-edna--handle-condition 'org-edna-condition/done\? 'nil 'nil targets3 consideration3)))))
                     ;; Use the top-level scope for then case
                     (progn
                       ;; Add targets for self finder
                       (setq targets1
                             (org-edna--add-targets targets1
                                                    (org-edna-finder/self)))
                       ;; Mark as TODO
                       (org-edna--handle-action 'org-edna-action/todo! targets1
                                                (point-marker)
                                                '(TODO)))
                   ;; End with a nil
                   nil)))
             (output-form (org-edna--expand-sexp-form input-sexp)))
    (should (equal output-form expected-form))))


;; Finders

(defsubst org-edna-heading (pom)
  (org-with-point-at pom
    (org-get-heading t t t t)))

(ert-deftest org-edna-finder/match-single-arg ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (targets (org-edna-finder/match "test&1")))
    (should (= (length targets) 2))
    (should (string-equal (org-edna-heading (nth 0 targets)) "Tagged Heading 1"))
    (should (string-equal (org-edna-heading (nth 1 targets)) "Tagged Heading 2"))))

(ert-deftest org-edna-finder/ids-single ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (test-id "caccd0a6-d400-410a-9018-b0635b07a37e")
         (targets (org-edna-finder/ids test-id)))
    (should (= (length targets) 1))
    (should (string-equal (org-edna-heading (nth 0 targets)) "Blocking Test"))
    (should (string-equal (org-entry-get (nth 0 targets) "ID") test-id))))

(ert-deftest org-edna-finder/ids-multiple ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (test-ids '("0d491588-7da3-43c5-b51a-87fbd34f79f7"
                     "b010cbad-60dc-46ef-a164-eb155e62cbb2"))
         (targets (apply 'org-edna-finder/ids test-ids)))
    (should (= (length targets) 2))
    (should (string-equal (org-edna-heading (nth 0 targets)) "ID Heading 1"))
    (should (string-equal (org-entry-get (nth 0 targets) "ID") (nth 0 test-ids)))
    (should (string-equal (org-edna-heading (nth 1 targets)) "ID Heading 2"))
    (should (string-equal (org-entry-get (nth 1 targets) "ID") (nth 1 test-ids)))))

(ert-deftest org-edna-finder/match-blocker ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (heading (org-id-find "caccd0a6-d400-410a-9018-b0635b07a37e" t))
         (blocker (org-entry-get heading "BLOCKER"))
         blocking-entry)
    (should (string-equal "match(\"test&1\")" blocker))
    (org-with-point-at heading
      (setq blocking-entry (org-edna-process-form blocker 'condition)))
    (should (string-equal (substring-no-properties blocking-entry)
                          "TODO Tagged Heading 1 :1:test:"))))

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
         (current (org-id-find "82a4ac3d-9565-4f94-bc84-2bbfd8d7d96c" t))
         (targets (org-with-point-at current (org-edna-finder/self))))
    (should (= (length targets) 1))
    (should (equal current (nth 0 targets)))))

(ert-deftest org-edna-finder/siblings ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-id-find org-edna-test-sibling-one-id t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    `(,org-edna-test-sibling-one-id
                      ,org-edna-test-sibling-two-id
                      ,org-edna-test-sibling-three-id)))
         (targets (org-with-point-at current
                    (org-edna-finder/siblings))))
    (should (equal siblings targets))))

(ert-deftest org-edna-finder/siblings-wrap ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-id-find "72534efa-e932-460b-ae2d-f044a0074815" t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    '("06aca55e-ce09-46df-80d7-5b52e55d6505"
                      "82a4ac3d-9565-4f94-bc84-2bbfd8d7d96c")))
         (targets (org-with-point-at current
                    (org-edna-finder/siblings-wrap))))
    (should (= (length targets) 2))
    (should (equal siblings targets))))

(ert-deftest org-edna-finder/rest-of-siblings ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-id-find "72534efa-e932-460b-ae2d-f044a0074815" t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    '("06aca55e-ce09-46df-80d7-5b52e55d6505")))
         (targets (org-with-point-at current
                    (org-edna-finder/rest-of-siblings))))
    (should (= (length targets) 1))
    (should (equal siblings targets))))

(ert-deftest org-edna-finder/next-sibling ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-id-find "72534efa-e932-460b-ae2d-f044a0074815" t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    '("06aca55e-ce09-46df-80d7-5b52e55d6505")))
         (targets (org-with-point-at current
                    (org-edna-finder/next-sibling))))
    (should (= (length targets) 1))
    (should (equal siblings targets))))

(ert-deftest org-edna-finder/next-sibling-wrap-next ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-id-find org-edna-test-sibling-two-id t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    `(,org-edna-test-sibling-three-id)))
         (targets (org-with-point-at current
                    (org-edna-finder/next-sibling-wrap))))
    (should (= (length targets) 1))
    (should (equal siblings targets))))

(ert-deftest org-edna-finder/next-sibling-wrap-wrap ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-id-find org-edna-test-sibling-three-id t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    `(,org-edna-test-sibling-one-id)))
         (targets (org-with-point-at current
                    (org-edna-finder/next-sibling-wrap))))
    (should (= (length targets) 1))
    (should (equal siblings targets))))

(ert-deftest org-edna-finder/previous-sibling ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-id-find "06aca55e-ce09-46df-80d7-5b52e55d6505" t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    '("72534efa-e932-460b-ae2d-f044a0074815")))
         (targets (org-with-point-at current
                    (org-edna-finder/previous-sibling))))
    (should (= (length targets) 1))
    (should (equal siblings targets))))

(ert-deftest org-edna-finder/first-child ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-id-find org-edna-test-parent-id t))
         (first-child (list (org-id-find org-edna-test-sibling-one-id t)))
         (targets (org-with-point-at current
                    (org-edna-finder/first-child))))
    (should (= (length targets) 1))
    (should (equal first-child targets))))

(ert-deftest org-edna-finder/children ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-id-find org-edna-test-parent-id t))
         (children (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    `(,org-edna-test-sibling-one-id
                      ,org-edna-test-sibling-two-id
                      ,org-edna-test-sibling-three-id)))
         (targets (org-with-point-at current
                    (org-edna-finder/children))))
    (should (= (length targets) 3))
    (should (equal children targets))))

(ert-deftest org-edna-finder/parent ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-id-find org-edna-test-sibling-one-id t))
         (parent (list (org-id-find org-edna-test-parent-id t)))
         (targets (org-with-point-at current
                    (org-edna-finder/parent))))
    (should (= (length targets) 1))
    (should (equal parent targets))))

(ert-deftest org-edna-relatives/from-top ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-id-find org-edna-test-sibling-one-id t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    `(,org-edna-test-sibling-one-id)))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives 'from-top 1))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/from-bottom ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (current (org-id-find org-edna-test-sibling-one-id t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    `(,org-edna-test-sibling-three-id)))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives 'from-bottom 1))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/forward-wrap-no-wrap ()
  (let* ((start-marker org-edna-test-sibling-one-id)
         (target-list `(,org-edna-test-sibling-two-id))
         (arg 'forward-wrap)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg 1))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/forward-wrap-wrap ()
  (let* ((start-marker org-edna-test-sibling-three-id)
         (target-list `(,org-edna-test-sibling-one-id))
         (arg 'forward-wrap)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg 1))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/forward-no-wrap-no-wrap ()
  (let* ((start-marker org-edna-test-sibling-one-id)
         (target-list `(,org-edna-test-sibling-two-id))
         (arg 'forward-no-wrap)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg 1))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/forward-no-wrap-wrap ()
  (let* ((start-marker org-edna-test-sibling-three-id)
         (target-list nil)
         (arg 'forward-no-wrap)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg size))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/walk-up-with-self ()
  (let* ((start-marker org-edna-test-sibling-one-id)
         (target-list `(,org-edna-test-sibling-one-id))
         (arg 'walk-up-with-self)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg 1))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/walk-down ()
  (let* ((start-marker org-edna-test-parent-id)
         (target-list `(,org-edna-test-sibling-one-id))
         (arg 'walk-down)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg 1))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/walk-down-with-self ()
  (let* ((start-marker org-edna-test-parent-id)
         (target-list `(,org-edna-test-parent-id))
         (arg 'walk-down-with-self)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg 1))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/walk-down ()
  (let* ((start-marker org-edna-test-parent-id)
         (target-list `(,org-edna-test-sibling-one-id))
         (arg 'walk-down)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    target-list))
         (targets (org-with-point-at current
                    (org-edna-finder/relatives arg size))))
    (should (equal siblings targets))))

(ert-deftest org-edna-relatives/filter-todo-only ()
  (let* ((start-marker org-edna-test-relative-parent-one)
         (target-list `(,org-edna-test-relative-child-with-todo))
         (arg 'step-down)
         (org-agenda-files `(,org-edna-test-file))
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
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
         (current (org-id-find start-marker t))
         (siblings (mapcar
                    (lambda (uuid) (org-id-find uuid t))
                    target-list)))
    (should (equal siblings
                   (org-with-point-at current
                     (org-edna-finder/relatives arg 'deadline-up size))))
    (should (equal (nreverse siblings)
                   (org-with-point-at current
                     (org-edna-finder/relatives arg 'deadline-down size))))))


;; Actions

(ert-deftest org-edna-action/todo-test ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (target (org-id-find "0d491588-7da3-43c5-b51a-87fbd34f79f7" t)))
    (org-with-point-at target
      (org-edna-action/todo! nil "DONE")
      (should (string-equal (org-entry-get nil "TODO") "DONE"))
      (org-edna-action/todo! nil "TODO")
      (should (string-equal (org-entry-get nil "TODO") "TODO"))
      (org-edna-action/todo! nil 'DONE)
      (should (string-equal (org-entry-get nil "TODO") "DONE"))
      (org-edna-action/todo! nil 'TODO)
      (should (string-equal (org-entry-get nil "TODO") "TODO")))))

(ert-deftest org-edna-action-scheduled/wkdy ()
  ;; Override `current-time' so we can get a deterministic value
  (cl-letf* (((symbol-function 'current-time) (lambda () org-edna-test-time))
             (org-agenda-files `(,org-edna-test-file))
             (target (org-id-find "0d491588-7da3-43c5-b51a-87fbd34f79f7" t)))
    (org-with-point-at target
      (org-edna-action/scheduled! nil "Mon")
      (should (string-equal (org-entry-get nil "SCHEDULED")
                            "<2000-01-17 Mon>"))
      (org-edna-action/scheduled! nil 'rm)
      (should (not (org-entry-get nil "SCHEDULED")))
      (org-edna-action/scheduled! nil "Mon 9:00")
      (should (string-equal (org-entry-get nil "SCHEDULED")
                            "<2000-01-17 Mon 09:00>"))
      (org-edna-action/scheduled! nil 'rm)
      (should (not (org-entry-get nil "SCHEDULED"))))))

(ert-deftest org-edna-action-scheduled/cp ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (target (org-id-find "0d491588-7da3-43c5-b51a-87fbd34f79f7" t))
         (source (org-id-find "97e6b0f0-40c4-464f-b760-6e5ca9744eb5" t))
         (pairs '((cp . rm) (copy . remove) ("cp" . "rm") ("copy" . "remove"))))
    (org-with-point-at target
      (dolist (pair pairs)
        (org-edna-action/scheduled! source (car pair))
        (should (string-equal (org-entry-get nil "SCHEDULED")
                              "<2000-01-15 Sat 00:00>"))
        (org-edna-action/scheduled! source (cdr pair))
        (should (not (org-entry-get nil "SCHEDULED")))))))

(ert-deftest org-edna-action-scheduled/inc ()
  ;; Override `current-time' so we can get a deterministic value
  (cl-letf* (((symbol-function 'current-time) (lambda () org-edna-test-time))
             (org-agenda-files `(,org-edna-test-file))
             (target (org-id-find "97e6b0f0-40c4-464f-b760-6e5ca9744eb5" t)))
    (org-with-point-at target
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
                            "<2000-01-15 Sat 00:00>")))))

(ert-deftest org-edna-action-scheduled/landing ()
  "Test landing arguments to scheduled increment."
  ;; Override `current-time' so we can get a deterministic value
  (cl-letf* (((symbol-function 'current-time) (lambda () org-edna-test-time))
             (org-agenda-files `(,org-edna-test-file))
             (target (org-id-find "97e6b0f0-40c4-464f-b760-6e5ca9744eb5" t)))
    (org-with-point-at target
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
                            "<2000-01-15 Sat 00:00>")))))

(ert-deftest org-edna-action-scheduled/float ()
  (cl-letf* (((symbol-function 'current-time) (lambda () org-edna-test-time))
             (org-agenda-files `(,org-edna-test-file))
             (target (org-id-find "97e6b0f0-40c4-464f-b760-6e5ca9744eb5" t)))
    (org-with-point-at target
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
                            "<2000-03-20 Mon 00:00>"))
      ;; Back to Saturday for other tests
      (org-edna-action/scheduled! nil "2000-01-15 Sat 00:00")
      (should (string-equal (org-entry-get nil "SCHEDULED")
                            "<2000-01-15 Sat 00:00>")))))

(ert-deftest org-edna-action-tag ()
  (let ((pom (org-edna-find-test-heading org-edna-test-id-heading-one)))
    (org-with-point-at pom
      (org-edna-action/tag! nil "tag")
      (should (equal (org-get-tags) '("tag")))
      (org-edna-action/tag! nil "")
      (should (equal (org-get-tags) '(""))))))

(ert-deftest org-edna-action-property ()
  (let ((pom (org-edna-find-test-heading org-edna-test-id-heading-one)))
    (org-with-point-at pom
      (org-edna-action/set-property! nil "TEST" "1")
      (should (equal (org-entry-get nil "TEST") "1"))
      (org-edna-action/delete-property! nil "TEST")
      (should-not (org-entry-get nil "TEST")))))

(ert-deftest org-edna-action-property/inc-dec ()
  (let ((pom (org-edna-find-test-heading org-edna-test-id-heading-one)))
    (org-with-point-at pom
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
      (should-not (org-entry-get nil "TEST")))))

(ert-deftest org-edna-action-property/next-prev ()
  (let ((pom (org-edna-find-test-heading org-edna-test-id-heading-one)))
    (org-with-point-at pom
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
      (should-not (org-entry-get nil "COUNTER")))))

(ert-deftest org-edna-action-clock ()
  (let ((pom (org-edna-find-test-heading org-edna-test-id-heading-one)))
    (org-with-point-at pom
      (org-edna-action/clock-in! nil)
      (should (org-clocking-p))
      (should (equal org-clock-hd-marker pom))
      (org-edna-action/clock-out! nil)
      (should-not (org-clocking-p)))))

(ert-deftest org-edna-action-priority ()
  (let ((pom (org-edna-find-test-heading org-edna-test-id-heading-one))
        (org-lowest-priority  ?C)
        (org-highest-priority ?A)
        (org-default-priority ?B))
    (org-with-point-at pom
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
  (let ((pom (org-edna-find-test-heading org-edna-test-id-heading-one)))
    (org-with-point-at pom
      (org-edna-action/set-effort! nil "0:01")
      (should (equal (org-entry-get nil "EFFORT") "0:01"))
      (org-edna-action/set-effort! nil 'increment)
      (should (equal (org-entry-get nil "EFFORT") "0:02"))
      (org-entry-delete nil "EFFORT"))))

(ert-deftest org-edna-action-archive ()
  (let ((org-archive-save-context-info '(todo))
        (pom (org-edna-find-test-heading org-edna-test-archive-heading))
        ;; Archive it to the same location
        (org-archive-location "::** Archive")
        (org-edna-prompt-for-archive nil))
    (org-with-point-at pom
      (org-edna-action/archive! nil)
      (should (equal (org-entry-get nil "ARCHIVE_TODO") "TODO"))
      (org-entry-delete nil "ARCHIVE_TODO"))))

(ert-deftest org-edna-action-chain ()
  (let ((old-pom (org-edna-find-test-heading org-edna-test-id-heading-one))
        (new-pom (org-edna-find-test-heading org-edna-test-id-heading-two)))
    (org-entry-put old-pom "TEST" "1")
    (org-with-point-at new-pom
      (org-edna-action/chain! old-pom "TEST")
      (should (equal (org-entry-get nil "TEST") "1")))
    (org-entry-delete old-pom "TEST")
    (org-entry-delete new-pom "TEST")))


;; Conditions

(defun org-edna-test-condition-form (func-sym pom-true pom-false block-true block-false &rest args)
  (org-with-point-at pom-true
    (should-not (apply func-sym t args))
    (should     (equal (apply func-sym nil args) block-true)))
  (org-with-point-at pom-false
    (should     (equal (apply func-sym t args) block-false))
    (should-not (apply func-sym nil args))))

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


;; Consideration

(ert-deftest org-edna-consideration/all ()
  (let ((blocks-blocking `("a" nil "b"))
        (blocks-no-blocking `(nil nil nil)))
    (should (string-equal (org-edna-handle-consideration 'all blocks-blocking) "a"))
    (should (not (org-edna-handle-consideration 'all blocks-no-blocking)))))

(ert-deftest org-edna-consideration/integer ()
  (let ((blocks-blocking `("a" "c" "b"))
        (blocks-no-blocking `("a" nil "b"))
        (blocks-empty `(nil nil nil)))
    (should (string-equal (org-edna-handle-consideration 1 blocks-blocking) "a"))
    (should (not (org-edna-handle-consideration 1 blocks-no-blocking)))
    (should (not (org-edna-handle-consideration 1 blocks-empty)))))

(ert-deftest org-edna-consideration/float ()
  (let ((blocks-blocking `("a" "c" "b"))
        (blocks-no-blocking `("a" nil "b"))
        (blocks-empty `(nil nil nil)))
    (should (string-equal (org-edna-handle-consideration 0.25 blocks-blocking) "a"))
    (should (not (org-edna-handle-consideration 0.25 blocks-no-blocking)))
    (should (not (org-edna-handle-consideration 0.25 blocks-empty)))))

(provide 'org-edna-tests)

;;; org-edna-tests.el ends here
