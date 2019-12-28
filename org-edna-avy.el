;;; org-edna-avy.el --- Avy commands for org-edna -*- lexical-binding: t -*-

;; Copyright (C) 2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (org-edna "0.1"))
;; Keywords: org
;; URL: https://github.com/akirak/org-edna

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides avy commands for org-edna.

;;; Code:

(require 'org)

(declare-function 'org-edna-add-id-blocker "ext:org-edna")

;; Based on part of `avy-org-refile-as-child' in avy.el.
(defmacro org-edna-avy--with-heading (&rest progn)
  "Select an Org heading with avy and evaluate PROGN."
  `(progn
     (require 'avy)
     (unless (eq 't (let ((byte-compile-warnings '(not free-vars)))
                      (avy-with avy-goto-line
                        (avy-jump (rx bol (1+ "*") (1+ space))))))
       (unless (derived-mode-p 'org-mode)
         (user-error "Not in org-mode"))
       ,@progn)))

(defun org-edna-avy--get-create-id ()
  "Retrieve the ID to an entry selected with avy."
  (save-excursion
    (org-edna-avy--with-heading
     (org-id-get-create t))))

;;;###autoload
(defun org-edna-avy-add-id-blocker ()
  (interactive)
  (unless (derived-mode-p 'org-mode 'org-agenda-mode)
    (user-error "Not in org-mode or org-agenda-mode"))
  (let ((id (save-selected-window (org-edna-avy--get-create-id))))
    (org-edna-add-id-blocker id)))

(provide 'org-edna-avy)
;;; org-edna-avy.el ends here
