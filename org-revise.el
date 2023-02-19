;;; org-revise.el --- Revision tool for Emacs            -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Haider Mirza

;; Author: Haider Mirza <paralle1epiped@outlook.com>
;; Keywords: orgmode education
;; Version: 1.0.0

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Org-revise is a package that uses spaced repetition and the features of org-agenda
;; to help people memorise and revise information.

;;; Test Area

(setq org-revise-files
      '("~/documents/revision/maths.org"
	"~/documents/revision/english.org"
	"~/documents/revision/physics.org"
	"~/documents/revision/chemistry.org"
	"~/documents/revision/biology.org"
	"~/documents/revision/geography.org"))

;;; Code:
(require 'org)
(require 'cl-lib)

(defvar org-revise-files nil
  "The files that org-revise will dedicate for revision.
Make sure that you include the '.org' extention the files are org files.")

(defcustom org-revise-ask-initial-rating nil
  "Should you be asked for the initial rating."
  :type 'boolean)

(defcustom org-revise-auto-date-today nil
  "This variable controls if the date automatically gets filled out for today when creating a task."
  :type 'boolean)

(defcustom org-revise-use-agenda t
  "Whether to make all files listed in the variable 'org-revise-files' included in Org Agenda."
  :type 'boolean)

(defcustom org-revise-initial-rating "C"
  "What should the initial rating be set as.
This variable will be used only if 'org-revise-ask-initial-rating' is nil.

The variable MUST be set to either 'A' 'B' or 'C'.
'C' is the default option"
  :type '(choice (const :tag "A" A)
		 (const :tag "B" B)
		 (const :tag "C" C)))

(defun org-revise-prerequisites-check ()
  "Checks Prerequisites for many functions in the org revise package"
  (if (= (length org-revise-files) 0)
      (user-error "ERROR: Please set the variable 'org-revise-files'")))

;; Remake this function
(defun org-revise-init ()
  "Initializes org-revise. This is to be ran only once on startup.
Variable 'org-revise-files' must be set before running this function."
  (interactive)
  (org-revise-prerequisites-check)
  (let ((org-revise-file-exists (mapcar #'file-exists-p org-revise-files))
	(org-revise-index-file-no-exist)
	(org-revise-files-copy org-revise-files))
    (when (member nil org-revise-file-exists)
      (if (not (yes-or-no-p "Create the files that do not exist?"))
	  (user-error "Make sure you set the variable 'org-revise-files' to have files that exist or let this function create the files.")
	(while (member nil org-revise-file-exists)
	  (setq org-revise-index-file-no-exist
		(cl-position nil org-revise-file-exists :test 'equal))
	  (make-empty-file
	   (nth org-revise-index-file-no-exist org-revise-files))
	  (setq org-revise-file-exists
		((lambda (list)
		   (let ((x list))
		     (pop (nthcdr org-revise-index-file-no-exist x))
		     x)) org-revise-file-exists))
	  (setq org-revise-files-copy
		((lambda (list)
		   (let ((x list))
		     (pop (nthcdr org-revise-index-file-no-exist x))
		     x)) org-revise-files-copy)))))))
  ;;  (when org-revise-use-agenda
  ;;   (mapcar #'(lambda (filedirectory) (push filedirectory org-agenda-files)) org-revise-files)))

(defmacro convert-to-org-capture (directory)
  (let ((name (file-name-base directory)))
    `(,(substring name 0 1) ,name entry
      (file+headline ,directory ,(concat "Org Revise - " name))
      ,(concat "* "
	       (if org-revise-ask-initial-rating
		   "%^{Select the initial rating|[#C]|[#B]|[#A]}"
		 (concat "[#" org-revise-initial-rating "]"))
	       " %^{Start typing a heading}\n"
	       (if org-revise-auto-date-today
		   "SCHEDULED: %t"
		 "SCHEDULED: %^t")
	       "\n%i%?"
	       ))))

(defun org-revise-create ()
  "Creates a task in one of the files declared in the variable 'org-revise-files'."
  (interactive)
  (let ((org-capture-templates
	 (mapcar #'(lambda (string) (macroexpand `(convert-to-org-capture ,string))) org-revise-files)))
    (org-capture)))

(defun org-revise-goto ()
  "Opens one of the files listed in the variable 'org-revise-files'."
  (interactive)
  (let ((org-revise-files-names (mapcar #'file-name-base org-revise-files)))
    (find-file
     (nth
      (cl-position
       (completing-read "Choose a file: " org-revise-files-names) org-revise-files-names :test 'equal) org-revise-files))))
