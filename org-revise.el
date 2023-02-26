;;; org-revise.el --- Revision tool for Emacs

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

;;                      |----------------------------|
;;                           Test Variables Area
;;                      |----------------------------|

(setq org-revise-files
      '("~/documents/revision/maths.org"
	"~/documents/revision/english.org"
	"~/documents/revision/physics.org"
	"~/documents/revision/chemistry.org"
	"~/documents/revision/biology.org"
	"~/documents/revision/geography.org"))

(setq org-agenda-files
      '("/home/haider/documents/agenda/home.org"))

(setq org-revise-task-date-incraments
      '("0" "7" "14" "30"))

;;; Code:
(require 'org-agenda)
(require 'cl-lib)

(defvar org-revise-files nil
  "The files that org-revise will dedicate for revision.
Make sure that you include the '.org' extention the files are org files.")

(defcustom org-revise-ask-initial-rating nil
  "Should you be asked for the initial rating."
  :type 'boolean)

(defcustom org-revise-use-scheduled t
  "Whether org-revise manages dates with scheduled or deadline"
  :type 'boolean)

(defcustom org-revise-auto-date-today nil
  "This variable controls if the date automatically gets filled out for today when creating a task."
  :type 'boolean)

(defvar org-revise-task-date-incraments '("0" "7" "14" "30")
  "In what incraments are dates added to a task.

The starting date by default is zero so the task has to first be done today.

This function allows org-revise to utilise spaced repetition.
This function should be set as a list of strings where each string contains the amount of days until next incrament.")

(defun org-revise-prerequisites-check ()
  "Checks Prerequisites for many functions in the org revise package"
  (when (= (length org-revise-files) 0)
    (user-error "ERROR: Please set the variable 'org-revise-files'"))
  (when ((lambda (list) ""
	   (let ((unique1 (cl-remove-duplicates list :test #'equal)))
	     (if (eq list unique1)
		 nil
	       t)))
	 
	 (mapcar #'(lambda (name) (substring name 0 1)) (mapcar #'file-name-base org-revise-files)))
    (user-error "ERROR: Make sure the filename starts with a different letter prefix")))

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

(defmacro convert-integer-to-org-revise-rating (integer)
  (concat "|[#" integer "]"))

(defmacro convert-to-org-capture (directory)
  (let ((name (file-name-base directory))
	(no-ratings (prin1-to-string (length org-revise-task-date-incraments))))
    `(,(substring name 0 1) ,name entry
      (file+headline ,directory ,(concat "Org Revise - " name ":" name ":\n"))
      ,(concat "* "
	       (if org-revise-ask-initial-rating
		   (concat "%^{Select the initial rating"
			   (mapconcat #'(lambda (element)
					  (macroexpand `(convert-integer-to-org-revise-rating
							 ,@(+ 1 (cl-position element org-revise-task-date-incraments :test 'equal)))))
				      org-revise-task-date-incraments)
			   "} :" name ":")
		 (concat "[#" no-ratings "]"))
	       
	       " %^{Start typing a heading}\n"
	       (if org-revise-auto-date-today
		   "SCHEDULED: %t"
		 "SCHEDULED: %^t")
	       "\n%i%?"
	       ))))

(defun org-revise-create ()
  "Creates a task in one of the files declared in the variable 'org-revise-files'."
  (interactive)
  (org-revise-prerequisites-check)
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

(defun org-revise-list-all-tasks ()
  "List all the tasks created with 'org-revise-create'."
  (interactive)
  (let ((org-agenda-files org-revise-files)
	(org-agenda-custom-commands 
	 '(("l" "All Org Revise Files"
	    ((tags (mapconcat #'file-name-base org-revise-files "|"))))
	   )))
    (org-agenda nil "l")))

(defun org-revise-done-task ()
  "This function reschedules the task due to the principles of spaced repetition. The incraments in which it reschedules are stored in the variable list 'org-revise-task-date-incraments'."
  (interactive)
  (when (not (eq major-mode 'org-mode))
    (user-error "ERROR: Make sure you are in a OrgMode file when running this function"))
  (let ((org-current-priority (string-to-number (org-entry-get nil "PRIORITY"))))
    ;; https://emacs.stackexchange.com/questions/75956/convert-foo-to-foo
    (org-schedule nil
		  (concat "+"
			  (number-to-string
			   (- (string-to-number
			       (nth (+ 1 (-
					  (length org-revise-task-date-incraments)
					  org-current-priority))
				    org-revise-task-date-incraments)) 1)) "d"))
    (save-excursion
      (search-backward-regexp "#[0-9]")
      (replace-match (concat "#" (number-to-string
				  (- org-current-priority 1)))))))
