;;; org-revise.el --- Revision tool for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Haider Mirza

;; Author: Haider Mirza <paralle1epiped@outlook.com>
;; Url: https://git.sr.ht/~parallelepiped/org-revise
;; Keywords: Org, education, agenda.
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

;;; Code:

;;;; Requirements:
(require 'org-agenda)
(require 'cl-lib)

;;;; Variables
(defvar org-revise-files nil
  "The files that org-revise will dedicate for revision.
Make sure that you include the '.org' extention the files are org files.")

(defcustom org-revise-ask-initial-rating nil
  "Should you be asked for the initial rating."
  :type 'boolean)

(defcustom org-revise-use-keyword-identifier t
  "Should 'org-revise-headings' have a REVISE 'org-todo-prefix'."
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

;;;; Functions & Macros
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
    (user-error "ERROR: Make sure the filenames in the variable 'org-revise-files' starts with a different letter prefix"))
  (mapcar #'(lambda (filepath) (add-to-list 'org-agenda-files filepath)) org-revise-files))

;;;###autoload
(defun org-revise-init ()
  "Initializes org-revise. This is to be ran only once on startup.
  Variable 'org-revise-files' must be set before running this function."
  (interactive)
  ;; Create the files that do not exist in the variable 'org-revise-files'
  ((lambda (files)
     (dolist (file files)
       (unless (file-directory-p file)
	 (unless (file-exists-p file)
           (make-directory (file-name-directory file) t)
           (with-temp-buffer (write-file file))))))
   org-revise-files)
  (when org-revise-use-keyword-identifier
    (add-to-list 'org-todo-keywords '(sequence "REVISE"))))

(defmacro convert-integer-to-org-revise-rating (integer)
  (concat "|[#" integer "]"))

(defmacro convert-to-org-capture (directory)
  (let ((name (file-name-base directory))
	(no-ratings (prin1-to-string (length org-revise-task-date-incraments))))
    `(,(substring name 0 1) ,name entry
      (file ,directory)
      ,(concat "* "
	       (when org-revise-use-keyword-identifier
		 "REVISE ")
	       (if org-revise-ask-initial-rating
		   (concat "%^{Select the initial rating"
			   (mapconcat #'(lambda (element)
					  (macroexpand `(convert-integer-to-org-revise-rating
							 ,@(+ 1 (cl-position element org-revise-task-date-incraments :test 'equal)))))
				      org-revise-task-date-incraments)
			   "} :" name ":")
		 (concat "[#" no-ratings "]"))
	       
	       " %^{Start typing a heading} :" name ":\n"
	       (if org-revise-auto-date-today
		   "SCHEDULED: %t"
		 "SCHEDULED: %^t")
	       "\n%i%?"))))

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
  (org-revise-prerequisites-check)
  (let ((org-revise-files-names (mapcar #'file-name-base org-revise-files)))
    (find-file
     (nth
      (cl-position
       (completing-read "Choose a file: " org-revise-files-names) org-revise-files-names :test 'equal) org-revise-files))))

(defun org-revise-list-all-tasks ()
  "List all the tasks created with 'org-revise-create'."
  (interactive)
  (org-revise-prerequisites-check)
  (if org-revise-use-keyword-identifier
      (org-agenda "REVISE" "T")
    (let ((org-agenda-files org-revise-files)
	  (org-agenda-custom-commands 
	   '(("l" "All Org Revise Files"
	      ((tags (mapconcat #'file-name-base org-revise-files "|"))))
	     )))
      (org-agenda nil "l"))))

;; This function is for 'org-revise-done-task'
(defun line-contains (regexp)
  "Return true if the current line contains the passed regular expression."
  (save-excursion
    (beginning-of-line)
    (search-forward-regexp regexp (point-at-eol) t)))

(defun org-revise-done-task ()
  "This function reschedules the task due to the principles of spaced repetition. The incraments in which it reschedules are stored in the variable list 'org-revise-task-date-incraments'."
  (interactive)
  (when (not (eq major-mode 'org-mode))
    (user-error "ERROR: Make sure you are in a OrgMode file when running this function"))
  (let ((org-current-priority (string-to-number (org-entry-get nil "PRIORITY"))))
    (cond ((eq org-current-priority 1)
	   (progn
	     (org-priority-down)
	     (save-excursion
	       (search-backward-regexp "REVISE")
	       (replace-match (concat "DONE")))))
	  ((string= "DONE" (org-get-todo-state))
	   (user-error "ERROR: This task is completed as it has the DONE keyword"))
	  (t
	   (progn
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
	       (if (line-contains "#[0-9]")
		   (line-contains "#[0-9]")
		 (search-backward-regexp "#[0-9]"))
	       (replace-match (concat "#" (number-to-string
					   (- org-current-priority 1))))))))))

(defun org-revise-agenda ()
  (interactive)
  "Review all org-revise tasks."
(let ((org-agenda-custom-commands
      '(("r" agenda "Org Revise"
         ((org-agenda-skip-function
           '(org-agenda-skip-entry-if 'nottodo '("REVISE"))))
	 ))))
  (org-agenda nil "r")))

(provide 'org-revise)

;;; org-revise.el ends here
