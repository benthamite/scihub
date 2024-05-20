;;; scihub.el --- Download files from SciHub -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/scihub
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Download files from SciHub.

;;; Code:

;;;; Variables

(defconst scihub-doi-regexp
  "\\(10\\.[0-9]\\{4,9\\}\\(/\\)[-._;()/:A-Z0-9]+\\)$"
  "Regular expression that matches a DOI.")

;;;; User options

(defgroup scihub ()
  "Download files from SciHub."
  :group 'emacs)

(defcustom scihub-download-directory
  (expand-file-name "~/Downloads/")
  "Directory where downloaded files are saved."
  :type 'directory
  :group 'scihub)

;;;; Functions

(defun scihub-download (&optional doi)
  "Download DOI from SciHub."
  (interactive)
  (scihub-ensure-executable-exists)
  (scihub-ensure-subdirectory-exists)
  (let* ((doi (or doi (scihub-read-doi)))
	 (default-directory scihub-download-directory)
	 (process-name "scidownl-process")
	 (command (format "\"%s\" download --doi %s" (scihub-get-executable) doi))
	 (buffer (generate-new-buffer "*scihub-download-output*"))
	 (proc (start-process-shell-command process-name buffer command))
	 download-successful)
    (message "Trying to download DOI `%s'..." doi)
    (set-process-filter proc
			(lambda (process output)
			  (when (string-match-p "Successfully download the url" output)
			    (setq download-successful t))))
    (set-process-sentinel proc
			  (lambda (process signal)
			    (if (and (string= signal "finished\n") download-successful)
				(message "File downloaded successfully to `%s'." scihub-download-directory)
			      (message "File download failed."))))))

(defun scihub-update-server-list ()
  "Update the list of SciHub servers."
  (interactive)
  (scihub-ensure-executable-exists)
  (when-let ((output (shell-command-to-string
		      (format "%s domain.update" (scihub-get-executable)))))
    (message output)))

(defun scihub-get-executable ()
  "Get `scihub-scidownl' executable."
  (executable-find "scidownl"))

(defun scihub-ensure-executable-exists ()
  "Check if `scihub-scidownl' executable exists and signal an error if it doesn’t."
  (unless (scihub-get-executable)
    (user-error "`scidownl' not found; please install it (https://github.com/Tishacy/SciDownl)")))

(defun scihub-ensure-subdirectory-exists ()
  "Check if `scihub-download-directory' exists.
If directory doesn’t exist, ask the user whether to create it. If directory is
empty, signal an error."
  (when (string-empty-p scihub-download-directory)
    (user-error "Directory `%s' is empty" scihub-download-directory))
  (unless (file-exists-p scihub-download-directory)
    (if (y-or-n-p
	 (format "Directory `%s' does not exist; create it?" scihub-download-directory))
	(make-directory scihub-download-directory t)
      (user-error "Aborted"))))

(defun scihub-read-doi ()
  "Read DOI from user input."
  (let ((doi (read-string "DOI: ")))
    (if (scihub-is-doi-p doi)
	doi
      (if (y-or-n-p (format "`%s' does not look like a valid DOI; proceed anyway?" doi))
	  doi
	(user-error "Aborted")))))

(defun scihub-is-doi-p (string)
  "Return t if STRING is a valid DOI."
  (not (null (string-match scihub-doi-regexp string))))

(provide 'scihub)
;;; scihub.el ends here
