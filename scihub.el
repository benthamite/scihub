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

;;;;; Command helpers

(defun scihub-get-executable ()
  "Get `scihub-scidownl' executable."
  (executable-find "scidownl"))

(defun scihub-command (args)
  "Run `scidownl' with ARGS."
  (scihub-ensure-executable-exists)
  (format "\"%s\" %s" (scihub-get-executable) args))

;;;;; Ensure funs

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

;;;;; Commands

(defun scihub-download (&optional doi callback)
  "Download DOI from SciHub.
CALLBACK is a callback function that is called after the download is complete.
It receives the downloaded file path and the BibTeX key as arguments."
  (interactive)
  (scihub-ensure-subdirectory-exists)
  (let* ((doi (or doi (scihub-read-doi)))
	 (bibtex-key (pcase major-mode
		       ('bibtex-mode (bibtex-extras-get-key))
		       ((or 'ebib-entry-mode 'ebib-index-mode)
			(ebib-extras-get-field "=key="))))
	 (default-directory scihub-download-directory)
	 (process-name "scidownl-process")
	 (command (scihub-command (format "download --doi %s" doi)))
	 (buffer (generate-new-buffer "*scihub-download-output*"))
	 (proc (start-process-shell-command process-name buffer command))
	 download-successful filename)
    (message "Trying to download DOI `%s'..." doi)
    (set-process-filter proc
			(lambda (_process output)
			  (when (string-match-p "Successfully download the url" output)
			    (setq download-successful t)
			    (setq filename (scihub-get-pdf-filename output)))))
    (set-process-sentinel proc
			  (lambda (_process signal)
			    (if (and (string= signal "finished\n") download-successful)
				(if callback
				    (funcall callback
					     (file-name-concat scihub-download-directory filename) bibtex-key)
				  (message "File downloaded successfully to `%s'." scihub-download-directory))
			      (user-error "File download failed"))))))

(defun scihub-get-pdf-filename (output)
  "Get the filename of the downloaded PDF from the shell command OUTPUT."
  (string-match "Successfully download the url to: \\(.*?\\.pdf\\)" output)
  (match-string 1 output))

(defun scihub-update-server-list ()
  "Update the list of SciHub servers."
  (interactive)
  (when-let ((output (shell-command-to-string (scihub-command "domain.update"))))
    (message output)))

;;;;; Read DOI

(defun scihub-is-doi-p (string)
  "Return t if STRING is a valid DOI."
  (not (null (string-match scihub-doi-regexp string))))

(defun scihub-read-doi ()
  "Read DOI from user input."
  (let ((doi (read-string "DOI: ")))
    (if (or (scihub-is-doi-p doi)
	    (y-or-n-p (format "`%s' does not look like a valid DOI; proceed anyway?" doi)))
	doi
      (user-error "Aborted"))))

(provide 'scihub)
;;; scihub.el ends here
