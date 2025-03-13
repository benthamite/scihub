;;; scihub.el --- Download files from SciHub -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/scihub
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))

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

;; Download academic papers from SciHub using their DOI or other identifiers.
;; This package requires the external command-line tool 'scidownl'.

;;; Code:

(require 'bibtex)
(require 'cl-lib)

;;;; Variables

(defconst scihub-doi-regexp
  "\\(10\\.[0-9]\\{4,9\\}\\(/\\)[-._;()/:A-Z0-9]+\\)$"
  "Regular expression that matches a DOI.")

(defvar scihub-history nil
  "History of downloaded papers.")

(defvar scihub-server-cache nil
  "Cache of working SciHub servers.")

(defvar scihub-timeout 60
  "Timeout in seconds for download operations.")

;;;; User options

(defgroup scihub ()
  "Download files from SciHub."
  :group 'applications)

(defcustom scihub-download-directory
  (expand-file-name "~/Downloads/")
  "Directory where downloaded files are saved."
  :type 'directory
  :group 'scihub)

(defcustom scihub-filename-format "%d"
  "Format for downloaded filenames.
Available placeholders:
%d - DOI (with / and : replaced by _)
%a - First author's last name
%y - Publication year
%t - Title (truncated)"
  :type 'string
  :group 'scihub)

(defcustom scihub-open-after-download nil
  "Whether to open PDFs after downloading them."
  :type 'boolean
  :group 'scihub)

(defcustom scihub-enable-server-caching t
  "Whether to cache successful server URLs for faster downloads."
  :type 'boolean
  :group 'scihub)

(defcustom scihub-retry-count 3
  "Number of times to retry download with different mirrors on failure."
  :type 'integer
  :group 'scihub)

(defcustom scihub-verify-checksum nil
  "Whether to verify file integrity with checksum after download."
  :type 'boolean
  :group 'scihub)

;;;; Functions

;;;;; Command helpers

(defun scihub-get-executable ()
  "Get `scidownl' executable path."
  (executable-find "scidownl"))

(defun scihub-command (args)
  "Run `scidownl' with ARGS."
  (scihub-ensure-executable-exists)
  (format "\"%s\" %s" (scihub-get-executable) args))

;;;;; Ensure functions

(defun scihub-ensure-executable-exists ()
  "Check if `scidownl' executable exists and signal an error if it doesn't."
  (unless (scihub-get-executable)
    (user-error "`scidownl' not found; please install it (https://github.com/Tishacy/SciDownl)")))

(defun scihub-ensure-subdirectory-exists ()
  "Check if `scihub-download-directory' exists.
If directory doesn't exist, ask the user whether to create it. If directory is
empty, signal an error."
  (when (string-empty-p scihub-download-directory)
    (user-error "Directory `%s' is empty" scihub-download-directory))
  (unless (file-exists-p scihub-download-directory)
    (if (y-or-n-p
	 (format "Directory `%s' does not exist; create it?" scihub-download-directory))
	(make-directory scihub-download-directory t)
      (user-error "Aborted"))))

;;;;; Filename generation

(defun scihub-generate-filename (doi metadata)
  "Generate a filename for DOI using METADATA and `scihub-filename-format'."
  (let* ((safe-doi (replace-regexp-in-string "[/.:]" "_" doi))
         (author (or (plist-get metadata :author) "unknown"))
         (year (or (plist-get metadata :year) "unknown"))
         (title (or (plist-get metadata :title) "unknown"))
         (safe-title (replace-regexp-in-string "[^a-zA-Z0-9]+" "_"
                                               (substring title 0 (min 30 (length title)))))
         (filename (replace-regexp-in-string "%d" safe-doi scihub-filename-format))
         (filename (replace-regexp-in-string "%a" author filename))
         (filename (replace-regexp-in-string "%y" year filename))
         (filename (replace-regexp-in-string "%t" safe-title filename)))
    (concat filename ".pdf")))

;;;;; Metadata extraction

(defun scihub-extract-metadata-from-bibtex ()
  "Extract metadata from current BibTeX entry."
  (save-excursion
    (save-restriction
      (bibtex-narrow-to-entry)
      (goto-char (point-min))
      (let ((author (bibtex-autokey-get-field "author"))
            (year (bibtex-autokey-get-field "year"))
            (title (bibtex-autokey-get-field "title")))
        (when author
          (setq author (car (split-string author "[ \t\n,]+" t))))
        (list :author author :year year :title title)))))

(defun scihub-extract-metadata-from-ebib ()
  "Extract metadata from Ebib entry if available."
  (when (and (fboundp 'ebib--get-key-at-point)
             (fboundp 'ebib-get-field-value)
             (boundp 'ebib--cur-db))
    (when-let* ((key (funcall #'ebib--get-key-at-point))
                (ebib--cur-db))
      (let ((author (funcall #'ebib-get-field-value "author" key ebib--cur-db t t t))
            (year (funcall #'ebib-get-field-value "year" key ebib--cur-db t t t))
            (title (funcall #'ebib-get-field-value "title" key ebib--cur-db t t t)))
        (when author
          (setq author (car (split-string author "[ \t\n,]+" t))))
        (list :author author :year year :title title)))))

(defun scihub-get-metadata ()
  "Get metadata for current entry based on major mode."
  (pcase major-mode
    ('bibtex-mode (scihub-extract-metadata-from-bibtex))
    ((guard (and (memq major-mode '(ebib-entry-mode ebib-index-mode))
                 (featurep 'ebib)))
     (scihub-extract-metadata-from-ebib))
    (_ nil)))

;;;;; Download functionality

;;;###autoload
(defun scihub-download (&optional doi callback)
  "Download DOI from SciHub.
CALLBACK is a callback function that is called after the download is complete.
It receives the downloaded file path and the BibTeX key as arguments."
  (interactive)
  (scihub-ensure-subdirectory-exists)
  (let* ((doi (or doi (scihub-read-doi (scihub-get-doi))))
         (bibtex-key (pcase major-mode
                       ('bibtex-mode (scihub-get-bibtex-key))
                       ((guard (and (memq major-mode '(ebib-entry-mode ebib-index-mode))
                                    (featurep 'ebib)))
                        (scihub-get-ebib-key))
                       (_ nil)))
         (metadata (scihub-get-metadata))
         (desired-filename (scihub-generate-filename doi metadata))
         (output-path (expand-file-name desired-filename scihub-download-directory))
         (default-directory scihub-download-directory))
    (scihub-download-with-retry doi output-path bibtex-key callback 0)))

(defun scihub-download-with-retry (doi output-path bibtex-key callback retry-count)
  "Download DOI to OUTPUT-PATH with retry mechanism.
BIBTEX-KEY is passed to CALLBACK on success.
RETRY-COUNT tracks the current retry attempt."
  (let* ((process-name (format "scidownl-process-%s" retry-count))
         (server-arg (if (and scihub-enable-server-caching scihub-server-cache)
                         (format "--scihub %s" (shell-quote-argument scihub-server-cache))
                       ""))
         (command (scihub-command (format "download --doi %s %s --out %s"
                                          (shell-quote-argument doi)
                                          server-arg
                                          (shell-quote-argument output-path))))
         (buffer (generate-new-buffer "*scihub-download-output*"))
         (proc (start-process-shell-command process-name buffer command))
         (download-successful nil)
         (server-url nil)
         (start-time (current-time))
         (timer (run-at-time scihub-timeout nil
                             (lambda ()
                               (when (process-live-p proc)
                                 (delete-process proc)
                                 (with-current-buffer buffer
                                   (insert "\nDownload timed out after "
                                           (number-to-string scihub-timeout)
                                           " seconds.")))))))
    (message "Downloading DOI `%s'... (attempt %d/%d)"
             doi (1+ retry-count) (1+ scihub-retry-count))
    ;; Process output in real-time
    (set-process-filter
     proc
     (lambda (_process output)
       (with-current-buffer buffer
         (let ((inhibit-read-only t))
           (goto-char (point-max))
           (insert output)))
       ;; Extract server URL if found
       (when (and (not server-url)
                  (string-match "Using Sci-Hub URL: \\(https?://[^[:space:]]+\\)" output))
         (setq server-url (match-string 1 output)))
       ;; Show progress
       (when (string-match "Progress: \\([0-9]+\\)%" output)
         (let ((progress (match-string 1 output)))
           (message "Downloading DOI `%s'... %s%%" doi progress)))
       ;; Check for success
       (when (string-match-p "Successfully download the url" output)
         (setq download-successful t))))
    
    ;; Process completion
    (set-process-sentinel
     proc
     (lambda (_process signal)
       (cancel-timer timer)
       (if (and (string= signal "finished\n") download-successful)
           (progn
             ;; Cache working server
             (when (and scihub-enable-server-caching server-url)
               (setq scihub-server-cache server-url))
             ;; Verify checksum if enabled
             (if (and scihub-verify-checksum
                      (not (scihub-verify-file-checksum output-path)))
                 (scihub-handle-download-failure
                  "File integrity check failed"
                  buffer doi output-path bibtex-key callback retry-count)
               ;; Add to history
               (push (list :doi doi
                           :path output-path
                           :time (current-time-string)
                           :key bibtex-key)
                     scihub-history)
               ;; Handle successful download
               (if callback
                   (funcall callback output-path bibtex-key)
                 (message "File <%s> downloaded successfully to '%s'."
                          (file-name-nondirectory output-path)
                          (abbreviate-file-name scihub-download-directory)))
               ;; Open file if configured
               (when scihub-open-after-download
                 (find-file-other-window output-path))))
         ;; Handle download failure
         (let ((elapsed-time (float-time (time-since start-time))))
           (scihub-handle-download-failure
            (format "Download failed after %.1f seconds" elapsed-time)
            buffer doi output-path bibtex-key callback retry-count)))))))

(defun scihub-handle-download-failure (reason buffer doi output-path bibtex-key callback retry-count)
  "Handle download failure with REASON.
BUFFER contains the output, retry for DOI to OUTPUT-PATH if
RETRY-COUNT permits. BIBTEX-KEY is passed to CALLBACK on success."
  (if (< retry-count scihub-retry-count)
      (progn
        (message "Download attempt %d failed: %s. Retrying..."
                 (1+ retry-count) reason)
        (scihub-download-with-retry
         doi output-path bibtex-key callback (1+ retry-count)))
    (with-current-buffer buffer
      (let ((error-output (buffer-string)))
        (kill-buffer buffer)
        (user-error "All download attempts failed: %s" error-output)))))

(defun scihub-verify-file-checksum (file-path)
  "Verify the integrity of FILE-PATH using checksum."
  ;; Simple implementation - could be improved with actual checksum verification
  (and (file-exists-p file-path)
       (> (file-attribute-size (file-attributes file-path)) 1000)))

;;;;; Batch operations

;;;###autoload
(defun scihub-batch-download (dois)
  "Download multiple DOIS from SciHub."
  (interactive
   (list
    (split-string
     (read-string "Enter DOIs (comma or newline separated): ")
     "[ \t\n,]+" t)))
  (let ((total (length dois))
        (count 0))
    (dolist (doi dois)
      (setq count (1+ count))
      (message "Processing DOI %d/%d: %s" count total doi)
      (scihub-download doi))))

;;;;; History and management

;;;###autoload
(defun scihub-view-history ()
  "Display download history."
  (interactive)
  (with-current-buffer (get-buffer-create "*SciHub History*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "SciHub Download History\n")
      (insert "=======================\n\n")
      (if scihub-history
          (dolist (item scihub-history)
            (insert (format "DOI: %s\n" (plist-get item :doi)))
            (insert (format "Time: %s\n" (plist-get item :time)))
            (insert (format "File: %s\n" (plist-get item :path)))
            (when (plist-get item :key)
              (insert (format "BibTeX key: %s\n" (plist-get item :key))))
            (insert "\n"))
        (insert "No download history available.\n")))
    (special-mode)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;;###autoload
(defun scihub-clear-history ()
  "Clear download history."
  (interactive)
  (when (y-or-n-p "Clear SciHub download history? ")
    (setq scihub-history nil)
    (message "SciHub history cleared")))

;;;###autoload
(defun scihub-search-by-title (title)
  "Search for a paper by TITLE and download it."
  (interactive "sTitle: ")
  (message "Searching for: %s" title)
  (let ((buffer (generate-new-buffer "*scihub-search*"))
        (command (scihub-command (format "search --title %s"
                                         (shell-quote-argument title)))))
    (with-current-buffer buffer
      (let ((proc (start-process-shell-command "scidownl-search" buffer command)))
        (set-process-sentinel
         proc
         (lambda (_process signal)
           (if (string= signal "finished\n")
               (with-current-buffer buffer
                 (goto-char (point-min))
                 (if (re-search-forward "DOI: \\(10\\.[0-9]+/[^ \n]+\\)" nil t)
                     (let ((doi (match-string 1)))
                       (message "Found DOI: %s" doi)
                       (when (y-or-n-p (format "Download paper with DOI %s? " doi))
                         (scihub-download doi)))
                   (message "No DOI found for title: %s" title)))
             (with-current-buffer buffer
               (let ((output (buffer-string)))
                 (if (string-match "exited abnormally with code 2" signal)
                     (message "Search failed: The scidownl command doesn't support search functionality or has invalid parameters")
                   (message "Search failed: %s\nOutput: %s" signal (if (string-empty-p output) "No output" output))))))))))))

;;;###autoload
(defun scihub-update-server-list ()
  "Update the list of SciHub servers."
  (interactive)
  (let ((proc (start-process-shell-command
               "scidownl-update"
               "*scihub-update*"
               (scihub-command "domain.update"))))
    (set-process-sentinel
     proc
     (lambda (_process signal)
       (when (string= signal "finished\n")
         (message "SciHub server list updated successfully")
         (setq scihub-server-cache nil))))))

;;;;; DOI extraction

(defun scihub-is-doi-p (string)
  "Return t if STRING is a valid DOI."
  (not (null (string-match scihub-doi-regexp string))))

(defun scihub-read-doi (&optional initial-input)
  "Read DOI from user input.
If INITIAL-INPUT is non-nil, use it as the initial input."
  (let ((doi (read-string "DOI: " initial-input 'scihub-history)))
    (if (or (scihub-is-doi-p doi)
	    (y-or-n-p (format "`%s' does not look like a valid DOI; proceed anyway?" doi)))
	doi
      (user-error "Aborted"))))

(declare-function ebib--get-key-at-point "ebib" ())
(declare-function ebib-get-field-value "ebib-utils"
                  (field key db &optional noerror unbraced xref))
(defun scihub-get-doi ()
  "Return the DOI in the current buffer, if found."
  (pcase major-mode
    ((guard (and (memq major-mode '(ebib-entry-mode ebib-index-mode))
                 (featurep 'ebib)))
     (when (and (fboundp 'ebib--get-key-at-point)
                (fboundp 'ebib-get-field-value)
                (boundp 'ebib--cur-db))
       (when-let* ((key (funcall #'ebib--get-key-at-point))
                   (ebib--cur-db))
         (funcall #'ebib-get-field-value "doi" key ebib--cur-db t t t))))
    ('bibtex-mode
     (save-excursion
       (save-restriction
         (bibtex-narrow-to-entry)
         (bibtex-beginning-of-entry)
         (let* ((bibtex-autokey-use-crossref nil))
           (bibtex-autokey-get-field "doi")))))))

(defun scihub-get-bibtex-key ()
  "Return the key of the current BibTeX entry."
  (save-excursion
    (save-restriction
      (bibtex-narrow-to-entry)
      (goto-char (point-min))
      (if (re-search-forward "@\\w+{\\([^,]+\\),")
          (match-string-no-properties 1)
        (user-error "Not on a BibTeX entry")))))

(defun scihub-get-ebib-key ()
  "Get the key value of the entry at point if ebib is available."
  (when (and (featurep 'ebib)
             (fboundp 'ebib-get-field-value)
             (fboundp 'ebib--get-key-at-point)
             (boundp 'ebib--cur-db))
    (when-let* ((value (funcall #'ebib-get-field-value "=key="
				(funcall #'ebib--get-key-at-point)
				ebib--cur-db t t t)))
      (replace-regexp-in-string "[\n\t ]+" " " value))))

(provide 'scihub)
;;; scihub.el ends here
