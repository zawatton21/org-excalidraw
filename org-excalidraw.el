;;; org-excalidraw.el --- Tools for working with excalidraw drawings -*- lexical-binding: t; -*-
;; Copyright (C) 2022 David Wilson

;; Author:  David Wilson <wdavew@gmail.com>
;; URL: https://github.com/wdavew/org-excalidraw
;; Created: 2022
;; Version: 0.1.0
;; Keywords: convenience, outlines
;; Package-Requires: ((org "9.3") (emacs "26.1"))

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;; org-excalidraw.el is a package to for embedding excalidraw drawings into Emacs.
;;; it adds an org-mode link type for excalidraw files to support inline display
;;; and opening the diagrams from Emacs for editing.

;;; Code:
(require 'cl-lib)
(require 'filenotify)
(require 'org-id)
(require 'ol)

(defun org-excalidraw--default-base ()
  "Get default JSON template used for new excalidraw files."
  "{
    \"type\": \"excalidraw\",
    \"version\": 2,
    \"source\": \"https://excalidraw.com\",
    \"elements\": [],
    \"appState\": {
      \"gridSize\": null,
      \"viewBackgroundColor\": \"#ffffff\"
    },
    \"files\": {}
  }
")

(defgroup org-excalidraw nil
  "Customization options for org-excalidraw."
  :group 'org
  :prefix "org-excalidraw-")

(defcustom org-excalidraw-directory-excal "~/org-excalidraw/excal"
  "Directory to store excalidraw files."
  :type 'string
  :group 'org-excalidraw)

(defcustom org-excalidraw-directory-svg "~/org-excalidraw/svg"
  "Directory to store svg files."
  :type 'string
  :group 'org-excalidraw)

(defcustom org-excalidraw-directory-org "~/org-excalidraw/org"
  "Directory to store svg files."
  :type 'string
  :group 'org-excalidraw)

(defcustom org-excalidraw-base (org-excalidraw--default-base)
  "JSON string representing base excalidraw template for new files."
  :type 'string
  :group 'org-excalidraw)

(defcustom org-excalidraw-use-org-id t
  "Whether to use org-id for file names."
  :type 'boolean
  :group 'org-excalidraw)

(defcustom org-excalidraw-file-prefix "Drawing-"
  "Prefix for excalidraw file names."
  :type 'string
  :group 'org-excalidraw)

(defcustom org-excalidraw-file-name (format-time-string "%Y%m%d%H%M%S")
  "Name for excalidraw file names when not using org-id."
  :type 'string
  :group 'org-excalidraw)

(defcustom org-excalidraw-attr-org-width "50%"
  "Width attribute for org images"
  :type 'string
  :group 'org-excalidraw)

(defcustom org-excalidraw-use-org-roam nil
  "Whether to integrate with org-roam for file creation and linking."
  :type 'boolean
  :group 'org-excalidraw)

(defun org-excalidraw--get-filename ()
  "Get filename for a new excalidraw file."
  (if org-excalidraw-use-org-id
      (format "%s.excalidraw" (org-id-uuid))
    (format "%s%s.excalidraw" org-excalidraw-file-prefix org-excalidraw-file-name)))


(defun org-excalidraw--validate-excalidraw-file (path)
  "Validate the excalidraw file at PATH is usable."
  (unless (string-suffix-p ".excalidraw" path)
    (error
     "Excalidraw file must have .excalidraw extension")))

(defun org-excalidraw--shell-cmd-to-svg (path)
  "Construct shell cmd for converting excalidraw file with PATH to svg."
  (if (eq system-type 'windows-nt)
      (list "cmd" "/c" "npx" "excalidraw_export" path)
    (list "excalidraw_export" path)))


(defun org-excalidraw--shell-cmd-open (path os-type)
  "Construct shell cmd to open excalidraw file with PATH for OS-TYPE."
  (cond ((eq os-type 'darwin) (concat "open " (shell-quote-argument path)))
        ((eq os-type 'windows-nt) (concat "cmd /c start \"\" " (format "\"%s\"" path)))
        (t (concat "xdg-open " (shell-quote-argument path)))))

(defun org-excalidraw--open-file-from-svg (path)
  "Open corresponding .excalidraw file for svg located at PATH."
  (let ((excal-file-path (string-remove-suffix ".svg" path)))
    (org-excalidraw--validate-excalidraw-file excal-file-path)
    (shell-command (org-excalidraw--shell-cmd-open excal-file-path system-type))))

(defun find-excalidraw-file-and-overwrite-json (org-buffer-name)
  "Find an excalidraw file that matches the name of the given org buffer and overwrite the JSON source block with its content."
  (interactive)
  (let* ((file-name-base (file-name-sans-extension org-buffer-name))  ;; '.org' を除去
         (excalidraw-file-path (file-truename (concat (file-name-as-directory org-excalidraw-directory-excal) file-name-base)))
         (file-exists (file-exists-p excalidraw-file-path)))
    (if file-exists
        (let* ((excalidraw-content (with-temp-buffer
                                     (insert-file-contents excalidraw-file-path)
                                     (buffer-string))))
          (save-excursion
            (goto-char (point-min))
            (if (search-forward "#+BEGIN_SRC json" nil t)
                (progn
                  (forward-line)
                  (let ((start (point))
                        (end (if (search-forward "#+END_SRC" nil t)
                                 (progn (forward-line -1) (end-of-line) (point))
                               (point-max))))
                    (delete-region start end)
                    (insert excalidraw-content)
                    (message "JSON code block in file %s has been overwritten with content from %s." org-buffer-name excalidraw-file-path)))
              (message "No JSON code block found in file: %s" org-buffer-name))))
      (message "Excalidraw file not found. Searched for file: %s in directory: %s" excalidraw-file-path org-excalidraw-directory-excal))))


(defun org-excalidraw--handle-file-change (event)
  "Handle file update EVENT to convert files to svg and update JSON blocks in corresponding org files."
  ;(message "org-excalidraw--handle-file-change called")
  ;(message "Detected file change event: %s" event)
  (when (or (string-equal (cadr event) "renamed")
            (string-equal (cadr event) "changed"))
    (let ((filename (cadddr event)))
      (when (string-suffix-p ".excalidraw" filename)
        ;; Convert the excalidraw file to SVG using the shell command
        (let* ((cmd-parts (org-excalidraw--shell-cmd-to-svg filename)))
          (message "Shell command: %s" (mapconcat 'identity cmd-parts " "))
          (apply 'start-process "excalidraw-convert" nil cmd-parts))
        ;; Find the corresponding org file
        (let* ((excalidraw-file-base-name (file-name-sans-extension (file-name-nondirectory filename)))
               (org-file-path (concat (file-name-as-directory org-excalidraw-directory-org) excalidraw-file-base-name ".excalidraw.org")))
          (if (file-exists-p org-file-path)
              (progn
                (message "Processing org file: %s" org-file-path)
                (save-current-buffer
                  (set-buffer (find-file-noselect org-file-path))
                  (find-excalidraw-file-and-overwrite-json (buffer-name))
                  (save-buffer)))
            (message "Org file not found. Searched for file: %s in directory: %s" org-file-path org-excalidraw-directory-org)))))))


;;;###autoload
(defun org-excalidraw-create-drawing ()
  "Create an excalidraw drawing and insert an 'org-mode' link to it at Point."
  (interactive)
  (let* ((filename (org-excalidraw--get-filename))
         (path-excal (expand-file-name filename org-excalidraw-directory-excal))
         (path-svg (expand-file-name (concat filename ".svg") org-excalidraw-directory-svg))
         (excalidraw-link (format "[[file:%s]]" path-excal))
         (svg-link (format "[[file:%s]]" path-svg)))
    (org-excalidraw--validate-excalidraw-file path-excal)
    (with-temp-file path-excal (insert org-excalidraw-base))
    (start-process "open-excalidraw" nil (org-excalidraw--shell-cmd-open path-excal system-type))))


;;;###autoload
(defun org-excalidraw-initialize ()
  "Setup excalidraw.el. Call this after 'org-mode initialization."
  (interactive)
  (unless (and (file-directory-p org-excalidraw-directory-excal) (file-directory-p org-excalidraw-directory-svg))
    (error
     "Excalidraw directories %s or %s do not exist"
     org-excalidraw-directory-excal org-excalidraw-directory-svg))
  (file-notify-add-watch org-excalidraw-directory-excal '(change) 'org-excalidraw--handle-file-change))

(provide 'org-excalidraw)
;;; org-excalidraw.el ends here
