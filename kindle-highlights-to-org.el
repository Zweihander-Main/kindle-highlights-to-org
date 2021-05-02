;;; kindle-highlights-to-org.el --- Convert your Kindle highlights to an org tree
;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2021, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder
;; Keywords: org-mode, kindle
;; Homepage: https://github.com/Zweihander-Main/kindle-highlights-to-org
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Takes a `My Clippings.txt` file from a Kindle and converts it into a relative tree with format:

;; * Book Title
;; ** Note contents
;; - Metadata (location and time)
;; ** Note contents
;; - Metadata (location and time)
;;
;; Usage:
;;
;; 1. Add `kindle-highlights-to-org.el` into your load path.
;; 2. Position your cursor in an org file where you want the data inserted
;;    (tree will be inserted relative to current heading).
;; 3. Call `(kindle-highlights-to-org)` and select your `My Clippings.txt` file,
;;    usually located in the `/documents` folder of your USB-connected Kindle.
;;
;; Note that the original file isn't modified and the tree will be built from
;; scratch every time so it may be worth deleting it to force a new file to be
;; created after you've pulled the data.
;;
;;; Code:

;; TODO: Make sure running in org buffer

(require 's)

(defun kindle-highlights-to-org--get-file-path (&optional path)
  "Asks the user for a file to read or read the full path from PATH.
Return full path of the file. Performs basic checking on the file to confirm
it can be read and the name is what the user intends."
  (interactive "P")
  (catch 'exit
    (let ((full-file-path (expand-file-name
                           (or path
                               (read-file-name "Select 'My Clippings.txt' file.")))))
      ;; Check filename, prompt if it seems wrong
      (unless (s-ends-with? "My Clippings.txt" full-file-path)
        (let ((proceed
               (y-or-n-p (concat "File doesn't match 'My Clippings.txt'."
                                 "\n"
                                 "are you sure this is the right file? "))))
          (unless proceed
            (throw 'exit "Not the right file, cancelling."))))
      ;; Check file exists
      (unless (file-exists-p full-file-path)
        (throw 'exit "File doesn't exist."))
      ;; Check file can be read
      (unless (file-readable-p full-file-path)
        (throw 'exit "File can't be read."))
      ;; Return path
      full-file-path)))

;;; Format notes:
;;;     - 5 lines for each note, regardless of length
;;; 1. TITLE
;;; 2. METADATA
;;;     - separated by |
;;;     - sometimes 2 blocks, sometimes 3
;;;     - last block should be time added
;;; 3. BLANK LINE
;;; 4. NOTEDATA
;;;     - appears to always all be on one line
;;;     - new lines are not preserved in any way
;;; 5. SEPERATOR
;;;     - 10 equal signs
;;;     - ==========

(defun kindle-highlights-to-org--process-file (path)
  "Takes an absolute PATH and return a hash table with note data.
Notes are grouped by the book each note came from.
Format of returned hash table:
KEY: TITLE
VALUE:
    List of plists, each with:
        META: Metadata including location and highlight date as string.
        CONTENTS: Notedata as one string (newlines aren't saved by the device)."
  (let ((bookhash (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents path)
      (while (search-forward "==========" nil t)
        (forward-line -4) ; to title
        ;; Grab just the line data without newlines at the end
        (let* ((title (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position)))
               (metadata (progn (forward-line 1)
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))))
               (notedata (progn (forward-line 2)
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))))
          ;; TODO break up metadata
          (unless (gethash title bookhash)
            (puthash title '() bookhash))
          (let ((curnotes (gethash title bookhash))
                (note-plist (list :meta metadata
                                  :contents notedata)))
            (push note-plist curnotes)
            (puthash title curnotes bookhash))
          (forward-line 1)
          (end-of-line))) ; end of note block
      bookhash)))

(defun kindle-highlights-to-org--insert-as-org (bookhash)
  "Takes BOOKHASH and outputs org tree.

Org tree should respect current relative heading and is in the format:
* Book
** Note contents
Metadata
** Note contents
Metadata

BOOKHASH is from the `kindle-highlights-to-org--process-file' function."
  (maphash
   (lambda (book notes)
     (org-insert-heading-respect-content t)
     (org-edit-headline book)
     (dolist (note notes)
       (let ((meta (plist-get note :meta))
             (contents (plist-get note :contents)))
         (save-excursion
           (org-insert-subheading nil)
           (org-edit-headline contents)
           (end-of-line)
           (newline-and-indent)
           (insert meta)))))
   bookhash))

(defun kindle-highlights-to-org ()
  "Ask for a file and convert it into an org tree relative to the current heading."
  (let* ((path (kindle-highlights-to-org--get-file-path))
         (bookhash (kindle-highlights-to-org--process-file path)))
    (kindle-highlights-to-org--insert-as-org bookhash)))

(provide 'kindle-highlights-to-org)

;; Local Variables:
;; coding: utf-8
;; End:

;;; kindle-highlights-to-org.el ends here
