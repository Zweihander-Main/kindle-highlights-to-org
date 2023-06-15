;;; kindle-highlights-to-org.el --- Convert your Kindle highlights to an org tree-*-lexical-binding:t-*-

;; Copyright (C) 2021, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder
;; Keywords: outlines convenience hardware
;; Homepage: https://github.com/Zweihander-Main/kindle-highlights-to-org
;; Version: 0.0.2
;; Package-Requires: ((emacs "26.1") (s "1.12.0"))

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

;; Takes a `My Clippings.txt` file from a Kindle and converts it into a relative
;; tree with format:

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

(require 's)
(require 'org)

(defgroup kindle-highlights-to-org nil
  "Customization for 'kindle-highlights-to-org' package."
  :group 'org
  :prefix "kindle-highlights-to-org-")

(defvar kindle-highlights-to-org-verbose nil
  "If non-nil, will display progress messages.")

(defun kindle-highlights-to-org--get-file-path (&optional path)
  "Asks the user for a file to read or read the full path from PATH.
Return full path of the file.  Performs basic checking on the file to confirm
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

(defun kindle-highlights-to-org--normalize-string (str)
  "Takes STR string, trim whitespace, and remove BOM mark.

Needed for titles from Kindle generated files which may add in BOM<FEFF> marks
on some but not all titles (possibly related to files spanning multiple firmware
versions).  Additionally, BOM marks may be found after the first line.  Titles
are used as keys for note hash table and are compared using 'equal so BOM marks
matter."
  (s-trim (remove
           (char-from-name "ZERO WIDTH NO-BREAK SPACE")
           str)))

;;; Format of My Clippings.txt file:
;;;     - 5 lines for each note, regardless of length
;;; 1. TITLE
;;; 2. METADATA
;;;     - one line with blocks separated by | char
;;;     - some amount of blocks, usually 2-3 but 4 might be possible
;;;     - last block should be time added
;;;     - can be broken apart but identifying blocks is difficult due to
;;;       language differences eg 'Added on' is only for English
;;; 3. BLANK LINE
;;; 4. NOTEDATA
;;;     - appears to always all be on one line
;;;     - new lines are not preserved in any way
;;; 5. SEPERATOR
;;;     - 10 equal signs
;;;     - ==========
;;;     - is at the end of the note block, not the start (file ends with one)

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
        (let* ((title (kindle-highlights-to-org--normalize-string
                       (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position))))
               (metadata (progn (forward-line 1)
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))))
               (notedata (progn (forward-line 2) ; skip blank
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))))
          (unless (gethash title bookhash)
            (puthash title '() bookhash))
          (let ((curnotes (gethash title bookhash))
                (note-plist (list :meta metadata
                                  :contents notedata)))
            (setq curnotes (append curnotes (list note-plist))) ; perf trap
            (puthash title curnotes bookhash))
          (forward-line 1)
          (end-of-line)))
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
     (let ((note))
       (dolist (note notes)
         (let ((meta (plist-get note :meta))
               (contents (plist-get note :contents)))
           (save-excursion
             (org-insert-subheading nil)
             (when (stringp contents)
               (org-edit-headline contents))
             (end-of-line)
             (newline-and-indent)
             (when (stringp meta)
               (insert meta)))))))
   bookhash))

;;;###autoload
(defun kindle-highlights-to-org ()
  "Ask for a file and convert it into an org tree relative to the current heading."
  (interactive)
  (catch 'exit
    (unless (eq major-mode 'org-mode)
      (throw 'exit "Not in org-mode, exiting."))
    (let* ((path (kindle-highlights-to-org--get-file-path))
           (bookhash (kindle-highlights-to-org--process-file path)))
      (kindle-highlights-to-org--insert-as-org bookhash))))

(provide 'kindle-highlights-to-org)

;; Local Variables:
;; coding: utf-8
;; End:

;;; kindle-highlights-to-org.el ends here
