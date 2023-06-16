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

(defconst kindle-highlights-to-org--note-seperator "=========="
  "Separator used in metadata section of 'My Clippings.txt' file.")

(defconst kindle-highlights-to-org--metadata-separator "|"
  "Separator used in metadata section of 'My Clippings.txt' file.")

(defconst kindle-highlights-to-org--regex-title-line "^\\(.*\\)(\\(.*\\))$"
  "Regex to match title line in `My Clippings.txt' file.")

(defconst kindle-highlights-to-org--regex-metadata-line
  "^-\\s*\\S* (\\S+) (.*)[\\s|]+Added on\\s+(.+)$"
  "Regex to match metadata line in `My Clippings.txt' file.")

(defconst kindle-highlights-to-org--regex-loc-data "[Ll]ocation \\([0-9\-]+\\)"
  "Regex to match location data in `My Clippings.txt' file.")

(defconst kindle-highlights-to-org--regex-page-data "[pP]age \\([0-9\\-]+\\)"
  "Regex to match page data in `My Clippings.txt' file.")

(defconst kindle-highlights-to-org--regex-date-data "Added on \\(.+\\)$"
  "Regex to match date data in `My Clippings.txt' file.")

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
  "Takes STR string, trim whitespace, and remove BOM mark(s).

Needed for titles from Kindle generated files which may add in BOM<FEFF> marks
on some but not all titles (possibly related to files spanning multiple firmware
versions).  Additionally, BOM marks may be found after the first line.  Titles
are used as keys for note hash table and are compared using 'equal so BOM marks
matter."
  (s-trim (remove
           (char-from-name "ZERO WIDTH NO-BREAK SPACE")
           str)))


;; TODO write tests for this
(defun kindle-highlights-to-org--parse-time (str)
  "Takes STR, run it through `parse-time-string' and add 12 hours if PM."
  (let* ((time (parse-time-string str))
         (hour (nth 2 time)))
    (cond ((and (s-contains? "PM" str t) (< hour 12)) (setf (nth 2 time) (+ hour 12)))
          ((and (s-contains? "AM" str t) (= hour 12)) (setf (nth 2 time) 0)))
    time))


(defun kindle-highlights-to-org--process-file (path)
  "Takes an absolute PATH and return a hash table with note data.
Notes are grouped by the book each note came from.

Taken for granted:
- PATH is a valid readable file.
- Contents of file are in format:
TITLE (AUTHOR)
?PAGE? | ?LOC? | DATE
CONTENTS
==========

Format of returned hash table:
KEY: TITLE (AUTHOR)
VALUE:
    List of plists, each with:
        TITLE: Title of book
        AUTHOR: Author of book
        META-LINE: Metadata line from file
        DATE: Date note was added in Emacs `parse-time-string' format
        PAGE: Page number of note or nil
        LOC: Location number of note or nil
        CONTENTS: Notedata as one string."
  (let ((bookhash (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents path)
      ;; Break into list of notes, each a string
      (let* ((file-contents (buffer-substring-no-properties (point-min) (point-max)))
             (data-list (split-string file-contents kindle-highlights-to-org--note-seperator))
             (filtered-data (mapc #'kindle-highlights-to-org--normalize-string data-list)))
        ;; For each filtered-data item, match title and metadata
        (dolist (item filtered-data)
          (unless (s-matches? "^[ \n]+$" item) ; skip no-content items
            (let* ((lines (s-split "\n" item t))
                   (title-line (nth 0 lines))
                   (title (s-trim (nth 1 (s-match kindle-highlights-to-org--regex-title-line
                                                  title-line))))
                   (author (s-trim (nth 2 (s-match kindle-highlights-to-org--regex-title-line
                                                   title-line))))
                   (metadata-line (nth 1 lines))
                   (metadata-blocks (s-split
                                     kindle-highlights-to-org--metadata-separator
                                     metadata-line t))
                   (date (kindle-highlights-to-org--parse-time
                          (nth 1 (s-match kindle-highlights-to-org--regex-date-data
                                          metadata-line))))
                   (page-block (seq-find
                                (lambda (str)
                                  (s-matches?
                                   kindle-highlights-to-org--regex-page-data
                                   str))
                                metadata-blocks))
                   (loc-block (seq-find
                               (lambda (str)
                                 (s-matches?
                                  kindle-highlights-to-org--regex-loc-data
                                  str))
                               metadata-blocks)))
              (let ((page (if page-block
                              (nth 1 (s-match kindle-highlights-to-org--regex-page-data
                                              page-block))
                            nil))
                    (loc (if loc-block
                             (nth 1 (s-match kindle-highlights-to-org--regex-loc-data
                                             loc-block))
                           nil))
                    (contents (s-join "\n" (nthcdr 2 lines))))
                (when kindle-highlights-to-org-verbose
                  (message "============\nEntry Found:")
                  (message "Title Line: %s" title-line)
                  (message "Parsed Title: %s" title)
                  (message "Parsed Author: %s" author)
                  (message "Metadata Line: %s" metadata-line)
                  (message "Parsed Date: %s" date)
                  (message "Parsed Page: %s" page)
                  (message "Parsed Location: %s" loc))
                ;; Only add book to hash table if not already there
                (unless (gethash title-line bookhash)
                  (puthash title-line '() bookhash))
                ;; Add note to book hash table
                (let ((curnotes (gethash title-line bookhash))
                      (note-plist (list :title title
                                        :author author
                                        :meta-line metadata-line
                                        :date date
                                        :page page
                                        :loc loc
                                        :contents contents)))
                  ;; Add to notes and sort: likely poor performance
                  (setq curnotes (append curnotes (list note-plist)))
                  (setq curnotes (sort curnotes
                                       (lambda (note1 note2)
                                         (time-less-p (plist-get note1 :date)
                                                      (plist-get note2 :date)))))
                  (puthash title-line curnotes bookhash))))))
        bookhash))))

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
     (dolist (note (reverse notes))
       (let ((meta-line (plist-get note :meta-line))
             (contents (plist-get note :contents)))
         (save-excursion
           (org-insert-subheading nil)
           (when (stringp contents)
             (org-edit-headline contents))
           (end-of-line)
           (newline-and-indent)
           (when (stringp meta-line)
             (insert meta-line))))))
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
