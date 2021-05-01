;;; kindle-highlights-to-org.el --- Convert your Kindle highlights to an org tree
;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2021, Zweihänder
;;
;; Version: 0.0.1
;;
;;; Commentary:

;; Convert your Kindle highlights to an org tree.
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
        NOTE: Notedata as one string (newlines aren't saved by the device)."
  (let ((bookhash (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents path)
      (while (search-forward "==========" nil t)
        (forward-line -4) ; to title
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
                                  :note notedata)))
            (push note-plist curnotes)
            (puthash title curnotes bookhash))
          (forward-line 1)
          (end-of-line))) ; end of note block
      bookhash)))

(defun kindle-highlights-to-org--main ()
  "Placeholder."
  ;; Parses the file into note blocks
  ;; Creates a books set-like list
  ;; Loops through blocks
  ;; Parses metadata and note data from each block
  ;; If the book doesn't exist in the book list, add it
  ;; Add block data to book in book list
  ;; For each book in book list, create a book org heading
  ;; Under each book org heading, create a note heading for each note
  ;; Add in note metadata under each note
  )

(provide 'kindle-highlights-to-org)

;; Local Variables:
;; coding: utf-8
;; End:

;;; kindle-highlights-to-org.el ends here
