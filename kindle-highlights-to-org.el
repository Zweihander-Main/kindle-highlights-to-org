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
    (let ((full-file-path (or path
                         (read-file-name "Select 'My Clippings.txt' file."))))
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

(defun kindle-highlights-to-org--main ()
  "Placeholder."
  ;; Reads the file into a variable
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
