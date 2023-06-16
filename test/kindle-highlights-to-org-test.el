;;; kindle-highlights-to-org-test.el --- Tests for kindle-highlights-to-org-*-lexical-binding:t-*-

;; Copyright (C) 2021, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder
;; Keywords: outlines
;; Homepage: https://github.com/Zweihander-Main/kindle-highlights-to-org
;; Version: 0.0.2
;; Package-Requires: ((emacs "26.1"))

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

;; Tests for kindle-highlights-to-org.el

;;; Code:

(require 'buttercup)
(require 'f)
(require 'kindle-highlights-to-org)

(defconst kindle-highlights-to-org-test-test-directory
  "./test"
  "The test directory.")

(defconst kindle-highlights-to-org-test-fixtures-directory
  (f-join kindle-highlights-to-org-test-test-directory "./fixtures")
  "The fixtures directory.")

(defconst kindle-highlights-to-org-test-main-fixture
  (f-join kindle-highlights-to-org-test-fixtures-directory "./My Clippings.txt")
  "The main My Clippings.txt fixture.")

(defconst kindle-highlights-to-org-test-main-fixture-hash
  #s(hash-table
     size 65
     test equal
     rehash-size 1.5
     rehash-threshold 0.8125
     data ("The First Book (Author First)"
           ((:title "The First Book" :author "Author First" :meta-line "- Your Highlight on Location 3776-3778 | Added on Thursday, August 16, 2018 10:47:12 PM" :date
             (12 47 22 16 8 2018 4 -1 nil)
             :page nil :loc "3776-3778" :contents "This is the first note.")
            (:title "The First Book" :author "Author First" :meta-line "- Your Highlight on page 43 | Location 558-559 | Added on Friday, August 17, 2018 3:48:30 AM" :date
             (30 48 3 17 8 2018 5 -1 nil)
             :page "43" :loc "558-559" :contents "This is the third note.")
            (:title "The First Book" :author "Author First" :meta-line "- Your Highlight on page 43 | Location 558-559 | Added on Friday, August 19, 2018 3:48:30 AM" :date
             (30 48 3 19 8 2018 5 -1 nil)
             :page "43" :loc "558-559" :contents "This is the fourth note with\na line break."))
           "The Sec (o) nd Book (Author Second)"
           ((:title "The Sec (o) nd Book" :author "Author Second" :meta-line "- Your Highlight on page 43 | Location 558-559 | Added on Friday, August 17, 2018 3:48:30 AM" :date
             (30 48 3 17 8 2018 5 -1 nil)
             :page "43" :loc "558-559" :contents "This is the second note."))))
  "The hash table equivalent of the main fixture.")

(defun kindle-highlights-to-org-test-hash-equal (hash1 hash2)
  "Compare HASH1 to HASH2 to see whether they are equal."
  (and (= (hash-table-count hash1)
          (hash-table-count hash2))
       (catch 'flag (maphash (lambda (x y)
                               (or (equal (gethash x hash2) y)
                                   (throw 'flag nil)))
                             hash1)
              (throw 'flag t))))

(describe "The get-file-path function"
  (before-each
    (spy-on 'read-file-name)
    (spy-on 'y-or-n-p))

  (it "prompts for a file if none is given"
    (spy-on 'read-file-name :and-return-value kindle-highlights-to-org-test-main-fixture)
    (kindle-highlights-to-org--get-file-path)
    (expect 'read-file-name :to-have-been-called))

  (it "doesn't prompt for a file if a path is given"
    (kindle-highlights-to-org--get-file-path kindle-highlights-to-org-test-main-fixture)
    (expect 'read-file-name :not :to-have-been-called))

  (it "asks the user if the supplied file doesn't match My Clippings.txt"
    (kindle-highlights-to-org--get-file-path "./test/fixtures/test.txt")
    (expect 'y-or-n-p :to-have-been-called))

  (it "doesn't ask the user if the supplied file matches My Clippings.txt"
    (kindle-highlights-to-org--get-file-path kindle-highlights-to-org-test-main-fixture)
    (expect 'y-or-n-p :not :to-have-been-called))

  (it "checks file exists"
    (expect
     (kindle-highlights-to-org--get-file-path "./test/nope/My Clippings.txt")
     :to-equal "File doesn't exist."))

  (it "returns an expanded path from a relative one"
    (expect
     (kindle-highlights-to-org--get-file-path kindle-highlights-to-org-test-main-fixture)
     :to-equal (expand-file-name kindle-highlights-to-org-test-main-fixture))))

(describe "The normalize-string function"
  :var ((bom-test (kindle-highlights-to-org--normalize-string
                   (concat
                    (string (char-from-name "ZERO WIDTH NO-BREAK SPACE"))
                    "Test"))))

  (it "should retain normal strings"
    (expect
     (kindle-highlights-to-org--normalize-string "Test")
     :to-equal "Test"))

  (it "should trim extra whitespace"
    (expect
     (kindle-highlights-to-org--normalize-string " Test ")
     :to-equal "Test"))

  (it "should remove BOM marks"
    (expect bom-test
            :to-equal "Test")
    (expect bom-test
            :not :to-equal
            (concat
             (string (char-from-name "ZERO WIDTH NO-BREAK SPACE"))
             "Test"))))

(describe "The process-file function return value"
  ;; Check grouping, reversing, metadata
  (it "should group based on book title"
    (expect (hash-table-keys
             (kindle-highlights-to-org--process-file
              (expand-file-name kindle-highlights-to-org-test-main-fixture)))
            :to-have-same-items-as
            '("The First Book (Author First)"
              "The Sec (o) nd Book (Author Second)")))

  (it "should put the books in the original order"
    (expect (car (hash-table-keys
                  (kindle-highlights-to-org--process-file
                   (expand-file-name kindle-highlights-to-org-test-main-fixture))))
            :to-equal "The First Book (Author First)"))

  (it "should pull all the notes"
    (expect (mapcar
             (lambda (value)
               (plist-get value :contents))
             (-flatten-n 1 (hash-table-values
                            (kindle-highlights-to-org--process-file
                             (expand-file-name kindle-highlights-to-org-test-main-fixture)))))
            :to-have-same-items-as
            '("This is the first note."
              "This is the second note."
              "This is the third note."
              "This is the fourth note with\na line break.")))

  (it "should put the notes in the right order"
    (expect (plist-get (car (-flatten-n 1 (hash-table-values
                                           (kindle-highlights-to-org--process-file
                                            (expand-file-name kindle-highlights-to-org-test-main-fixture)))))
                       :contents)
            :to-equal "This is the first note."))


  (it "should pull all the titles"
    (expect (mapcar
             (lambda (value)
               (plist-get value :title))
             (-flatten-n 1 (hash-table-values
                            (kindle-highlights-to-org--process-file
                             (expand-file-name kindle-highlights-to-org-test-main-fixture)))))
            :to-have-same-items-as
            '("The First Book"
              "The First Book"
              "The First Book"
              "The Sec (o) nd Book")))

  (it "should pull all the authors"
    (expect (mapcar
             (lambda (value)
               (plist-get value :author))
             (-flatten-n 1 (hash-table-values
                            (kindle-highlights-to-org--process-file
                             (expand-file-name kindle-highlights-to-org-test-main-fixture)))))
            :to-have-same-items-as
            '("Author First"
              "Author First"
              "Author First"
              "Author Second")))

  (it "should pull all the dates"
    (expect (mapcar
             (lambda (value)
               (plist-get value :date))
             (-flatten-n 1 (hash-table-values
                            (kindle-highlights-to-org--process-file
                             (expand-file-name kindle-highlights-to-org-test-main-fixture)))))
            :to-have-same-items-as
            '((12 47 22 16 8 2018 4 -1 nil)
              (30 48 3 17 8 2018 5 -1 nil)
              (30 48 3 19 8 2018 5 -1 nil)
              (30 48 3 17 8 2018 5 -1 nil))))

  (it "should pull all the page numbers"
    (expect (mapcar
             (lambda (value)
               (plist-get value :page))
             (-flatten-n 1 (hash-table-values
                            (kindle-highlights-to-org--process-file
                             (expand-file-name kindle-highlights-to-org-test-main-fixture)))))
            :to-have-same-items-as
            '(nil
              "43"
              "43"
              "43")))

  (it "should pull all the location numbers"
    (expect (mapcar
             (lambda (value)
               (plist-get value :loc))
             (-flatten-n 1 (hash-table-values
                            (kindle-highlights-to-org--process-file
                             (expand-file-name kindle-highlights-to-org-test-main-fixture)))))
            :to-have-same-items-as
            '("3776-3778"
              "558-559"
              "558-559"
              "558-559")))

  (it "should put the dates in the right order"
    (expect (plist-get (car (car (hash-table-values
                                  (kindle-highlights-to-org--process-file
                                   (expand-file-name kindle-highlights-to-org-test-main-fixture)))))
                       :date)
            :to-equal
            '(12 47 22 16 8 2018 4 -1 nil))))

(describe "The insert-as-org function"
  :var ((from-process-file
         (progn
           (with-temp-buffer
             (org-mode)
             (kindle-highlights-to-org--insert-as-org
              (kindle-highlights-to-org--process-file
               (expand-file-name kindle-highlights-to-org-test-main-fixture)))
             (buffer-string))))
        (from-fixture-hash
         (progn
           (with-temp-buffer
             (org-mode)
             (kindle-highlights-to-org--insert-as-org kindle-highlights-to-org-test-main-fixture-hash)
             (buffer-string))))
        (from-fixture-hash-deep
         (progn
           (with-temp-buffer
             (org-mode)
             (insert "**** Deep heading")
             (kindle-highlights-to-org--insert-as-org kindle-highlights-to-org-test-main-fixture-hash)
             (buffer-string)))))

  (it "should construct the intended org tree based on the fixture"
    (expect from-process-file :to-equal from-fixture-hash ))

  (it "should construct the tree relative to the root note"
    (expect
     from-fixture-hash-deep
     :to-equal
     "**** Deep heading
**** The First Book (Author First)
***** This is the first note.
- Your Highlight on Location 3776-3778 | Added on Thursday, August 16, 2018 10:47:12 PM
***** This is the third note.
- Your Highlight on page 43 | Location 558-559 | Added on Friday, August 17, 2018 3:48:30 AM
***** This is the fourth note with
a line break.
- Your Highlight on page 43 | Location 558-559 | Added on Friday, August 19, 2018 3:48:30 AM
**** The Sec (o) nd Book (Author Second)
***** This is the second note.
- Your Highlight on page 43 | Location 558-559 | Added on Friday, August 17, 2018 3:48:30 AM")))

(provide 'kindle-highlights-to-org-test)

;; Local Variables:
;; coding: utf-8
;; flycheck-disabled-checkers: 'emacs-lisp-elsa
;; End:

;;; kindle-highlights-to-org-test.el ends here
