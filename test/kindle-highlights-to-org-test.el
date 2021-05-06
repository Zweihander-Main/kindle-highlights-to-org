;;; kindle-highlights-to-org-test.el --- Tests for kindle-highlights-to-org
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

;; Tests for kindle-highlights-to-org.el

;;; Code:

(require 'buttercup)
(require 'f)
(require 'kindle-highlights-to-org)

(defconst khto-test-directory
  "./test"
  "The test directory.")

(defconst khto-fixtures-directory
  (f-join khto-test-directory "./fixtures")
  "The fixtures directory.")

(defconst khto-main-fixture
  (f-join khto-fixtures-directory "./My Clippings.txt")
  "The main My Clippings.txt fixture.")

(describe "The get-file-path function"
  (before-each
    (spy-on 'read-file-name)
    (spy-on 'y-or-n-p))

  (it "prompts for a file if none is given"
    (spy-on 'read-file-name :and-return-value khto-main-fixture)
    (kindle-highlights-to-org--get-file-path)
    (expect 'read-file-name :to-have-been-called))

  (it "doesn't prompt for a file if a path is given"
    (kindle-highlights-to-org--get-file-path khto-main-fixture)
    (expect 'read-file-name :not :to-have-been-called))

  (it "asks the user if the supplied file doesn't match My Clippings.txt"
    (kindle-highlights-to-org--get-file-path "./test/fixtures/test.txt")
    (expect 'y-or-n-p :to-have-been-called))

  (it "doesn't ask the user if the supplied file matches My Clippings.txt"
    (kindle-highlights-to-org--get-file-path khto-main-fixture)
    (expect 'y-or-n-p :not :to-have-been-called))

  (it "checks file exists"
    (expect
     (kindle-highlights-to-org--get-file-path "./test/nope/My Clippings.txt")
     :to-equal "File doesn't exist."))

  (it "returns an expanded path from a relative one"
    (expect
     (kindle-highlights-to-org--get-file-path khto-main-fixture)
     :to-equal (expand-file-name khto-main-fixture))))

(describe "The process-file function return value"
  ;; Check grouping, reversing, metadata
  (it "should group based on book title"
    (expect (hash-table-keys
             (kindle-highlights-to-org--process-file
              (expand-file-name khto-main-fixture)))
            :to-have-same-items-as
            '("The First Book (Author First)"
              "The Second Book (Author Second)")))

  (it "should put the books in the original order"
    (expect (car (hash-table-keys
                  (kindle-highlights-to-org--process-file
                   (expand-file-name khto-main-fixture))))
            :to-equal "The First Book (Author First)"))

  (it "should pull all the notes"
    (expect (mapcar
             (lambda (value)
               (plist-get value :contents))
             (-flatten-n 1 (hash-table-values
                            (kindle-highlights-to-org--process-file
                             (expand-file-name khto-main-fixture)))))
            :to-have-same-items-as
            '("This is the first note."
              "This is the second note."
              "This is the third note.")))

  (it "should put the notes in the right order"
    (expect (plist-get (car (-flatten-n 1 (hash-table-values
                                  (kindle-highlights-to-org--process-file
                                   (expand-file-name khto-main-fixture)))))
                       :contents)
            :to-equal "This is the first note."))


  (it "should pull all the metadata"
    (expect (mapcar
             (lambda (value)
               (plist-get value :meta))
             (-flatten-n 1 (hash-table-values
                            (kindle-highlights-to-org--process-file
                             (expand-file-name khto-main-fixture)))))
            :to-have-same-items-as
            '("- Your Highlight on Location 3776-3778 | Added on Thursday, August 16, 2018 10:47:12 PM"
              "- Your Highlight on page 43 | Location 558-559 | Added on Friday, August 17, 2018 3:48:30 AM"
              "- Your Highlight on page 43 | Location 558-559 | Added on Friday, August 17, 2018 3:48:30 AM")))

  (it "should put the metadata in the right order"
    (expect (plist-get (car (car (hash-table-values
                                  (kindle-highlights-to-org--process-file
                                   (expand-file-name khto-main-fixture)))))
                       :meta)
            :to-equal
            "- Your Highlight on Location 3776-3778 | Added on Thursday, August 16, 2018 10:47:12 PM"))


  ;; TODO Don't break on edge cases
  ;; Empty file
  )

;; Local Variables:
;; coding: utf-8
;; flycheck-disabled-checkers: 'emacs-lisp-elsa
;; End:

;;; kindle-highlights-to-org-test.el ends here
