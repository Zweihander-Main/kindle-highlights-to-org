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

(describe "kindle-highlights-to-org--get-file-path"
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

  (it "asks the user if the supplied file doesn't match 'My Clippings.txt'"
    (kindle-highlights-to-org--get-file-path "./test/fixtures/test.txt")
    (expect 'y-or-n-p :to-have-been-called))

  (it "doesn't ask the user if the supplied file matches 'My Clippings.txt'"
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

;; Local Variables:
;; coding: utf-8
;; End:

;;; kindle-highlights-to-org-test.el ends here
