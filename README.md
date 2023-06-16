# kindle-highlights-to-org

> An Emacs package to convert your Kindle highlights to an org tree

## Overview

Takes a `My Clippings.txt` file from a Kindle and converts it into a relative org tree with format:

``` org
* Book Title
** Note contents
- Metadata (location and time)
** Note contents
- Metadata (location and time)
```

Does not modify original file or update existing data. 

Notes: 
- User-inserted newlines should be preserved (these are usually rare).
- At the moment, these newlines will create broken headings with newlines in the middle of them.
- Notes are sorted by date added. 
- May or may not work with languages other than English -- bug reports appreciated

## Usage

1. Add `kindle-highlights-to-org.el` into your load path. Set the appropriate `use-package`, something like the following for DOOM and `org-roam`:
``` emacs-lisp
(use-package! kindle-highlights-to-org
  :after org-roam
  :defer t)
```
2. Position your cursor in an org buffer where you want the data inserted (tree will be inserted relative to current heading).
3. Call `(kindle-highlights-to-org)` and select your `My Clippings.txt` file, usually located in the `/documents` folder of your USB-connected Kindle.

Note that the original file isn't modified and the tree will be built from scratch every time so it may be worth deleting it to force a new file to be created after you've pulled the data. 

Tested on Kindle Paperwhite.

## Dev notes

- Uses `eldev` for `lint`, `test`, ect.

``` emacs-lisp
;;; Format of My Clippings.txt file:
;;;     - 5+ lines for each note, regardless of length
;;; 1. TITLE
;;; 2. METADATA
;;;     - one line with blocks separated by | char
;;;     - some amount of blocks, usually 2-3 but 4 might be possible
;;;     - last block should be time added
;;;     - can be broken apart but identifying blocks is difficult due to
;;;       language differences eg 'Added on' is only for English
;;; 3. BLANK LINE
;;; 4-n. NOTEDATA
;;;     - mostly on one line but user can manually add a newline char
;;; n+1. SEPERATOR
;;;     - 10 equal signs
;;;     - ==========
;;;     - is at the end of the note block, not the start (file ends with one)
```

## Possible future improvements

- Include more options for tree construction
- Newlines shouldn't destroy headings. 
- Better sanity checks on the given file
- Add hash to notes to allow updating (has internationalization problems)

## See also

- [lvzon/kindle-clippings](https://github.com/lvzon/kindle-clippings): similar script which outputs to text files, uses python
- [lxyu/kindle-clippings](https://github.com/lxyu/kindle-clippings): alternative to above
- [kindle2notion](https://github.com/paperboi/kindle2notion): export Kindle Notes into Notion, uses python
- [Reading Notes for Kindle](https://github.com/mammuth/kindle-clippings): local web app for managing Kindle notes

## Available for Hire

I'm available for freelance, contracts, and consulting both remotely and in the Hudson Valley, NY (USA) area. [Some more about me](https://www.zweisolutions.com/about.html) and [what I can do for you](https://www.zweisolutions.com/services.html).

Feel free to drop me a message at:

```
hi [a+] zweisolutions {●} com
```

## License

[AGPLv3](./LICENSE)

    kindle-highlights-to-org
    Copyright (C) 2021 Zweihänder

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published
    by the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
