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

Important: does not currently work with notes typed manually with line breaks in them. Fix is planned.

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

## Possible future improvements

- Multiline note support
- Breaking up metadata
- Add hash to notes to allow updating (has internationalization problems)
- Include more options for tree construction
- Sanity check on the file?

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
