# kindle-highlights-to-org

> Convert your Kindle highlights to an org tree

## Overview

Takes a `My Clippings.txt` file from a Kindle and converts it into a relative tree with format:

``` org
* Book Title
** Note contents
- Metadata (location and time)
** Note contents
- Metadata (location and time)
```

## Usage

1. Add `kindle-highlights-to-org.el` into your load path. 
2. Position your cursor in an org file where you want the data inserted (tree will be inserted relative to current heading).
3. Call `(kindle-highlights-to-org)` and select your `My Clippings.txt` file, usually located in the `/documents` folder of your USB-connected Kindle.

Note that the original file isn't modified and the tree will be built from scratch every time so it may be worth deleting it to force a new file to be created after you've pulled the data. 

Tested on Kindle Paperwhite.

## Future improvements

- Make sure in org buffer
- Break up metadata
- Include more options for tree construction
- FIX: notes are reversed per book
- FIX: extra lines are inserted in certain org buffers (roam)

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
