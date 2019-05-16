# Elpher

[![MELPA Stable](https://stable.melpa.org/packages/elpher-badge.svg)](https://stable.melpa.org/#/elpher)
[![MELPA](https://melpa.org/packages/elpher-badge.svg)](https://melpa.org/#/elpher)

Elpher aims to provide a full-featured gopher client for GNU Emacs.

It supports:
- simple keyboard and mouse-driven browsing, including out-of-the-box
  compatibility with evil-mode,
- caching of visited sites,
- clean and configurable visualization of Gopher directories,
- direct visualisation of image files,
- jumping directly to links by name (with autocompletion),
- clickable web and gopher links in plain text.

Keep the non-web internet alive!

## Installation

Elpher is available from [MELPA](https://melpa.org).  If you have
never installed packages from this repository before, you'll need
to follow the instructions at https://melpa.org/#/getting-started.

To install Elpher, enter the following:

    M-x package-install RET elpher RET

To uninstall, use `M-x package-delete RET elpher RET`.

## Usage

Once installed, use `M-x elpher` to launch the browser.  This will
open a start page which documents the default key bindings and
provides some links to help kick start your exploration of gopherspace.

To customize the various faces Elpher uses, the start page
and a few other odds and ends, use the following:

    M-x customize-group RET elpher RET

## History and Caching

This is an aspect of Elpher that perhaps requires separate explanation.

Every item you visit with Elpher is modeled as a "node" in a tree.
For instance, a gopher directory represents a single node.  When
you open such a directory, Elpher creates nodes for every entry
in that directory and makes these children of the original directory node:

               X  <- current directory node
               |
            -------
            |  |  |
            o  o  o  <- nodes representing entries in directory

If one of those entries is itself a directory and you click on it,
Elpher marks that node the current node, and extends the tree as follows:

               o  <- original directory node
               |
            -------
            |  |  |
            o  o  X  <- current directory node (marked with X)
                  |
              ---------
              | | | | |
              o o o o o  <- nodes representing entries in new directory
              
Pressing the 'u' key (introduced on page which opens when elopher starts)
always moves to the page representing "parent" node, whatever that is.

Once a node is visited, its "contents" (i.e. whatever is retrieved
from the gopher server) are recorded in memory with the corresponding
node.  The cursor position (point) is also stored. If the node is
visited again in the same session, the cached contents are displayed
and the cursor returns to its previous position.  This makes
navigating amongst different documents referenced from within the same
directory very snappy. (To conserve memory, this caching isn't by
default applied to images entries viewed in the browser.)

This hierarchy is also maintained when gopher URLs are followed from plain
text documents, and when directories are retrieved explicitly using the 'g'
key.

## Licence

Elpher is free software and is distributed under the terms of version
3 the GNU General Public License, which can be found in the file named
COPYING.
