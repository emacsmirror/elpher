# Elpher

Elpher aims to provide a full-featured gopher client for GNU Emacs.

It supports:
- simple keyboard and mouse-driven browsing, including out-of-the-box
  compatibility with evil-mode,
- caching of visited sites,
- clean and configurable visualization of Gopher directories,
- direct visualisation of image files,
- clickable web and gopher links in plain text.

Elpher is still under development.  Planned features include:
- (m)enu key support, similar to the Emacs info browser,
- support for multiple Elpher buffers.

Keep the non-web internet alive!

## Installation

Installation instructions will be added soon.

## Usage

Once installed, use `M-x elpher` to launch the browser.  This will
open a start page which documents the default key bindings and
provides some links to help kick start your exploration of gopherspace.

To customize the various faces Elpher uses, the start page
and a few other odds and ends, simply use `M-x customize-group`
and enter "elpher" at the group prompt.

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
              
Pressing the 'u' button (introduced on page which opens when elopher starts)
always moves to the page representing "parent" node, whatever that is.

Once a node is visited, its "contents" (i.e. whatever is retrieved
from the gopher server) are recorded with the corresponding node.  The
cursor position (point) is also stored. If the node is visited again,
the cached contents are displayed and the cursor returns to its
previous position.  This makes navigating amongst different documents
referenced from within the same directory very snappy.

## Licence

Elpher is free software and is distributed under the terms of version
3 the GNU General Public License, which can be found in the file named
COPYING.
