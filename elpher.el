;;; elpher.el --- Full-featured gopher client.

;; Copyright (C) 2019 Tim Vaughan

;; Author: Tim Vaughan <tgvaughan@gmail.com>
;; Created: 11 April 2019
;; Version: 1.1.0
;; Keywords: comm gopher
;; Homepage: https://github.com/tgvaughan/elpher
;; Package-Requires: ((emacs "25"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Elpher aims to provide a full-featured gopher client for GNU Emacs.
;; It supports:

;; - intuitive keyboard and mouse-driven browsing,
;; - caching of visited sites (both content and cursor position),
;; - pleasant and configurable colouring of Gopher directories,
;; - direct visualisation of image files,
;; - (m)enu key support, similar to Emacs' info browser,
;; - clickable web and gopher links in plain text.

;; Visited pages are stored as a hierarchy rather than a linear history,
;; meaning that navigation between these pages is quick and easy.

;; To launch Elpher, simply use 'M-x elpher'.  This will open a start
;; page containing information on key bindings and suggested starting
;; points for your gopher exploration.

;; Faces, caching options and start page can be configured via
;; the Elpher customization group in Applications.

;;; Code:

(provide 'elpher)
(require 'seq)
(require 'pp)

;;; Global constants
;;

(defconst elpher-version "1.1.0"
  "Current version of elpher.")

(defconst elpher-margin-width 6
  "Width of left-hand margin used when rendering indicies.")

(defconst elpher-start-index
  (mapconcat
   'identity
   (list "i\tfake\tfake\t1"
         "i     --------------------------------------------\tfake\tfake\t1"
         "i                Elpher Gopher Client             \tfake\tfake\t1"
         (format "i                   version %s\tfake\tfake\t1" elpher-version)
         "i     --------------------------------------------\tfake\tfake\t1"
         "i\tfake\tfake\t1"
         "iUsage:\tfake\tfake\t1"
         "i\tfake\tfake\t1"
         "i - tab/shift-tab: next/prev directory entry on current page\tfake\tfake\t1"
         "i - RET/mouse-1: open directory entry under cursor\tfake\tfake\t1"
         "i - m: select a directory entry by name (autocompletes)\tfake\tfake\t1"
         "i - u: return to parent directory entry\tfake\tfake\t1"
         "i - O: visit the root directory of the current server\tfake\tfake\t1"
         "i - g: go to a particular page\tfake\tfake\t1"
         "i - r: redraw current page (using cached contents if available)\tfake\tfake\t1"
         "i - R: reload current page (regenerates cache)\tfake\tfake\t1"
         "i - d: download directory entry under cursor\tfake\tfake\t1"
         "i - w: display the raw server response for the current page\tfake\tfake\t1"
         "i\tfake\tfake\t1"
         "iPlaces to start exploring Gopherspace:\tfake\tfake\t1"
         "i\tfake\tfake\t1"
         "1Floodgap Systems Gopher Server\t\tgopher.floodgap.com\t70"
         "i\tfake\tfake\t1"
         "iAlternatively, select the following item and enter some\tfake\tfake\t1"
         "isearch terms:\tfake\tfake\t1"
         "i\tfake\tfake\t1"
         "7Veronica-2 Gopher Search Engine\t/v2/vs\tgopher.floodgap.com\t70"
         ".\r\n")
   "\r\n")
  "Source for elpher start page.")

(defconst elpher-type-map
  '((?0 elpher-get-text-node "T" elpher-text)
    (?1 elpher-get-index-node "/" elpher-index)
    (?g elpher-get-image-node "im" elpher-image)
    (?p elpher-get-image-node "im" elpher-image)
    (?I elpher-get-image-node "im" elpher-image)
    (?4 elpher-get-node-download "B" elpher-binary)
    (?5 elpher-get-node-download "B" elpher-binary)
    (?7 elpher-get-search-node "?" elpher-search)
    (?8 elpher-get-telnet-node "?" elpher-telnet)
    (?9 elpher-get-node-download "B" elpher-binary)
    (?h elpher-get-url-node "W" elpher-url))
  "Association list from types to getters, margin codes and index faces.")


;;; Customization group
;;

(defgroup elpher nil
  "A gopher client."
  :group 'applications)

;; Face customizations

(defface elpher-index
  '((t :inherit org-drawer))
  "Face used for directory type directory records.")

(defface elpher-text
  '((t :inherit org-tag))
  "Face used for text type directory records.")

(defface elpher-info
  '((t :inherit org-default))
  "Face used for info type directory records.")

(defface elpher-image
  '((t :inherit org-level-4))
  "Face used for image type directory records.")

(defface elpher-search
  '((t :inherit org-level-5))
  "Face used for search type directory records.")

(defface elpher-url
  '((t :inherit org-level-6))
  "Face used for url type directory records.")

(defface elpher-telnet
  '((t :inherit org-level-6))
  "Face used for telnet type directory records.")

(defface elpher-binary
  '((t :inherit org-level-7))
  "Face used for binary type directory records.")

(defface elpher-unknown
  '((t :inherit org-warning))
  "Face used for directory records with unknown/unsupported types.")

(defface elpher-margin-key
  '((t :inherit org-tag))
  "Face used for directory margin key.")

(defface elpher-margin-brackets
  '((t :inherit org-special-keyword))
  "Face used for brackets around directory margin key.")

;; Other customizations

(defcustom elpher-open-urls-with-eww nil
  "If non-nil, open URL selectors using eww.
Otherwise, use the system browser via the BROWSE-URL function."
  :type '(boolean))

(defcustom elpher-buttonify-urls-in-directories nil
  "If non-nil, turns URLs matched in directories into clickable buttons."
  :type '(boolean))

(defcustom elpher-cache-images nil
  "If non-nil, cache images in memory in the same way as other content."
  :type '(boolean))

(defcustom elpher-start-address nil
  "If nil, the default start directory is shown when Elpher is started.
Otherwise, a list containing the selector, host and port of a directory to
use as the start page."
  :type '(list string string integer))


;;; Model
;;

;; Address

(defun elpher-make-address (selector host port)
  "Create an address of a gopher object with SELECTOR, HOST and PORT."
  (list selector host port))

(defun elpher-address-selector (address)
  "Retrieve selector from ADDRESS."
  (car address))

(defun elpher-address-host (address)
  "Retrieve host from ADDRESS."
  (cadr address))

(defun elpher-address-port (address)
  "Retrieve port from ADDRESS."
  (caddr address))

;; Node

(defun elpher-make-node (parent address getter &optional content pos)
  "Create a node in the gopher page hierarchy.

PARENT specifies the parent of the node, ADDRESS specifies the address of
the gopher page, GETTER provides the getter function used to obtain this
page.

The optional arguments CONTENT and POS can be used to fill the cached
content and cursor position fields of the node."
  (list parent address getter content pos))

(defun elpher-node-parent (node)
  "Retrieve the parent node of NODE."
  (elt node 0))

(defun elpher-node-address (node)
  "Retrieve the address of NODE."
  (elt node 1))

(defun elpher-node-getter (node)
  "Retrieve the preferred getter function of NODE."
  (elt node 2))

(defun elpher-node-content (node)
  "Retrieve the cached content of NODE, or nil if none exists."
  (elt node 3))

(defun elpher-node-pos (node)
  "Retrieve the cached cursor position for NODE, or nil if none exists."
  (elt node 4))

(defun elpher-set-node-content (node content)
  "Set the content cache of NODE to CONTENT."
  (setcar (nthcdr 3 node) content))

(defun elpher-set-node-pos (node pos)
  "Set the cursor position cache of NODE to POS."
  (setcar (nthcdr 4 node) pos))

;; Node graph traversal

(defvar elpher-current-node nil)

(defun elpher-visit-node (node &optional getter)
  "Visit NODE using its own getter or GETTER, if non-nil."
  (elpher-save-pos)
  (elpher-process-cleanup)
  (setq elpher-current-node node)
  (if getter
      (funcall getter)
    (funcall (elpher-node-getter node))))

(defun elpher-visit-parent-node ()
  "Visit the parent of the current node."
  (let ((parent-node (elpher-node-parent elpher-current-node)))
    (when parent-node
      (elpher-visit-node parent-node))))
      
(defun elpher-reload-current-node ()
  "Reload the current node, discarding any existing cached content."
  (elpher-set-node-content elpher-current-node nil)
  (elpher-visit-node elpher-current-node))

(defun elpher-save-pos ()
  "Save the current position of point to the current node."
  (when elpher-current-node
    (elpher-set-node-pos elpher-current-node (point))))

(defun elpher-restore-pos ()
  "Restore the position of point to that cached in the current node."
  (let ((pos (elpher-node-pos elpher-current-node)))
    (if pos
        (goto-char pos)
      (goto-char (point-min)))))


;;; Buffer preparation
;;

(defmacro elpher-with-clean-buffer (&rest args)
  "Evaluate ARGS with a clean *elpher* buffer as current."
  (list 'with-current-buffer "*elpher*"
        '(elpher-mode)
        (append (list 'let '((inhibit-read-only t))
                      '(erase-buffer))
                args)))


;;; Index rendering
;;

(defun elpher-insert-index (string)
  "Insert the index corresponding to STRING into the current buffer."
  ;; Should be able to split directly on CRLF, but some non-conformant
  ;; LF-only servers sadly exist, hence the following.
  (let* ((str-no-period (replace-regexp-in-string "\r\n\.\r\n$" "\r\n" string))
         (str-no-cr (replace-regexp-in-string "\r" "" str-no-period)))
    (dolist (line (split-string str-no-cr "\n"))
      (unless (= (length line) 0)
        (elpher-insert-index-record line)))))

(defun elpher-insert-margin (&optional type-name)
  "Insert index margin, optionally containing the TYPE-NAME, into the current buffer."
  (if type-name
      (progn
        (insert (format (concat "%" (number-to-string (- elpher-margin-width 1)) "s")
                        (concat
                         (propertize "[" 'face 'elpher-margin-brackets)
                         (propertize type-name 'face 'elpher-margin-key)
                         (propertize "]" 'face 'elpher-margin-brackets))))
        (insert " "))
    (insert (make-string elpher-margin-width ?\s))))

(defun elpher-node-button-help (node)
  "Return a string containing the help text for a button corresponding to NODE."
  (let ((address (elpher-node-address node)))
    (if (eq (elpher-node-getter node) #'elpher-get-url-node)
        (let ((url (cadr (split-string (elpher-address-selector address) "URL:"))))
          (format "mouse-1, RET: open url '%s'" url))
      (format "mouse-1, RET: open '%s' on %s port %s"
              (elpher-address-selector address)
              (elpher-address-host address)
              (elpher-address-port address)))))

(defun elpher-insert-index-record (line)
  "Insert the index record corresponding to LINE into the current buffer."
  (let* ((type (elt line 0))
         (fields (split-string (substring line 1) "\t"))
         (display-string (elt fields 0))
         (selector (elt fields 1))
         (host (elt fields 2))
         (port (elt fields 3)))
    (elpher-insert-index-record-helper type display-string selector host port)))

(defun elpher-insert-index-record-helper (type display-string selector host port)
  "Helper function to insert an index record into the current buffer.
The contents of the record are dictated by TYPE, DISPLAY-STRING, SELECTOR, HOST
and PORT.

This function is essentially the second half of `elpher-insert-index-record',
but broken out so that it can be used by other functions to construct indices
on the fly."
  (let ((address (elpher-make-address selector host port))
        (type-map-entry (alist-get type elpher-type-map)))
    (if type-map-entry
        (let* ((getter (car type-map-entry))
               (margin-code (cadr type-map-entry))
               (face (caddr type-map-entry))
               (node (elpher-make-node elpher-current-node address getter)))
          (elpher-insert-margin margin-code)
          (insert-text-button display-string
                              'face face
                              'elpher-node node
                              'elpher-node-type type
                              'action #'elpher-click-link
                              'follow-link t
                              'help-echo (elpher-node-button-help node)))
      (pcase type
        (?i (elpher-insert-margin) ;; Information
            (insert (propertize
                     (if elpher-buttonify-urls-in-directories
                         (elpher-buttonify-urls display-string)
                       display-string)
                     'face 'elpher-info)))
        (tp (elpher-insert-margin (concat (char-to-string tp) "?"))
            (insert (propertize display-string
                                'face 'elpher-unknown-face)))))
    (insert "\n")))

(defun elpher-click-link (button)
  "Function called when the gopher link BUTTON is activated (via mouse or keypress)."
  (let ((node (button-get button 'elpher-node)))
    (elpher-visit-node node)))


;;; Selector retrieval (all kinds)
;;

(defun elpher-process-cleanup ()
  "Immediately shut down any extant elpher process."
  (let ((p (get-process "elpher-process")))
    (if p (delete-process p))))

(defvar elpher-selector-string)

(defun elpher-get-selector (address after)
  "Retrieve selector specified by ADDRESS, then execute AFTER.
The result is stored as a string in the variable ‘elpher-selector-string’."
  (setq elpher-selector-string "")
  (make-network-process
   :name "elpher-process"
   :host (elpher-address-host address)
   :service (elpher-address-port address)
   :filter (lambda (proc string)
             (setq elpher-selector-string (concat elpher-selector-string string)))
   :sentinel after)
  (process-send-string "elpher-process"
                       (concat (elpher-address-selector address) "\n")))

;; Index retrieval

(defun elpher-get-index-node ()
  "Getter which retrieves the current node contents as an index."
  (let ((content (elpher-node-content elpher-current-node))
        (address (elpher-node-address elpher-current-node)))
    (if content
        (progn
          (elpher-with-clean-buffer
           (insert content)
           (elpher-restore-pos)))
      (if address
          (progn
            (elpher-with-clean-buffer
             (insert "LOADING DIRECTORY..."))
            (elpher-get-selector address
                                  (lambda (proc event)
                                    (unless (string-prefix-p "deleted" event)
                                      (elpher-with-clean-buffer
                                       (elpher-insert-index elpher-selector-string)
                                       (elpher-restore-pos)
                                       (elpher-set-node-content elpher-current-node
                                                                (buffer-string)))))))
        (progn
          (elpher-with-clean-buffer
           (elpher-insert-index elpher-start-index)
           (elpher-restore-pos)
           (elpher-set-node-content elpher-current-node
                                    (buffer-string))))))))

;; Text retrieval

(defconst elpher-url-regex
  "\\([a-zA-Z]+\\)://\\([a-zA-Z0-9.\-]+\\)\\(?3::[0-9]+\\)?\\(?4:/[^ \r\n\t(),]*\\)?"
  "Regexp used to locate and buttinofy URLs in text files loaded by elpher.")

(defun elpher-buttonify-urls (string)
  "Turn substrings which look like urls in STRING into clickable buttons."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward elpher-url-regex nil t)
      (let ((url (match-string 0))
            (protocol (downcase (match-string 1))))
        (let ((node
               (if (string= protocol "gopher")
                   (let* ((host (match-string 2))
                          (port (if (> (length (match-string 3))  1)
                                    (string-to-number (substring (match-string 3) 1))
                                  70))
                          (type-and-selector (match-string 4))
                          (type (if (> (length type-and-selector) 1)
                                    (elt type-and-selector 1)
                                  ?1))
                          (selector (if (> (length type-and-selector) 1)
                                        (substring type-and-selector 2)
                                      ""))
                          (address (elpher-make-address selector host port))
                          (getter (car (alist-get type elpher-type-map))))
                     (elpher-make-node elpher-current-node address getter))
                 (let* ((host (match-string 2))
                        (port (if (> (length (match-string 3)) 1)
                                  (string-to-number (substring (match-string 3) 1))
                                70))
                        (selector (concat "URL:" url))
                        (address (elpher-make-address selector host port))
                        (getter (car (alist-get ?h elpher-type-map))))
                   (elpher-make-node elpher-current-node address getter)))))
          (make-text-button (match-beginning 0)
                            (match-end 0)
                            'elpher-node  node
                            'action #'elpher-click-link
                            'follow-link t
                            'help-echo (elpher-node-button-help node)))))
    (buffer-string)))

(defun elpher-process-text (string)
  "Remove CRs and trailing period from the gopher text document STRING."
  (let* ((chopped-str (replace-regexp-in-string "\r\n\.\r\n$" "\r\n" string))
         (cleaned-str (replace-regexp-in-string "\r" "" chopped-str)))
    (elpher-buttonify-urls cleaned-str)))

(defun elpher-get-text-node ()
  "Getter which retrieves the current node contents as a text document."
  (let ((content (elpher-node-content elpher-current-node))
        (address (elpher-node-address elpher-current-node)))
    (if content
        (progn
          (elpher-with-clean-buffer
           (insert content)
           (elpher-restore-pos)))
      (progn
        (elpher-with-clean-buffer
         (insert "LOADING TEXT..."))
        (elpher-get-selector address
                              (lambda (proc event)
                                (unless (string-prefix-p "deleted" event)
                                  (elpher-with-clean-buffer
                                   (insert (elpher-process-text elpher-selector-string))
                                   (elpher-restore-pos)
                                   (elpher-set-node-content elpher-current-node
                                                            (buffer-string))))))))))

;; Image retrieval

(defun elpher-get-image-node ()
  "Getter which retrieves the current node contents as an image to view."
  (let ((content (elpher-node-content elpher-current-node))
        (address (elpher-node-address elpher-current-node)))
    (if content
        (progn
          (elpher-with-clean-buffer
           (insert-image content)
           (elpher-restore-pos)))
      (if (display-images-p)
          (progn
            (elpher-with-clean-buffer
             (insert "LOADING IMAGE..."))
            (elpher-get-selector address
                                 (lambda (proc event)
                                   (unless (string-prefix-p "deleted" event)
                                     (let ((image (create-image
                                                   (encode-coding-string elpher-selector-string
                                                                         'no-conversion)
                                                   nil t)))
                                       (elpher-with-clean-buffer
                                        (insert-image image)
                                        (elpher-restore-pos))
                                       (if elpher-cache-images
                                           (elpher-set-node-content elpher-current-node
                                                                    image)))))))
        (elpher-get-node-download)))))

;; Search retrieval

(defun elpher-get-search-node ()
  "Getter which submits a search query to the address of the current node."
  (let ((content (elpher-node-content elpher-current-node))
        (address (elpher-node-address elpher-current-node))
        (aborted t))
    (if content
        (progn
          (elpher-with-clean-buffer
           (insert content)
           (elpher-restore-pos))
          (message "Displaying cached search results.  Reload to perform a new search."))
      (unwind-protect
          (let* ((query-string (read-string "Query: "))
                 (query-selector (concat (elpher-address-selector address) "\t" query-string))
                 (search-address (elpher-make-address query-selector
                                                       (elpher-address-host address)
                                                       (elpher-address-port address))))
            (setq aborted nil)
            (elpher-with-clean-buffer
             (insert "LOADING RESULTS..."))
            (elpher-get-selector search-address
                                  (lambda (proc event)
                                    (unless (string-prefix-p "deleted" event)
                                      (elpher-with-clean-buffer
                                       (elpher-insert-index elpher-selector-string))
                                      (goto-char (point-min))
                                      (elpher-set-node-content elpher-current-node
                                                                (buffer-string))))))
        (if aborted
            (elpher-visit-parent-node))))))

;; Raw server response retrieval

(defun elpher-get-node-raw ()
  "Getter which retrieves the raw server response for the current node."
  (let* ((content (elpher-node-content elpher-current-node))
         (address (elpher-node-address elpher-current-node)))
    (elpher-with-clean-buffer
     (insert "LOADING RAW SERVER RESPONSE..."))
    (if address
        (elpher-get-selector address
                              (lambda (proc event)
                                (unless (string-prefix-p "deleted" event)
                                  (elpher-with-clean-buffer
                                   (insert elpher-selector-string)
                                   (goto-char (point-min))))))
      (progn
        (elpher-with-clean-buffer
         (insert elpher-start-index))
        (goto-char (point-min)))))
  (message "Displaying raw server response.  Reload or redraw to return to standard view."))
 
;; File export retrieval

(defvar elpher-download-filename)

(defun elpher-get-node-download ()
  "Getter which retrieves the current node and writes the result to a file."
  (let* ((address (elpher-node-address elpher-current-node))
         (selector (elpher-address-selector address)))
    (elpher-visit-parent-node) ; Do first in case of non-local exits.
    (let* ((filename-proposal (file-name-nondirectory selector))
           (filename (read-file-name "Save file as: "
                                     nil nil nil
                                     (if (> (length filename-proposal) 0)
                                         filename-proposal
                                       "gopher.file"))))
      (message "Downloading...")
      (setq elpher-download-filename filename)
      (elpher-get-selector address
                            (lambda (proc event)
                              (let ((coding-system-for-write 'binary))
                                (with-temp-file elpher-download-filename
                                  (insert elpher-selector-string)
                                  (message (format "Download complate, saved to file %s."
                                                   elpher-download-filename)))))))))

;; URL retrieval

(defun elpher-get-url-node ()
  "Getter which attempts to open the URL specified by the current node."
  (let* ((address (elpher-node-address elpher-current-node))
         (selector (elpher-address-selector address)))
    (elpher-visit-parent-node) ; Do first in case of non-local exits.
    (let ((url (elt (split-string selector "URL:") 1)))
      (if elpher-open-urls-with-eww
          (browse-web url)
        (browse-url url)))))

;; Telnet node connection

(defun elpher-get-telnet-node ()
  "Getter which opens a telnet connection to the server specified by the current node."
  (let* ((address (elpher-node-address elpher-current-node))
         (host (elpher-address-host address))
         (port (elpher-address-port address)))
    (elpher-visit-parent-node)
    (telnet host port)))


;;; Bookmarks
;;

(defun elpher-make-bookmark (type display-string address)
  (list type display-string address))
  
(defun elpher-bookmark-type (bookmark)
  (elt bookmark 0))

(defun elpher-bookmark-display-string (bookmark)
  (elt bookmark 1))

(defun elpher-bookmark-address (bookmark)
  (elt bookmark 2))

(defun elpher-save-bookmarks (bookmarks)
  (with-temp-file (locate-user-emacs-file "elpher-bookmarks")
    (erase-buffer)
    (pp bookmarks (current-buffer))))

(defun elpher-load-bookmarks ()
  (with-temp-buffer 
    (ignore-errors
      (insert-file-contents (locate-user-emacs-file "elpher-bookmarks"))
      (goto-char (point-min))
      (read (current-buffer)))))

(defun elpher-add-bookmark (bookmark)
  (let ((bookmarks (elpher-load-bookmarks)))
    (add-to-list 'bookmarks bookmark)
    (elpher-save-bookmarks bookmarks)))

(defun elpher-remove-bookmark (bookmark)
  (elpher-save-bookmarks
   (seq-filter (lambda (this-bookmark)
                 (not (equal bookmark this-bookmark)))
               (elpher-load-bookmarks))))
     
(defun elpher-display-bookmarks ()
  (interactive)
  (elpher-with-clean-buffer
   (insert
    "Use 'u' to return to the previous page.\n\n"
    "---- Bookmark list ----\n\n")
   (let ((bookmarks (elpher-load-bookmarks)))
     (if bookmarks
         (dolist (bookmark (elpher-load-bookmarks))
           (let ((type (elpher-bookmark-type bookmark))
                 (display-string (elpher-bookmark-display-string bookmark))
                 (address (elpher-bookmark-address bookmark)))
             (elpher-insert-index-record-helper type display-string
                                                (elpher-address-selector address)
                                                (elpher-address-host address)
                                                (elpher-address-port address))))
       (insert "No bookmarks found.\n")))
   (insert "\n-----------------------")
   (goto-char (point-min))
   (elpher-next-link)))

(defun elpher-bookmark-link ()
  "Bookmark the link at point."
  (interactive)
  (let ((button (button-at (point))))
    (if button
        (let ((node (button-get button 'elpher-node))
              (type (button-get button 'elpher-node-type))
              (label (button-label button)))
          (if node
              (progn
                (elpher-add-bookmark
                 (elpher-make-bookmark type
                                       label
                                       (elpher-node-address node)))
                (message "Bookmarked \"%s\"" label))
            (error "Can only bookmark gopher links, not general URLs.")))
      (error "No link selected."))))

(defun elpher-unbookmark-link ()
  "Remove bookmark for the link at point."
  (interactive)
  (let ((button (button-at (point))))
    (if button
        (let ((node (button-get button 'elpher-node))
              (type (button-get button 'elpher-node-type)))
          (if node
              (elpher-remove-bookmark 
               (elpher-make-bookmark type
                                     (button-label button)
                                     (elpher-node-address node)))
            (error "Can only bookmark gopher links, not general URLs.")))
      (error "No link selected."))))

;;; Interactive navigation procedures
;;

(defun elpher-next-link ()
  "Move point to the next link on the current page."
  (interactive)
  (forward-button 1))

(defun elpher-prev-link ()
  "Move point to the previous link on the current page."
  (interactive)
  (backward-button 1))

(defun elpher-follow-current-link ()
  "Open the link or url at point."
  (interactive)
  (push-button))

(defun elpher-go ()
  "Go to a particular gopher site."
  (interactive)
  (switch-to-buffer "*elpher*")
  (let* (
         (hostname (read-string "Gopher host: "))
         (selector (read-string "Selector (default none): " nil nil ""))
         (port (read-string "Port (default 70): " nil nil 70))
         (address (list selector hostname port)))
    (elpher-visit-node
     (elpher-make-node elpher-current-node
                        address
                        #'elpher-get-index-node))))

(defun  elpher-redraw ()
  "Redraw current page."
  (interactive)
  (if elpher-current-node
      (elpher-visit-node elpher-current-node)
    (message "No current site.")))

(defun  elpher-reload ()
  "Reload current page."
  (interactive)
  (if elpher-current-node
      (elpher-reload-current-node)
    (message "No current site.")))

(defun elpher-view-raw ()
  "View current page as plain text."
  (interactive)
  (if elpher-current-node
      (elpher-visit-node elpher-current-node
                         #'elpher-get-node-raw)
    (message "No current site.")))

(defun elpher-back ()
  "Go to previous site."
  (interactive)
  (if (elpher-node-parent elpher-current-node)
      (elpher-visit-parent-node)
    (error "No previous site.")))

(defun elpher-download ()
  "Download the link at point."
  (interactive)
  (let ((button (button-at (point))))
    (if button
        (let ((node (button-get button 'elpher-node)))
          (if node
              (elpher-visit-node (button-get button 'elpher-node)
                                 #'elpher-get-node-download)
            (error "Can only download gopher links, not general URLs.")))
      (error "No link selected."))))

(defun elpher-build-link-map ()
  "Build alist mapping link names to destination nodes in current buffer."
  (let ((link-map nil)
        (b (next-button (point-min) t)))
    (while b
      (add-to-list 'link-map (cons (button-label b) b))
      (setq b (next-button (button-start b))))
    link-map))

(defun elpher-menu ()
  "Select a directory entry by name.  Similar to the info browser (m)enu command."
  (interactive)
  (let* ((link-map (elpher-build-link-map)))
    (if link-map
        (let ((key (let ((completion-ignore-case t))
                     (completing-read "Directory entry/link (tab to autocomplete): "
                                      link-map nil t))))
          (if (and key (> (length key) 0))
              (let ((b (cdr (assoc key link-map))))
                (goto-char (button-start b))
                (button-activate b)))))))

(defun elpher-root-dir ()
  "Visit root of current server."
  (interactive)
  (let ((address (elpher-node-address elpher-current-node)))
    (if address
        (let ((host (elpher-address-host address))
              (selector (elpher-address-selector address))
              (port (elpher-address-port address)))
          (if (> (length selector) 0)
              (let ((root-address (elpher-make-address "" host port)))
                (elpher-visit-node (elpher-make-node elpher-current-node
                                                     root-address
                                                     #'elpher-get-index-node)))
            (error "Already at root directory of current server.")))
      (error "Command invalid for Elpher start page."))))


;;; Mode and keymap
;;

(defvar elpher-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'elpher-next-link)
    (define-key map (kbd "<backtab>") 'elpher-prev-link)
    (define-key map (kbd "u") 'elpher-back)
    (define-key map (kbd "O") 'elpher-root-dir)
    (define-key map (kbd "g") 'elpher-go)
    (define-key map (kbd "r") 'elpher-redraw)
    (define-key map (kbd "R") 'elpher-reload)
    (define-key map (kbd "w") 'elpher-view-raw)
    (define-key map (kbd "d") 'elpher-download)
    (define-key map (kbd "m") 'elpher-menu)
    (when (fboundp 'evil-define-key)
      (add-to-list 'evil-motion-state-modes 'elpher-mode)
      (evil-define-key 'motion map
        (kbd "TAB") 'elpher-next-link
        (kbd "C-]") 'elpher-follow-current-link
        (kbd "C-t") 'elpher-back
        (kbd "u") 'elpher-back
        (kbd "O") 'elpher-root-dir
        (kbd "g") 'elpher-go
        (kbd "r") 'elpher-redraw
        (kbd "R") 'elpher-reload
        (kbd "w") 'elpher-view-raw
        (kbd "d") 'elpher-download
        (kbd "m") 'elpher-menu
        (kbd "a") 'elpher-bookmark-link
        (kbd "x") 'elpher-unbookmark-link
        (kbd "B") 'elpher-display-bookmarks))
    map)
  "Keymap for gopher client.")

(define-derived-mode elpher-mode special-mode "elpher"
  "Major mode for elpher, an elisp gopher client.")


;;; Main start procedure
;;

;;;###autoload
(defun elpher ()
  "Start elpher with default landing page."
  (interactive)
  (if (get-buffer "*elpher*")
      (switch-to-buffer "*elpher*")
    (switch-to-buffer "*elpher*")
    (setq elpher-current-node nil)
    (let ((start-node (elpher-make-node nil
                                        elpher-start-address
                                        #'elpher-get-index-node)))
      (elpher-visit-node start-node)))
  "Started Elpher.") ; Otherwise (elpher) evaluates to start page string.

;;; elpher.el ends here
