;;; elpher.el --- Full-featured gopher client.

;; Copyright (C) 2019 Tim Vaughan

;; Author: Tim Vaughan <tgvaughan@gmail.com>
;; Created: 11 April 2019
;; Version: 1.0.0
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

;; Elpher is a tool for exploring "gopherspace" using GNU Emacs.

;;; Code:

(provide 'elpher)

;;; Global constants
;;

(defconst elpher-version "1.0.0"
  "Current version of elpher.")

(defconst elpher-margin-width 6
  "Width of left-hand margin used when rendering indicies.")

(defconst elpher-start-index
  (mapconcat
   'identity
   (list "i\tfake\tfake\t1"
         "i--------------------------------------------\tfake\tfake\t1"
         "i           Elpher Gopher Client             \tfake\tfake\t1"
         (format "i              version %s\tfake\tfake\t1" elpher-version)
         "i--------------------------------------------\tfake\tfake\t1"
         "i\tfake\tfake\t1"
         "iBasic usage:\tfake\tfake\t1"
         "i\tfake\tfake\t1"
         "i - tab/shift-tab: next/prev directory entry on current page\tfake\tfake\t1"
         "i - RET/mouse-1: open directory entry under cursor\tfake\tfake\t1"
         "i - u: return to parent directory entry\tfake\tfake\t1"
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
         ".")
   "\r\n")
  "Source for elpher start page.")


;;; Customization group
;;

(defgroup elpher nil
  "A gopher client."
  :group 'applications)

(defface elpher-index
  '((((background dark)) :foreground "deep sky blue")
    (((background light)) :foreground "blue"))
  "Face used for index records.")

(defface elpher-text
  '((((background dark)) :foreground "white")
    (((background light)) :weight bold))
  "Face used for text records.")

(defface elpher-info '()
  "Face used for info records.")

(defface elpher-image
  '((((background dark)) :foreground "green")
    (t :foreground "dark green"))
  "Face used for image records.")

(defface elpher-search
  '((((background light)) :foreground "orange")
    (((background dark)) :foreground "dark orange"))
  "Face used for search records.")

(defface elpher-url
  '((((background dark)) :foreground "yellow")
    (((background light)) :foreground "dark red"))
  "Face used for url records.")

(defface elpher-binary
  '((t :foreground "magenta"))
  "Face used for binary records.")

(defface elpher-unknown
  '((t :foreground "red"))
  "Face used for unknown record types.")

(defface elpher-margin-key
  '((((background dark)) :foreground "white"))
  "Face used for margin key.")

(defface elpher-margin-brackets
  '((t :foreground "blue"))
  "Face used for brackets around margin key.")

(defcustom elpher-open-urls-with-eww nil
  "If non-nil, open URL selectors using eww.
Otherwise, use the system browser via the BROWSE-URL function."
  :type '(boolean))

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

(defvar elpher-current-node)

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
  (list 'progn
        '(switch-to-buffer "*elpher*")
        '(elpher-mode)
        (append (list 'let '((inhibit-read-only t))
                      '(erase-buffer))
                args)))

;;; Index rendering
;;

(defun elpher-insert-index (string)
  "Insert the index corresponding to STRING into the current buffer."
  (dolist (line (split-string string "\r\n"))
    (unless (= (length line) 0)
      (elpher-insert-index-record line))))

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

(defvar elpher-type-map
  '((?0 elpher-get-text-node "T" elpher-text)
    (?1 elpher-get-index-node "/" elpher-index)
    (?g elpher-get-image-node "im" elpher-image)
    (?p elpher-get-image-node "im" elpher-image)
    (?I elpher-get-image-node "im" elpher-image)
    (?4 elpher-get-node-download "B" elpher-binary)
    (?5 elpher-get-node-download "B" elpher-binary)
    (?9 elpher-get-node-download "B" elpher-binary)
    (?7 elpher-get-search-node "?" elpher-search))
  "Association list from types to getters, margin codes and index faces.")

(defun elpher-insert-index-record (line)
  "Insert the index record corresponding to LINE into the current buffer."
  (let* ((type (elt line 0))
         (fields (split-string (substring line 1) "\t"))
         (display-string (elt fields 0))
         (selector (elt fields 1))
         (host (elt fields 2))
         (port (elt fields 3))
         (address (elpher-make-address selector host port))
         (type-map-entry (alist-get type elpher-type-map)))
    (if type-map-entry
        (let ((getter (car type-map-entry))
              (margin-code (cadr type-map-entry))
              (face (caddr type-map-entry)))
          (elpher-insert-margin margin-code)
          (insert-text-button display-string
                              'face face
                              'elpher-node (elpher-make-node elpher-current-node
                                                               address
                                                               getter)
                              'action #'elpher-click-link
                              'follow-link t
                              'help-echo (format "mouse-1, RET: open %s on %s port %s"
                                                 selector host port)))
      (pcase type
        (?i (elpher-insert-margin) ; Information
            (insert (propertize display-string
                                'face 'elpher-info)))
        (?h (elpher-insert-margin "W") ; Web link
            (let ((url (elt (split-string selector "URL:") 1)))
              (insert-text-button display-string
                                  'face 'elpher-url
                                  'elpher-url url
                                  'action #'elpher-click-url
                                  'follow-link t
                                  'help-echo (format "mouse-1, RET: open url %s" url))))
        (?.) ; Occurs at end of index, can safely ignore.
        (tp (elpher-insert-margin (concat (char-to-string tp) "?"))
            (insert (propertize display-string
                                'face 'elpher-unknown-face)))))
    (insert "\n")))


;;; Selector retrieval (all kinds)
;;

(defun elpher-process-cleanup ()
  "Immediately shut down any extant elpher process."
  (let ((p (get-process "elpher-process")))
    (if p (delete-process p))))

(defvar elpher-selector-string)

(defun elpher-get-selector (address after)
  "Retrieve selector specified by ADDRESS, then execute AFTER.
The result is stored as a string in the variable elpher-selector-string."
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
           (insert content))
          (elpher-restore-pos))
      (if address
          (progn
            (elpher-with-clean-buffer
             (insert "LOADING DIRECTORY..."))
            (elpher-get-selector address
                                  (lambda (proc event)
                                    (unless (string-prefix-p "deleted" event)
                                      (elpher-with-clean-buffer
                                       (elpher-insert-index elpher-selector-string))
                                      (elpher-restore-pos)
                                      (elpher-set-node-content elpher-current-node
                                                                (buffer-string))))))
        (progn
          (elpher-with-clean-buffer
           (elpher-insert-index elpher-start-index))
          (elpher-restore-pos)
          (elpher-set-node-content elpher-current-node
                                    (buffer-string)))))))

;; Text retrieval

(defconst elpher-url-regex
  "\\(https?\\|gopher\\)://\\([a-zA-Z0-9.\-]+\\)\\(?3::[0-9]+\\)?\\(?4:/[^ \r\n\t(),]*\\)?"
  "Regexp used to locate and buttinofy URLs in text files loaded by elpher.")

(defun elpher-buttonify-urls (string)
  "Turn substrings which look like urls in STRING into clickable buttons."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward elpher-url-regex nil t)
      (let ((url (match-string 0))
            (protocol (downcase (match-string 1))))
        (if (string= protocol "gopher")
            (let* ((host (match-string 2))
                   (port 70)
                   (type-and-selector (match-string 4))
                   (type (if (> (length type-and-selector) 1)
                             (elt type-and-selector 1)
                           ?1))
                   (selector (if (> (length type-and-selector) 1)
                                 (substring type-and-selector 2)
                               ""))
                   (address (elpher-make-address selector host port))
                   (getter (car (alist-get type elpher-type-map))))
              (make-text-button (match-beginning 0)
                                (match-end 0)
                                'elpher-node (elpher-make-node elpher-current-node
                                                                 address
                                                                 getter)
                                'action #'elpher-click-link
                                'follow-link t
                                'help-echo (format "mouse-1, RET: open %s on %s port %s"
                                                   selector host port)))
          (make-text-button (match-beginning 0)
                            (match-end 0)
                            'elpher-url url
                            'action #'elpher-click-url
                            'follow-link t
                            'help-echo (format "mouse-1, RET: open url %s" url)))))
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
           (insert content))
          (elpher-restore-pos))
      (progn
        (elpher-with-clean-buffer
         (insert "LOADING TEXT..."))
        (elpher-get-selector address
                              (lambda (proc event)
                                (unless (string-prefix-p "deleted" event)
                                  (elpher-with-clean-buffer
                                   (insert (elpher-process-text elpher-selector-string)))
                                  (elpher-restore-pos)
                                  (elpher-set-node-content elpher-current-node
                                                            (buffer-string)))))))))

;; Image retrieval

(defun elpher-get-image-node ()
  "Getter which retrieves the current node contents as an image to view."
  (let ((content (elpher-node-content elpher-current-node))
        (address (elpher-node-address elpher-current-node)))
    (if content
        (progn
          (elpher-with-clean-buffer
           (insert-image content))
          (setq cursor-type nil)
          (elpher-restore-pos))
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
                                     (insert-image image))
                                    (setq cursor-type nil)
                                    (elpher-restore-pos)
                                    (elpher-set-node-content elpher-current-node
                                                              image)))))))))

;; Search retrieval

(defun elpher-get-search-node ()
  "Getter which submits a search query to the address of the current node."
  (let ((content (elpher-node-content elpher-current-node))
        (address (elpher-node-address elpher-current-node))
        (aborted t))
    (if content
        (progn
          (elpher-with-clean-buffer
           (insert content))
          (elpher-restore-pos)
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
                                   (insert elpher-selector-string))
                                  (goto-char (point-min)))))
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


;;; Navigation procedures
;;

(defun elpher-next-link ()
  "Move point to the next link on the current page."
  (interactive)
  (forward-button 1))

(defun elpher-prev-link ()
  "Move point to the previous link on the current page."
  (interactive)
  (backward-button 1))

(defun elpher-click-link (button)
  "Function called when the gopher link BUTTON is activated (via mouse or keypress)."
  (let ((node (button-get button 'elpher-node)))
    (elpher-visit-node node)))

(defun elpher-click-url (button)
  "Function called when the url link BUTTON is activated (via mouse or keypress)."
  (let ((url (button-get button 'elpher-url)))
    (if elpher-open-urls-with-eww
        (browse-web url)
      (browse-url url))))

(defun elpher-follow-current-link ()
  "Open the link or url at point."
  (interactive)
  (push-button))

(defun elpher-go ()
  "Go to a particular gopher site."
  (interactive)
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
  (elpher-visit-node elpher-current-node))

(defun  elpher-reload ()
  "Reload current page."
  (interactive)
  (elpher-reload-current-node))

(defun elpher-view-raw ()
  "View current page as plain text."
  (interactive)
  (elpher-visit-node elpher-current-node
                      #'elpher-get-node-raw))

(defun elpher-back ()
  "Go to previous site."
  (interactive)
  (if (elpher-node-parent elpher-current-node)
      (elpher-visit-parent-node)
    (message "No previous site.")))

(defun elpher-download ()
  "Download the link at point."
  (interactive)
  (let ((button (button-at (point))))
    (if button
        (let ((node (button-get button 'elpher-node)))
          (if node
              (elpher-visit-node (button-get button 'elpher-node)
                                  #'elpher-get-node-download)
            (message "Can only download gopher links, not general URLs.")))
      (message "No link selected."))))

;;; Mode and keymap
;;

(defvar elpher-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'elpher-next-link)
    (define-key map (kbd "<backtab>") 'elpher-prev-link)
    (define-key map (kbd "u") 'elpher-back)
    (define-key map (kbd "g") 'elpher-go)
    (define-key map (kbd "r") 'elpher-redraw)
    (define-key map (kbd "R") 'elpher-reload)
    (define-key map (kbd "w") 'elpher-view-raw)
    (define-key map (kbd "d") 'elpher-download)
    (when (fboundp 'evil-define-key)
      (evil-define-key 'normal map
        (kbd "TAB") 'elpher-next-link
        (kbd "C-]") 'elpher-follow-current-link
        (kbd "C-t") 'elpher-back
        (kbd "u") 'elpher-back
        (kbd "g") 'elpher-go
        (kbd "r") 'elpher-redraw
        (kbd "R") 'elpher-reload
        (kbd "w") 'elpher-view-raw
        (kbd "d") 'elpher-download))
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
  (setq elpher-current-node nil)
  (let ((start-node (elpher-make-node nil nil #'elpher-get-index-node)))
    (elpher-visit-node start-node))
  "Started Elpher.") ; Otherwise (elpher) evaluates to start page string.

;;; elpher.el ends here
