;;; elpher.el --- A friendly gopher client.  -*- lexical-binding:t -*-

;; Copyright (C) 2019 Tim Vaughan

;; Author: Tim Vaughan <tgvaughan@gmail.com>
;; Created: 11 April 2019
;; Version: 2.4.4
;; Keywords: comm gopher
;; Homepage: http://thelambdalab.xyz/elpher
;; Package-Requires: ((emacs "26"))

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

;; Elpher aims to provide a practical and friendly gopher client
;; for GNU Emacs.  It supports:

;; - intuitive keyboard and mouse-driven browsing,
;; - out-of-the-box compatibility with evil-mode,
;; - clickable web and gopher links *in plain text*,
;; - caching of visited sites,
;; - pleasant and configurable colouring of Gopher directories,
;; - direct visualisation of image files,
;; - a simple bookmark management system,
;; - connections using TLS encryption,
;; - support for the fledgling Gemini protocol.

;; To launch Elpher, simply use 'M-x elpher'.  This will open a start
;; page containing information on key bindings and suggested starting
;; points for your gopher exploration.

;; Full instructions can be found in the Elpher info manual.

;; Elpher is under active development.  Any suggestions for
;; improvements are welcome, and can be made on the official
;; project page, gopher://thelambdalab.xyz/1/projects/elpher/.

;;; Code:

(provide 'elpher)

;;; Dependencies
;;

(require 'seq)
(require 'pp)
(require 'shr)
(require 'url-util)
(require 'subr-x)
(require 'dns)


;;; Global constants
;;

(defconst elpher-version "2.4.4"
  "Current version of elpher.")

(defconst elpher-margin-width 6
  "Width of left-hand margin used when rendering indicies.")

(defconst elpher-type-map
  '(((gopher ?0) elpher-get-gopher-node elpher-render-text "txt" elpher-text)
    ((gopher ?1) elpher-get-gopher-node elpher-render-index "/" elpher-index)
    ((gopher ?4) elpher-get-gopher-node elpher-render-download "bin" elpher-binary)
    ((gopher ?5) elpher-get-gopher-node elpher-render-download "bin" elpher-binary)
    ((gopher ?7) elpher-get-gopher-query-node elpher-render-index "?" elpher-search)
    ((gopher ?9) elpher-get-gopher-node elpher-render-download "bin" elpher-binary)
    ((gopher ?g) elpher-get-gopher-node elpher-render-image "img" elpher-image)
    ((gopher ?p) elpher-get-gopher-node elpher-render-image "img" elpher-image)
    ((gopher ?I) elpher-get-gopher-node elpher-render-image "img" elpher-image)
    ((gopher ?d) elpher-get-gopher-node elpher-render-download "doc" elpher-binary)
    ((gopher ?P) elpher-get-gopher-node elpher-render-download "doc" elpher-binary)
    ((gopher ?s) elpher-get-gopher-node elpher-render-download "snd" elpher-binary)
    ((gopher ?h) elpher-get-gopher-node elpher-render-html "htm" elpher-html)
    (gemini elpher-get-gemini-node elpher-render-gemini "gem" elpher-gemini)
    (telnet elpher-get-telnet-node nil "tel" elpher-telnet)
    (other-url elpher-get-other-url-node nil "url" elpher-other-url)
    ((special bookmarks) elpher-get-bookmarks-node nil "/" elpher-index)
    ((special start) elpher-get-start-node nil))
  "Association list from types to getters, renderers, margin codes and index faces.")


;;; Customization group
;;

(defgroup elpher nil
  "A gopher client."
  :group 'applications)

;; Face customizations

(defface elpher-index
  '((t :inherit font-lock-keyword-face))
  "Face used for directory type directory records.")

(defface elpher-text
  '((t :inherit bold))
  "Face used for text type directory records.")

(defface elpher-info
  '((t :inherit default))
  "Face used for info type directory records.")

(defface elpher-image
  '((t :inherit font-lock-string-face))
  "Face used for image type directory records.")

(defface elpher-search
  '((t :inherit warning))
  "Face used for search type directory records.")

(defface elpher-html
  '((t :inherit font-lock-comment-face))
  "Face used for html type directory records.")

(defface elpher-gemini
  '((t :inherit font-lock-regexp-grouping-backslash))
  "Face used for html type directory records.")

(defface elpher-other-url
  '((t :inherit font-lock-comment-face))
  "Face used for other URL type links records.")

(defface elpher-telnet
  '((t :inherit font-lock-function-name-face))
  "Face used for telnet type directory records.")

(defface elpher-binary
  '((t :inherit font-lock-doc-face))
  "Face used for binary type directory records.")

(defface elpher-unknown
  '((t :inherit error))
  "Face used for directory records with unknown/unsupported types.")

(defface elpher-margin-key
  '((t :inherit bold))
  "Face used for directory margin key.")

(defface elpher-margin-brackets
  '((t :inherit shadow))
  "Face used for brackets around directory margin key.")

;; Other customizations

(defcustom elpher-open-urls-with-eww nil
  "If non-nil, open URL selectors using eww.
Otherwise, use the system browser via the BROWSE-URL function."
  :type '(boolean))

(defcustom elpher-buttonify-urls-in-directories t
  "If non-nil, turns URLs matched in directories into clickable buttons."
  :type '(boolean))

(defcustom elpher-use-header t
  "If non-nil, display current node information in buffer header."
  :type '(boolean))

(defcustom elpher-auto-disengage-TLS nil
  "If non-nil, automatically disengage TLS following an unsuccessful connection.
While enabling this may seem convenient, it is also potentially dangerous as it
allows switching from an encrypted channel back to plain text without user input."
  :type '(boolean))

(defcustom elpher-connection-timeout 5
  "Specifies the number of seconds to wait for a network connection to time out."
  :type '(integer))

;;; Model
;;

;; Address

;; An elpher "address" object is either a url object or a symbol.
;; Symbol addresses are "special", corresponding to pages generated
;; dynamically for and by elpher.  All others represent pages which
;; rely on content retrieved over the network.

(defun elpher-address-from-url (url-string)
  "Create a ADDRESS object corresponding to the given URL-STRING."
  (let ((data (match-data))) ; Prevent parsing clobbering match data
    (unwind-protect
        (let ((url (url-generic-parse-url url-string)))
          (unless (and (not (url-fullness url)) (url-type url))
            (setf (url-fullness url) t)
            (setf (url-filename url)
                  (url-unhex-string (url-filename url)))
            (unless (url-type url)
              (setf (url-type url) "gopher"))
            (when (or (equal "gopher" (url-type url))
                      (equal "gophers" (url-type url)))
              ;; Gopher defaults
              (unless (url-host url)
                (setf (url-host url) (url-filename url))
                (setf (url-filename url) ""))
              (when (or (equal (url-filename url) "")
                        (equal (url-filename url) "/"))
                (setf (url-filename url) "/1")))
            (when (equal "gemini" (url-type url))
              ;; Gemini defaults
              (if (equal (url-filename url) "")
                  (setf (url-filename url) "/"))))
          url)
      (set-match-data data))))

(defun elpher-make-gopher-address (type selector host port &optional tls)
  "Create an ADDRESS object using gopher directory record attributes.
The basic attributes include: TYPE, SELECTOR, HOST and PORT.
If the optional attribute TLS is non-nil, the address will be marked as
requiring gopher-over-TLS."
  (cond
   ((and (equal type ?h)
         (string-prefix-p "URL:" selector))
    (elpher-address-from-url (elt (split-string selector "URL:") 1)))
   ((equal type ?8)
    (elpher-address-from-url
     (concat "telnet"
             "://" host
             ":" (number-to-string port))))
   (t
    (elpher-address-from-url
     (concat "gopher" (if tls "s" "")
             "://" host
             ":" (number-to-string port)
             "/" (string type)
             selector)))))

(defun elpher-make-special-address (type)
  "Create an ADDRESS object corresponding to the given special page symbol TYPE."
  type)

(defun elpher-address-to-url (address)
  "Get string representation of ADDRESS, or nil if ADDRESS is special."
  (if (not (elpher-address-special-p address))
      (url-encode-url (url-recreate-url address))
    nil))

(defun elpher-address-type (address)
  "Retrieve type of ADDRESS object.
This is used to determine how to retrieve and render the document the
address refers to, via the table `elpher-type-map'."
  (if (symbolp address)
      (list 'special address)
    (let ((protocol (url-type address)))
      (cond ((or (equal protocol "gopher")
                 (equal protocol "gophers"))
             (list 'gopher
                   (if (member (url-filename address) '("" "/"))
                       ?1
                     (string-to-char (substring (url-filename address) 1)))))
            ((equal protocol "gemini")
             'gemini)
            ((equal protocol "telnet")
             'telnet)
            (t 'other-url)))))

(defun elpher-address-protocol (address)
  "Retrieve the transport protocol for ADDRESS.  This is nil for special addresses."
  (if (symbolp address)
      nil
    (url-type address)))

(defun elpher-address-filename (address)
  "Retrieve the filename component of ADDRESS.
For gopher addresses this is a combination of the selector type and selector."
  (if (symbolp address)
      nil
    (url-filename address)))

(defun elpher-address-host (address)
  "Retrieve host from ADDRESS object."
  (url-host address))

(defun elpher-address-port (address)
  "Retrieve port from ADDRESS object.
If no address is defined, returns 0.  (This is for compatibility with the URL library.)"
  (if (symbolp address)
      0
    (url-port address)))

(defun elpher-address-special-p (address)
  "Return non-nil if ADDRESS object is special (e.g. start page, bookmarks page)."
  (symbolp address))

(defun elpher-address-gopher-p (address)
  "Return non-nill if ADDRESS object is a gopher address."
  (and (not (elpher-address-special-p address))
       (member (elpher-address-protocol address) '("gopher gophers"))))

(defun elpher-gopher-address-selector (address)
  "Retrieve gopher selector from ADDRESS object."
  (if (member (url-filename address) '("" "/"))
      ""
    (substring (url-filename address) 2)))

;; Node

(defun elpher-make-node (display-string address &optional parent)
  "Create a node in the page hierarchy.

DISPLAY-STRING records the display string used for the page.

ADDRESS specifies the address object of the page.

The optional PARENT specifies the parent node in the hierarchy.
This is set every time the node is visited, so while it forms
an important part of the node data there is no need to set it
initially."
  (list display-string address parent))

(defun elpher-node-display-string (node)
  "Retrieve the display string of NODE."
  (elt node 0))

(defun elpher-node-address (node)
  "Retrieve the ADDRESS object of NODE."
  (elt node 1))

(defun elpher-node-parent (node)
  "Retrieve the parent node of NODE."
  (elt node 2))

(defun elpher-set-node-parent (node parent)
  "Set the parent node of NODE to be PARENT."
  (setcar (cdr (cdr node)) parent))

;; Cache

(defvar elpher-content-cache (make-hash-table :test 'equal))
(defvar elpher-pos-cache (make-hash-table :test 'equal))

(defun elpher-get-cached-content (address)
  "Retrieve the cached content for ADDRESS, or nil if none exists."
  (gethash address elpher-content-cache))

(defun elpher-cache-content (address content)
  "Set the content cache for ADDRESS to CONTENT."
  (puthash address content elpher-content-cache))

(defun elpher-get-cached-pos (address)
  "Retrieve the cached cursor position for ADDRESS, or nil if none exists."
  (gethash address elpher-pos-cache))

(defun elpher-cache-pos (address pos)
  "Set the cursor position cache for ADDRESS to POS."
  (puthash address pos elpher-pos-cache))

;; Node graph traversal

(defvar elpher-current-node nil)

(defun elpher-visit-node (node &optional renderer preserve-parent)
  "Visit NODE using its own renderer or RENDERER, if non-nil.
Additionally, set the parent of NODE to `elpher-current-node',
unless PRESERVE-PARENT is non-nil."
  (elpher-save-pos)
  (elpher-process-cleanup)
  (unless preserve-parent
    (if (and (elpher-node-parent elpher-current-node)
             (equal (elpher-node-address elpher-current-node)
                    (elpher-node-address node)))
        (elpher-set-node-parent node (elpher-node-parent elpher-current-node))
      (elpher-set-node-parent node elpher-current-node)))
  (setq elpher-current-node node)
  (let* ((address (elpher-node-address node))
         (type (elpher-address-type address))
         (type-record (cdr (assoc type elpher-type-map))))
    (if type-record
        (funcall (car type-record)
                 (if renderer
                     renderer
                   (cadr type-record)))
      (elpher-visit-parent-node)
      (pcase type
        (`(gopher ,type-char)
         (error "Unsupported gopher selector type '%c' for '%s'"
                type-char (elpher-address-to-url address)))
        (other
         (error "Unsupported address type '%S' for '%s'"
                other (elpher-address-to-url address)))))))

(defun elpher-visit-parent-node ()
  "Visit the parent of the current node."
  (let ((parent-node (elpher-node-parent elpher-current-node)))
    (when parent-node
      (elpher-visit-node parent-node nil t))))
      
(defun elpher-reload-current-node ()
  "Reload the current node, discarding any existing cached content."
  (elpher-cache-content (elpher-node-address elpher-current-node) nil)
  (elpher-visit-node elpher-current-node))

(defun elpher-save-pos ()
  "Save the current position of point to the current node."
  (when elpher-current-node
    (elpher-cache-pos (elpher-node-address elpher-current-node) (point))))

(defun elpher-restore-pos ()
  "Restore the position of point to that cached in the current node."
  (let ((pos (elpher-get-cached-pos (elpher-node-address elpher-current-node))))
    (if pos
        (goto-char pos)
      (goto-char (point-min)))))


;;; Buffer preparation
;;

(defun elpher-update-header ()
  "If `elpher-use-header' is true, display current node info in window header."
  (if elpher-use-header
      (let* ((display-string (elpher-node-display-string elpher-current-node))
             (address (elpher-node-address elpher-current-node))
             (tls-string (if (and (not (elpher-address-special-p address))
                                  (member (elpher-address-protocol address)
                                          '("gophers" "gemini")))
                             " [TLS encryption]"
                           ""))
             (header (concat display-string
                             (propertize tls-string 'face 'bold))))
        (setq header-line-format header))))

(defmacro elpher-with-clean-buffer (&rest args)
  "Evaluate ARGS with a clean *elpher* buffer as current."
  (list 'with-current-buffer "*elpher*"
        '(elpher-mode)
        (append (list 'let '((inhibit-read-only t))
                      '(erase-buffer)
                      '(elpher-update-header))
                args)))


;;; Text Processing
;;

(defvar elpher-user-coding-system nil
  "User-specified coding system to use for decoding text responses.")

(defun elpher-decode (string)
  "Decode STRING using autodetected or user-specified coding system."
  (decode-coding-string string
                        (if elpher-user-coding-system
                            elpher-user-coding-system
                          (detect-coding-string string t))))

(defun elpher-preprocess-text-response (string)
  "Preprocess text selector response contained in STRING.
This involes decoding the character representation, and clearing
away CRs and any terminating period."
  (elpher-decode (replace-regexp-in-string "\n\.\n$" "\n"
                                           (replace-regexp-in-string "\r" "" string))))


;;; Network error reporting
;;

(defun elpher-network-error (address error)
  "Display ERROR message following unsuccessful negotiation with ADDRESS.
ERROR can be either an error object or a string."
  (elpher-with-clean-buffer
   (insert (propertize "\n---- ERROR -----\n\n" 'face 'error)
           "When attempting to retrieve " (elpher-address-to-url address) ":\n"
           (if (stringp error) error (error-message-string error)) "\n"
           (propertize "\n----------------\n\n" 'face 'error)
           "Press 'u' to return to the previous page.")))


;;; Gopher selector retrieval
;;

(defvar elpher-network-timer nil
  "Timer used for network connections.")

(defun elpher-process-cleanup ()
  "Immediately shut down any extant elpher process and timers."
  (let ((p (get-process "elpher-process")))
    (if p (delete-process p)))
  (if (timerp elpher-network-timer)
      (cancel-timer elpher-network-timer)))

(defvar elpher-use-tls nil
  "If non-nil, use TLS to communicate with gopher servers.")

(defun elpher-get-selector (address renderer &optional force-ipv4)
  "Retrieve selector specified by ADDRESS, then render it using RENDERER.
If FORCE-IPV4 is non-nil, explicitly look up and use IPv4 address corresponding
to ADDRESS."
  (when (equal (elpher-address-protocol address) "gophers")
    (if (gnutls-available-p)
        (when (not elpher-use-tls)
          (setq elpher-use-tls t)
          (message "Engaging TLS gopher mode."))
      (error "Cannot retrieve TLS gopher selector: GnuTLS not available")))
  (unless (< (elpher-address-port address) 65536)
    (error "Cannot retrieve gopher selector: port number > 65536"))
  (condition-case nil
      (let* ((kill-buffer-query-functions nil)
             (port (elpher-address-port address))
             (host (elpher-address-host address))
             (selector-string "")
             (proc (open-network-stream "elpher-process"
                                        nil
                                        (if force-ipv4 (dns-query host) host)
                                        (if (> port 0) port 70)
                                        :type (if elpher-use-tls 'tls 'plain)
                                        :nowait t))
             (timer (run-at-time elpher-connection-timeout
                                 nil
                                 (lambda ()
                                   (pcase (process-status proc)
                                     ('failed
                                      (if (and (not (equal (elpher-address-protocol address)
                                                           "gophers"))
                                               elpher-use-tls
                                               (or elpher-auto-disengage-TLS
                                                   (yes-or-no-p "Could not establish encrypted connection.  Disable TLS mode?")))
                                          (progn
                                            (message "Disabling TLS mode.")
                                            (setq elpher-use-tls nil)
                                            (elpher-get-selector address renderer))
                                        (elpher-network-error address "Could not establish encrypted connection")))
                                     ('connect
                                      (elpher-process-cleanup)
                                      (unless force-ipv4
                                        (message "Connection timed out. Retrying with IPv4 address.")
                                        (elpher-get-selector address renderer t))))))))
        (setq elpher-network-timer timer)
        (set-process-coding-system proc 'binary)
        (set-process-filter proc
                            (lambda (_proc string)
                              (cancel-timer timer)
                              (setq selector-string
                                    (concat selector-string string))))
        (set-process-sentinel proc
                              (lambda (_proc event)
                                (condition-case the-error
                                    (cond
                                     ((string-prefix-p "deleted" event))
                                     ((string-prefix-p "open" event)
                                      (let ((inhibit-eol-conversion t))
                                        (process-send-string
                                         proc
                                         (concat (elpher-gopher-address-selector address)
                                                 "\r\n"))))
                                     (t
                                      (cancel-timer timer)
                                      (funcall renderer selector-string)
                                      (elpher-restore-pos)))
                                  (error
                                   (elpher-network-error address the-error))))))
    (error
     (error "Error initiating connection to server"))))

(defun elpher-get-gopher-node (renderer)
  "Getter function for gopher nodes.
The RENDERER procedure is used to display the contents of the node
once they are retrieved from the gopher server."
  (let* ((address (elpher-node-address elpher-current-node))
         (content (elpher-get-cached-content address)))
    (if (and content (funcall renderer nil))
        (elpher-with-clean-buffer
         (insert content)
         (elpher-restore-pos))
      (elpher-with-clean-buffer
       (insert "LOADING... (use 'u' to cancel)"))
      (condition-case the-error
          (elpher-get-selector address renderer)
        (error
         (elpher-network-error address the-error))))))

;; Index rendering

(defun elpher-insert-index (string)
  "Insert the index corresponding to STRING into the current buffer."
  ;; Should be able to split directly on CRLF, but some non-conformant
  ;; LF-only servers sadly exist, hence the following.
  (let ((str-processed (elpher-preprocess-text-response string)))
    (dolist (line (split-string str-processed "\n"))
      (ignore-errors
        (unless (= (length line) 0)
          (let* ((type (elt line 0))
                 (fields (split-string (substring line 1) "\t"))
                 (display-string (elt fields 0))
                 (selector (elt fields 1))
                 (host (elt fields 2))
                 (port (if (elt fields 3)
                           (string-to-number (elt fields 3))
                         nil))
                 (address (elpher-make-gopher-address type selector host port)))
            (elpher-insert-index-record display-string address)))))))

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
    (format "mouse-1, RET: open '%s'" (if (elpher-address-special-p address)
                                          address
                                        (elpher-address-to-url address)))))

(defun elpher-insert-index-record (display-string &optional address)
  "Function to insert an index record into the current buffer.
The contents of the record are dictated by DISPLAY-STRING and ADDRESS.
If ADDRESS is not supplied or nil the record is rendered as an
'information' line."
  (let* ((type (if address (elpher-address-type address) nil))
         (type-map-entry (cdr (assoc type elpher-type-map))))
    (if type-map-entry
        (let* ((margin-code (elt type-map-entry 2))
               (face (elt type-map-entry 3))
               (node (elpher-make-node display-string address)))
          (elpher-insert-margin margin-code)
          (insert-text-button display-string
                              'face face
                              'elpher-node node
                              'action #'elpher-click-link
                              'follow-link t
                              'help-echo (elpher-node-button-help node)))
      (pcase type
        ((or '(gopher ?i) 'nil) ;; Information
         (elpher-insert-margin)
         (let ((propertized-display-string
                (propertize display-string 'face 'elpher-info)))
           (insert (if elpher-buttonify-urls-in-directories
                       (elpher-buttonify-urls propertized-display-string)
                     propertized-display-string))))
        (`(gopher ,selector-type) ;; Unknown
         (elpher-insert-margin (concat (char-to-string selector-type) "?"))
         (insert (propertize display-string
                             'face 'elpher-unknown)))))
    (insert "\n")))

(defun elpher-click-link (button)
  "Function called when the gopher link BUTTON is activated (via mouse or keypress)."
  (let ((node (button-get button 'elpher-node)))
    (elpher-visit-node node)))

(defun elpher-render-index (data &optional _mime-type-string)
  "Render DATA as an index.  MIME-TYPE-STRING is unused."
  (elpher-with-clean-buffer
   (if (not data)
       t
     (elpher-insert-index data)
     (elpher-cache-content (elpher-node-address elpher-current-node)
                           (buffer-string)))))

;; Text rendering

(defconst elpher-url-regex
  "\\([a-zA-Z]+\\)://\\([a-zA-Z0-9.\-]*[a-zA-Z0-9\-]\\|\[[a-zA-Z0-9:]+\]\\)\\(:[0-9]+\\)?\\(/\\([0-9a-zA-Z\-_~?/@|:.%#]*[0-9a-zA-Z\-_~?/@|#]\\)?\\)?"
  "Regexp used to locate and buttniofy URLs in text files loaded by elpher.")

(defun elpher-buttonify-urls (string)
  "Turn substrings which look like urls in STRING into clickable buttons."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward elpher-url-regex nil t)
      (let ((node (elpher-make-node (substring-no-properties (match-string 0))
                                    (elpher-address-from-url (match-string 0)))))
          (make-text-button (match-beginning 0)
                            (match-end 0)
                            'elpher-node  node
                            'action #'elpher-click-link
                            'follow-link t
                            'help-echo (elpher-node-button-help node)
                            'face 'button)))
    (buffer-string)))

(defun elpher-render-text (data &optional _mime-type-string)
  "Render DATA as text.  MIME-TYPE-STRING is unused."
  (elpher-with-clean-buffer
   (if (not data)
       t
     (insert (elpher-buttonify-urls (elpher-preprocess-text-response data)))
     (elpher-cache-content
      (elpher-node-address elpher-current-node)
      (buffer-string)))))

;; Image retrieval

(defun elpher-render-image (data &optional _mime-type-string)
  "Display DATA as image.  MIME-TYPE-STRING is unused."
  (if (not data)
      nil
    (if (display-images-p)
        (progn
          (let ((image (create-image
                        data
                        nil t)))
            (elpher-with-clean-buffer
             (insert-image image)
             (elpher-restore-pos))))
      (elpher-render-download data))))

;; Search retrieval and rendering

(defun elpher-get-gopher-query-node (renderer)
  "Getter for gopher addresses requiring input.
The response is rendered using the rendering function RENDERER."
   (let* ((address (elpher-node-address elpher-current-node))
          (content (elpher-get-cached-content address))
          (aborted t))
    (if (and content (funcall renderer nil))
        (elpher-with-clean-buffer
         (insert content)
         (elpher-restore-pos)
         (message "Displaying cached search results.  Reload to perform a new search."))
      (unwind-protect
          (let* ((query-string (read-string "Query: "))
                 (query-selector (concat (elpher-gopher-address-selector address) "\t" query-string))
                 (search-address (elpher-make-gopher-address ?1
                                                             query-selector
                                                             (elpher-address-host address)
                                                             (elpher-address-port address)
                                                             (equal (elpher-address-type address) "gophers"))))
            (setq aborted nil)

            (elpher-with-clean-buffer
             (insert "LOADING RESULTS... (use 'u' to cancel)"))
            (elpher-get-selector search-address renderer))
        (if aborted
            (elpher-visit-parent-node))))))
 
;; Raw server response rendering

(defun elpher-render-raw (data &optional mime-type-string)
  "Display raw DATA in buffer.  MIME-TYPE-STRING is also displayed if provided."
  (if (not data)
      nil
    (elpher-with-clean-buffer
     (when mime-type-string
       (insert "MIME type specified by server: '" mime-type-string "'\n"))
     (insert data)
     (goto-char (point-min)))
    (message "Displaying raw server response.  Reload or redraw to return to standard view.")))

;; File save "rendering"

(defun elpher-render-download (data &optional _mime-type-string)
  "Save DATA to file.  MIME-TYPE-STRING is unused."
  (if (not data)
      nil
    (let* ((address (elpher-node-address elpher-current-node))
           (selector (elpher-gopher-address-selector address)))
      (elpher-visit-parent-node) ; Do first in case of non-local exits.
      (let* ((filename-proposal (file-name-nondirectory selector))
             (filename (read-file-name "Download complete. Save file as: "
                                       nil nil nil
                                       (if (> (length filename-proposal) 0)
                                           filename-proposal
                                         "download.file"))))
        (let ((coding-system-for-write 'binary))
          (with-temp-file filename
            (insert data)))
        (message (format "Saved to file %s." filename))))))

;; HTML rendering

(defun elpher-render-html (data &optional _mime-type-string)
  "Render DATA as HTML using shr.  MIME-TYPE-STRING is unused."
  (elpher-with-clean-buffer
   (if (not data)
       t
     (let ((dom (with-temp-buffer
                  (insert data)
                  (libxml-parse-html-region (point-min) (point-max)))))
       (shr-insert-document dom)))))

;; Gemini node retrieval

(defvar elpher-gemini-redirect-chain)

(defun elpher-get-gemini-response (address renderer &optional force-ipv4)
  "Retrieve gemini ADDRESS, then render using RENDERER.
If FORCE-IPV4 is non-nil, explicitly look up and use IPv4 address corresponding
to ADDRESS."
  (if (not (gnutls-available-p))
      (error "Cannot establish gemini connection: GnuTLS not available")
    (unless (< (elpher-address-port address) 65536)
      (error "Cannot establish gemini connection: port number > 65536"))
    (condition-case nil
        (let* ((kill-buffer-query-functions nil)
               (port (elpher-address-port address))
               (host (elpher-address-host address))
               (response-string "")
               (proc (open-network-stream "elpher-process"
                                          nil
                                          (if force-ipv4 (dns-query host) host)
                                          (if (> port 0) port 1965)
                                          :type 'tls
                                          :nowait t))
               (timer (run-at-time elpher-connection-timeout nil
                                   (lambda ()
                                     (elpher-process-cleanup)
                                     (unless force-ipv4
                                        ; Try again with IPv4
                                       (message "Connection timed out.  Retrying with IPv4.")
                                       (elpher-get-gemini-response address renderer t))))))
          (setq elpher-network-timer timer)
          (set-process-coding-system proc 'binary)
          (set-process-filter proc
                              (lambda (_proc string)
                                (cancel-timer timer)
                                (setq response-string
                                      (concat response-string string))))
          (set-process-sentinel proc
                                (lambda (proc event)
                                  (condition-case the-error
                                      (cond
                                       ((string-prefix-p "open" event)    ; request URL
                                        (let ((inhibit-eol-conversion t))
                                          (process-send-string
                                           proc
                                           (concat (elpher-address-to-url address)
                                                   "\r\n"))))
                                       ((string-prefix-p "deleted" event)) ; do nothing
                                       ((and (string-empty-p response-string)
                                             (not force-ipv4))
                                        ; Try again with IPv4
                                        (message "Connection failed. Retrying with IPv4.")
                                        (cancel-timer timer)
                                        (elpher-get-gemini-response address renderer t))
                                       (t
                                        (funcall #'elpher-process-gemini-response
                                                 response-string
                                                 renderer)
                                        (elpher-restore-pos)))
                                    (error
                                           (elpher-network-error address the-error))))))
      (error
       (error "Error initiating connection to server")))))

(defun elpher-parse-gemini-response (response)
  "Parse the RESPONSE string and return a list of components.
The list is of the form (code meta body).  A response of nil implies
that the response was malformed."
  (let ((header-end-idx (string-match "\r\n" response)))
    (if header-end-idx
        (let ((header (string-trim (substring response 0 header-end-idx)))
              (body (substring response (+ header-end-idx 2))))
          (if (>= (length header) 2)
              (let ((code (substring header 0 2))
                    (meta (string-trim (substring header 2))))
                (list code meta body))
            (error "Malformed response: No response status found in header %s" header)))
      (error "Malformed response: No CRLF-delimited header found"))))

(defun elpher-process-gemini-response (response-string renderer)
  "Process the gemini response RESPONSE-STRING and pass the result to RENDERER."
  (let ((response-components (elpher-parse-gemini-response response-string)))
    (let ((response-code (elt response-components 0))
          (response-meta (elt response-components 1))
          (response-body (elt response-components 2)))
      (pcase (elt response-code 0)
        (?1 ; Input required
         (elpher-with-clean-buffer
          (insert "Gemini server is requesting input."))
         (let* ((query-string (read-string (concat response-meta ": ")))
                (url (elpher-address-to-url (elpher-node-address elpher-current-node)))
                (query-address (elpher-address-from-url (concat url "?" query-string))))
           (elpher-get-gemini-response query-address renderer)))
        (?2 ; Normal response
         (funcall renderer response-body response-meta))
        (?3 ; Redirect
         (message "Following redirect to %s" response-meta)
         (if (>= (length elpher-gemini-redirect-chain) 5)
             (error "More than 5 consecutive redirects followed"))
         (let ((redirect-address (elpher-address-from-gemini-url response-meta)))
           (if (member redirect-address elpher-gemini-redirect-chain)
               (error "Redirect loop detected"))
           (if (not (string= (elpher-address-protocol redirect-address)
                             "gemini"))
               (error "Server tried to automatically redirect to non-gemini URL: %s"
                      response-meta))
           (add-to-list 'elpher-gemini-redirect-chain redirect-address)
           (elpher-get-gemini-response redirect-address renderer)))
        (?4 ; Temporary failure
         (error "Gemini server reports TEMPORARY FAILURE for this request: %s %s"
                response-code response-meta))
        (?5 ; Permanent failure
         (error "Gemini server reports PERMANENT FAILURE for this request: %s %s"
                response-code response-meta))
        (?6 ; Client certificate required
         (error "Gemini server requires client certificate (unsupported at this time)"))
        (_other
         (error "Gemini server response unknown: %s %s"
                response-code response-meta))))))

(defun elpher-get-gemini-node (renderer)
  "Getter which retrieves and renders a Gemini node and renders it using RENDERER."
  (let* ((address (elpher-node-address elpher-current-node))
         (content (elpher-get-cached-content address)))
    (condition-case the-error
        (if (and content (funcall renderer nil))
            (elpher-with-clean-buffer
              (insert content)
              (elpher-restore-pos))
          (elpher-with-clean-buffer
           (insert "LOADING GEMINI... (use 'u' to cancel)"))
          (setq elpher-gemini-redirect-chain nil)
          (elpher-get-gemini-response address renderer))
      (error
       (elpher-network-error address the-error)))))


(defun elpher-render-gemini (body &optional mime-type-string)
  "Render gemini response BODY with rendering MIME-TYPE-STRING."
  (if (not body)
      t
    (let* ((mime-type-string* (if (or (not mime-type-string)
                                      (string-empty-p mime-type-string))
                                  "text/gemini; charset=utf-8"
                                mime-type-string))
           (mime-type-split (split-string mime-type-string* ";" t))
           (mime-type (string-trim (car mime-type-split)))
           (parameters (mapcar (lambda (s)
                                 (let ((key-val (split-string s "=")))
                                   (list (downcase (string-trim (car key-val)))
                                         (downcase (string-trim (cadr key-val))))))
                               (cdr mime-type-split))))
      (when (string-prefix-p "text/" mime-type)
        (setq body (decode-coding-string
                    body
                    (if (assoc "charset" parameters)
                        (intern (cadr (assoc "charset" parameters)))
                      'utf-8)))
        (setq body (replace-regexp-in-string "\r" "" body)))
      (pcase mime-type
        ((or "text/gemini" "")
         (elpher-render-gemini-map body parameters))
        ("text/html"
         (elpher-render-html body))
        ((pred (string-prefix-p "text/"))
         (elpher-render-gemini-plain-text body parameters))
        ((pred (string-prefix-p "image/"))
         (elpher-render-image body))
        (_other
         (error "Unsupported MIME type %S" mime-type))))))

(defun elpher-gemini-get-link-url (line)
  "Extract the url portion of LINE, a gemini map file link line."
  (string-trim (elt (split-string (substring line 2)) 0)))

(defun elpher-gemini-get-link-display-string (line)
  "Extract the display string portion of LINE, a gemini map file link line."
  (let* ((rest (string-trim (elt (split-string line "=>") 1)))
         (idx (string-match "[ \t]" rest)))
    (if idx
        (string-trim (substring rest (+ idx 1)))
      "")))

(defun elpher-collapse-dot-sequences (filename)
  "Collapse dot sequences in FILENAME.
For instance, the filename /a/b/../c/./d will reduce to /a/c/d"
  (let* ((path (split-string filename "/"))
         (path-reversed-normalized
          (seq-reduce (lambda (a b)
                        (cond ((and a (equal b "..") (cdr a)))
                              ((and (not a) (equal b "..")) a) ;leading .. are dropped
                              ((equal b ".") a)
                              (t (cons b a))))
                      path nil)))
    (string-join (reverse path-reversed-normalized) "/")))

(defun elpher-address-from-gemini-url (url)
  "Extract address from URL with defaults as per gemini map files."
  (let ((address (url-generic-parse-url url)))
    (unless (and (url-type address) (not (url-fullness address))) ;avoid mangling mailto: urls
      (setf (url-fullness address) t)
      (if (url-host address) ;if there is an explicit host, filenames are absolute
          (if (string-empty-p (url-filename address))
              (setf (url-filename address) "/")) ;ensure empty filename is marked as absolute
        (setf (url-host address) (url-host (elpher-node-address elpher-current-node)))
        (unless (string-prefix-p "/" (url-filename address)) ;deal with relative links
          (setf (url-filename address)
                (concat (file-name-directory
                         (url-filename (elpher-node-address elpher-current-node)))
                        (url-filename address)))))
      (unless (url-type address)
        (setf (url-type address) "gemini"))
      (if (equal (url-type address) "gemini")
          (setf (url-filename address)
                (elpher-collapse-dot-sequences (url-filename address)))))
    address))

(defun elpher-render-gemini-map (data _parameters)
  "Render DATA as a gemini map file, PARAMETERS is currently unused."
  (elpher-with-clean-buffer
   (dolist (line (split-string data "\n"))
     (if (string-prefix-p "=>" line)
         (let* ((url (elpher-gemini-get-link-url line))
                (display-string (elpher-gemini-get-link-display-string line))
                (address (elpher-address-from-gemini-url url)))
           (if (> (length display-string) 0)
               (elpher-insert-index-record display-string address)
             (elpher-insert-index-record url address)))
       (elpher-insert-index-record line)))
   (elpher-cache-content
    (elpher-node-address elpher-current-node)
    (buffer-string))))

(defun elpher-render-gemini-plain-text (data _parameters)
  "Render DATA as plain text file.  PARAMETERS is currently unused."
  (elpher-with-clean-buffer
   (insert (elpher-buttonify-urls data))
   (elpher-cache-content
    (elpher-node-address elpher-current-node)
    (buffer-string))))

;; Other URL node opening

(defun elpher-get-other-url-node (renderer)
  "Getter which attempts to open the URL specified by the current node (RENDERER must be nil)."
  (when renderer
    (elpher-visit-parent-node)
    (error "Command not supported for general URLs"))
  (let* ((address (elpher-node-address elpher-current-node))
         (url (elpher-address-to-url address)))
    (progn
      (elpher-visit-parent-node) ; Do first in case of non-local exits.
      (message "Opening URL...")
      (if elpher-open-urls-with-eww
          (browse-web url)
        (browse-url url)))))

;; Telnet node connection

(defun elpher-get-telnet-node (renderer)
  "Opens a telnet connection to the current node address (RENDERER must be nil)."
  (when renderer
    (elpher-visit-parent-node)
    (error "Command not supported for telnet URLs"))
  (let* ((address (elpher-node-address elpher-current-node))
         (host (elpher-address-host address))
         (port (elpher-address-port address)))
    (elpher-visit-parent-node)
    (if (> port 0)
        (telnet host port)
      (telnet host))))

;; Start page node retrieval

(defun elpher-get-start-node (renderer)
  "Getter which displays the start page (RENDERER must be nil)."
  (when renderer
    (elpher-visit-parent-node)
    (error "Command not supported for start page"))
  (elpher-with-clean-buffer
   (insert "     --------------------------------------------\n"
           "                Elpher Gopher Client             \n"
           "                   version " elpher-version "\n"
           "     --------------------------------------------\n"
           "\n"
           "Default bindings:\n"
           "\n"
           " - TAB/Shift-TAB: next/prev item on current page\n"
           " - RET/mouse-1: open item under cursor\n"
           " - m: select an item on current page by name (autocompletes)\n"
           " - u/mouse-3: return to previous page\n"
           " - o/O: visit different selector or the root menu of the current server\n"
           " - g: go to a particular gopher address\n"
           " - d/D: download item under cursor or current page\n"
           " - i/I: info on item under cursor or current page\n"
           " - c/C: copy URL representation of item under cursor or current page\n"
           " - a/A: bookmark the item under cursor or current page\n"
           " - x/X: remove bookmark for item under cursor or current page\n"
           " - B: visit the bookmarks page\n"
           " - r: redraw current page (using cached contents if available)\n"
           " - R: reload current page (regenerates cache)\n"
           " - S: set character coding system for gopher (default is to autodetect)\n"
           " - T: toggle TLS gopher mode\n"
           " - .: display the raw server response for the current page\n"
           "\n"
           "Start your exploration of gopher space:\n")
   (elpher-insert-index-record "Floodgap Systems Gopher Server"
                               (elpher-make-gopher-address ?1 "" "gopher.floodgap.com" 70))
   (insert "\n"
           "Alternatively, select the following item and enter some search terms:\n")
   (elpher-insert-index-record "Veronica-2 Gopher Search Engine"
                               (elpher-make-gopher-address ?7 "/v2/vs" "gopher.floodgap.com" 70))
   (insert "\n"
           "This page contains your bookmarked sites (also visit with B):\n")
   (elpher-insert-index-record "Your Bookmarks" 'bookmarks)
   (insert "\n"
           "For Elpher release news or to leave feedback, visit:\n")
   (elpher-insert-index-record "The Elpher Project Page"
                               (elpher-make-gopher-address ?1
                                                           "/projects/elpher/"
                                                           "thelambdalab.xyz"
                                                           70))
   (insert "\n"
           "** Refer to the ")
   (let ((help-string "RET,mouse-1: Open Elpher info manual (if available)"))
     (insert-text-button "Elpher info manual"
                         'face 'link
                         'action (lambda (_)
                                   (interactive)
                                   (info "(elpher)"))
                         'follow-link t
                         'help-echo help-string))
   (insert " for the full documentation. **\n")
   (insert (propertize
            (concat "  (This should be available if you have installed Elpher using\n"
                    "   MELPA. Otherwise you will have to install the manual yourself.)\n")
            'face 'shadow))
   (elpher-restore-pos)))

;; Bookmarks page node retrieval

(defun elpher-get-bookmarks-node (renderer)
  "Getter to load and display the current bookmark list (RENDERER must be nil)."
  (when renderer
    (elpher-visit-parent-node)
    (error "Command not supported for bookmarks page"))
  (elpher-with-clean-buffer
   (insert "---- Bookmark list ----\n\n")
   (let ((bookmarks (elpher-load-bookmarks)))
     (if bookmarks
         (dolist (bookmark bookmarks)
           (let ((display-string (elpher-bookmark-display-string bookmark))
                 (address (elpher-address-from-url (elpher-bookmark-url bookmark))))
             (elpher-insert-index-record display-string address)))
       (insert "No bookmarks found.\n")))
   (insert "\n-----------------------\n"
           "\n"
           "- u: return to previous page\n"
           "- x: delete selected bookmark\n"
           "- a: rename selected bookmark\n"
           "\n"
           "Bookmarks are stored in the file ")
   (let ((filename (locate-user-emacs-file "elpher-bookmarks"))
         (help-string "RET,mouse-1: Open bookmarks file in new buffer for editing."))
     (insert-text-button filename
                         'face 'link
                         'action (lambda (_)
                                   (interactive)
                                   (find-file filename))
                         'follow-link t
                         'help-echo help-string))
   (insert "\n")
   (elpher-restore-pos)))
  

;;; Bookmarks
;;

(defun elpher-make-bookmark (display-string url)
  "Make an elpher bookmark.
DISPLAY-STRING determines how the bookmark will appear in the
bookmark list, while URL is the url of the entry."
  (list display-string url))
  
(defun elpher-bookmark-display-string (bookmark)
  "Get the display string of BOOKMARK."
  (elt bookmark 0))

(defun elpher-set-bookmark-display-string (bookmark display-string)
  "Set the display string of BOOKMARK to DISPLAY-STRING."
  (setcar bookmark display-string))

(defun elpher-bookmark-url (bookmark)
  "Get the address for BOOKMARK."
  (elt bookmark 1))

(defun elpher-save-bookmarks (bookmarks)
  "Record the bookmark list BOOKMARKS to the user's bookmark file.
Beware that this completely replaces the existing contents of the file."
  (with-temp-file (locate-user-emacs-file "elpher-bookmarks")
    (erase-buffer)
    (insert "; Elpher bookmarks file\n\n"
            "; Bookmarks are stored as a list of (label URL) items.\n"
            "; Feel free to edit by hand, but take care to ensure\n"
            "; the list structure remains intact.\n\n")
    (pp bookmarks (current-buffer))))

(defun elpher-load-bookmarks ()
  "Get the list of bookmarks from the users's bookmark file."
  (let ((bookmarks
         (with-temp-buffer
           (ignore-errors
             (insert-file-contents (locate-user-emacs-file "elpher-bookmarks"))
             (goto-char (point-min))
             (read (current-buffer))))))
    (if (and bookmarks (listp (cadar bookmarks)))
        (progn
          (message "Reading old bookmark file. (Will be updated on write.)")
          (mapcar (lambda (old-bm)
                    (list (car old-bm)
                          (elpher-address-to-url (apply #'elpher-make-gopher-address
                                                        (cadr old-bm)))))
                  bookmarks))
      bookmarks)))

(defun elpher-add-address-bookmark (address display-string)
  "Save a bookmark for ADDRESS with label DISPLAY-STRING.)))
If ADDRESS is already bookmarked, update the label only."
  (let ((bookmarks (elpher-load-bookmarks))
        (url (elpher-address-to-url address)))
    (let ((existing-bookmark (rassoc (list url) bookmarks)))
      (if existing-bookmark
          (elpher-set-bookmark-display-string existing-bookmark display-string)
        (push (elpher-make-bookmark display-string url) bookmarks)))
    (elpher-save-bookmarks bookmarks)))

(defun elpher-remove-address-bookmark (address)
  "Remove any bookmark to ADDRESS."
  (let ((url (elpher-address-to-url address)))
    (elpher-save-bookmarks
     (seq-filter (lambda (bookmark)
                   (not (equal (elpher-bookmark-url bookmark) url)))
                 (elpher-load-bookmarks)))))

;;; Interactive procedures
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
  "Go to a particular gopher site read from the minibuffer."
  (interactive)
  (let ((node
         (let ((host-or-url (read-string "Gopher or Gemini URL: ")))
           (elpher-make-node host-or-url
                             (elpher-address-from-url host-or-url)))))
    (switch-to-buffer "*elpher*")
    (elpher-visit-node node)))

(defun elpher-go-current ()
  "Go to a particular site read from the minibuffer, initialized with the current URL."
  (interactive)
  (let ((address (elpher-node-address elpher-current-node)))
    (if (elpher-address-special-p address)
        (error "Command invalid for this page")
      (let ((url (read-string "Gopher or Gemini URL: " (elpher-address-to-url address))))
        (elpher-visit-node (elpher-make-node url (elpher-address-from-url url)))))))

(defun elpher-redraw ()
  "Redraw current page."
  (interactive)
  (if elpher-current-node
      (elpher-visit-node elpher-current-node)
    (message "No current site.")))

(defun elpher-reload ()
  "Reload current page."
  (interactive)
  (if elpher-current-node
      (elpher-reload-current-node)
    (message "No current site.")))

(defun elpher-toggle-tls ()
  "Toggle TLS encryption mode for gopher."
  (interactive)
  (setq elpher-use-tls (not elpher-use-tls))
  (if elpher-use-tls
      (if (gnutls-available-p)
          (message "TLS gopher mode enabled.  (Will not affect current page until reload.)")
        (setq elpher-use-tls nil)
        (error "Cannot enable TLS gopher mode: GnuTLS not available"))
    (message "TLS gopher mode disabled.  (Will not affect current page until reload.)")))

(defun elpher-view-raw ()
  "View raw server response for current page."
  (interactive)
  (if elpher-current-node
      (if (elpher-address-special-p (elpher-node-address elpher-current-node))
          (error "This page was not generated by a server")
        (elpher-visit-node elpher-current-node
                           #'elpher-render-raw))
    (message "No current site.")))

(defun elpher-back ()
  "Go to previous site."
  (interactive)
  (if (elpher-node-parent elpher-current-node)
      (elpher-visit-parent-node)
    (error "No previous site")))

(defun elpher-download ()
  "Download the link at point."
  (interactive)
  (let ((button (button-at (point))))
    (if button
        (let ((node (button-get button 'elpher-node)))
          (if (elpher-address-special-p (elpher-node-address node))
              (error "Cannot download %s"
                     (elpher-node-display-string node))
            (elpher-visit-node (button-get button 'elpher-node)
                               #'elpher-render-download)))
      (error "No link selected"))))

(defun elpher-download-current ()
  "Download the current page."
  (interactive)
  (if (elpher-address-special-p (elpher-node-address elpher-current-node))
      (error "Cannot download %s"
             (elpher-node-display-string elpher-current-node))
    (elpher-visit-node (elpher-make-node
                        (elpher-node-display-string elpher-current-node)
                        (elpher-node-address elpher-current-node)
                        elpher-current-node)
                       #'elpher-render-download
                       t)))

(defun elpher-build-link-map ()
  "Build alist mapping link names to destination nodes in current buffer."
  (let ((link-map nil)
        (b (next-button (point-min) t)))
    (while b
      (push (cons (button-label b) b) link-map)
      (setq b (next-button (button-start b))))
    link-map))

(defun elpher-jump ()
  "Select a directory entry by name.  Similar to the info browser (m)enu command."
  (interactive)
  (let* ((link-map (elpher-build-link-map)))
    (if link-map
        (let ((key (let ((completion-ignore-case t))
                     (completing-read "Directory item/link: "
                                      link-map nil t))))
          (if (and key (> (length key) 0))
              (let ((b (cdr (assoc key link-map))))
                (goto-char (button-start b))
                (button-activate b)))))))

(defun elpher-root-dir ()
  "Visit root of current server."
  (interactive)
  (let ((address (elpher-node-address elpher-current-node)))
    (if (not (elpher-address-special-p address))
        (if (or (member (url-filename address) '("/" ""))
                (and (elpher-address-gopher-p address)
                     (= (length (elpher-gopher-address-selector address)) 0)))
            (error "Already at root directory of current server")
          (let ((address-copy (elpher-address-from-url
                               (elpher-address-to-url address))))
            (setf (url-filename address-copy) "")
            (elpher-visit-node
             (elpher-make-node (elpher-address-to-url address-copy)
                               address-copy))))
      (error "Command invalid for %s" (elpher-node-display-string elpher-current-node)))))

(defun elpher-bookmarks-current-p ()
  "Return non-nil if current node is a bookmarks page."
  (equal (elpher-address-type (elpher-node-address elpher-current-node))
         '(special bookmarks)))

(defun elpher-reload-bookmarks ()
  "Reload bookmarks if current node is a bookmarks page."
  (if (elpher-bookmarks-current-p)
      (elpher-reload-current-node)))

(defun elpher-bookmark-current ()
  "Bookmark the current node."
  (interactive)
  (let ((address (elpher-node-address elpher-current-node))
        (display-string (elpher-node-display-string elpher-current-node)))
    (if (not (elpher-address-special-p address))
        (let ((bookmark-display-string (read-string "Bookmark display string: "
                                                    display-string)))
          (elpher-add-address-bookmark address bookmark-display-string)
          (message "Bookmark added."))
      (error "Cannot bookmark %s" display-string))))

(defun elpher-bookmark-link ()
  "Bookmark the link at point."
  (interactive)
  (let ((button (button-at (point))))
    (if button
        (let* ((node (button-get button 'elpher-node))
               (address (elpher-node-address node))
               (display-string (elpher-node-display-string node)))
          (if (not (elpher-address-special-p address))
              (let ((bookmark-display-string (read-string "Bookmark display string: "
                                                          display-string)))
                (elpher-add-address-bookmark address bookmark-display-string)
                (elpher-reload-bookmarks)
                (message "Bookmark added."))
            (error "Cannot bookmark %s" display-string)))
      (error "No link selected"))))

(defun elpher-unbookmark-current ()
  "Remove bookmark for the current node."
  (interactive)
  (let ((address (elpher-node-address elpher-current-node)))
    (unless (elpher-address-special-p address)
      (elpher-remove-address-bookmark address)
      (message "Bookmark removed."))))

(defun elpher-unbookmark-link ()
  "Remove bookmark for the link at point."
  (interactive)
  (let ((button (button-at (point))))
    (if button
        (let ((node (button-get button 'elpher-node)))
          (elpher-remove-address-bookmark (elpher-node-address node))
          (elpher-reload-bookmarks)
          (message "Bookmark removed."))
      (error "No link selected"))))

(defun elpher-bookmarks ()
  "Visit bookmarks page."
  (interactive)
  (switch-to-buffer "*elpher*")
  (elpher-visit-node
   (elpher-make-node "Bookmarks Page" (elpher-make-special-address 'bookmarks))))

(defun elpher-info-node (node)
  "Display information on NODE."
  (let ((display-string (elpher-node-display-string node))
        (address (elpher-node-address node)))
    (if (elpher-address-special-p address)
        (message "Special page: %s" display-string)
      (message "%s" (elpher-address-to-url address)))))

(defun elpher-info-link ()
  "Display information on node corresponding to link at point."
  (interactive)
  (let ((button (button-at (point))))
    (if button
        (elpher-info-node (button-get button 'elpher-node))
      (error "No item selected"))))
  
(defun elpher-info-current ()
  "Display information on current node."
  (interactive)
  (elpher-info-node elpher-current-node))

(defun elpher-copy-node-url (node)
  "Copy URL representation of address of NODE to `kill-ring'."
  (let ((address (elpher-node-address node)))
    (if (elpher-address-special-p address)
        (error (format "Cannot represent %s as URL" (elpher-node-display-string node)))
      (let ((url (elpher-address-to-url address)))
        (message "Copied \"%s\" to kill-ring/clipboard." url)
        (kill-new url)))))

(defun elpher-copy-link-url ()
  "Copy URL of item at point to `kill-ring'."
  (interactive)
  (let ((button (button-at (point))))
    (if button
        (elpher-copy-node-url (button-get button 'elpher-node))
      (error "No item selected"))))

(defun elpher-copy-current-url ()
  "Copy URL of current node to `kill-ring'."
  (interactive)
  (elpher-copy-node-url elpher-current-node))

(defun elpher-set-gopher-coding-system ()
  "Specify an explicit character coding system for gopher selectors."
  (interactive)
  (let ((system (read-coding-system "Set coding system to use for gopher (default is to autodetect): " nil)))
    (setq elpher-user-coding-system system)
    (if system
        (message "Gopher coding system fixed to %s. (Reload to see effect)." system)
      (message "Gopher coding system set to autodetect. (Reload to see effect)."))))


;;; Mode and keymap
;;

(defvar elpher-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'elpher-next-link)
    (define-key map (kbd "<backtab>") 'elpher-prev-link)
    (define-key map (kbd "u") 'elpher-back)
    (define-key map [mouse-3] 'elpher-back)
    (define-key map (kbd "O") 'elpher-root-dir)
    (define-key map (kbd "g") 'elpher-go)
    (define-key map (kbd "o") 'elpher-go-current)
    (define-key map (kbd "r") 'elpher-redraw)
    (define-key map (kbd "R") 'elpher-reload)
    (define-key map (kbd "T") 'elpher-toggle-tls)
    (define-key map (kbd ".") 'elpher-view-raw)
    (define-key map (kbd "d") 'elpher-download)
    (define-key map (kbd "D") 'elpher-download-current)
    (define-key map (kbd "m") 'elpher-jump)
    (define-key map (kbd "i") 'elpher-info-link)
    (define-key map (kbd "I") 'elpher-info-current)
    (define-key map (kbd "c") 'elpher-copy-link-url)
    (define-key map (kbd "C") 'elpher-copy-current-url)
    (define-key map (kbd "a") 'elpher-bookmark-link)
    (define-key map (kbd "A") 'elpher-bookmark-current)
    (define-key map (kbd "x") 'elpher-unbookmark-link)
    (define-key map (kbd "X") 'elpher-unbookmark-current)
    (define-key map (kbd "B") 'elpher-bookmarks)
    (define-key map (kbd "S") 'elpher-set-gopher-coding-system)
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'motion map
        (kbd "TAB") 'elpher-next-link
        (kbd "C-") 'elpher-follow-current-link
        (kbd "C-t") 'elpher-back
        (kbd "u") 'elpher-back
        [mouse-3] 'elpher-back
        (kbd "g") 'elpher-go
        (kbd "o") 'elpher-go-current
        (kbd "r") 'elpher-redraw
        (kbd "R") 'elpher-reload
        (kbd "T") 'elpher-toggle-tls
        (kbd ".") 'elpher-view-raw
        (kbd "d") 'elpher-download
        (kbd "D") 'elpher-download-current
        (kbd "m") 'elpher-jump
        (kbd "i") 'elpher-info-link
        (kbd "I") 'elpher-info-current
        (kbd "c") 'elpher-copy-link-url
        (kbd "C") 'elpher-copy-current-url
        (kbd "a") 'elpher-bookmark-link
        (kbd "A") 'elpher-bookmark-current
        (kbd "x") 'elpher-unbookmark-link
        (kbd "X") 'elpher-unbookmark-current
        (kbd "B") 'elpher-bookmarks
        (kbd "S") 'elpher-set-gopher-coding-system))
    map)
  "Keymap for gopher client.")

(define-derived-mode elpher-mode special-mode "elpher"
  "Major mode for elpher, an elisp gopher client.

This mode is automatically enabled by the interactive
functions which initialize the gopher client, namely
`elpher', `elpher-go' and `elpher-bookmarks'.")

(when (fboundp 'evil-set-initial-state)
  (evil-set-initial-state 'elpher-mode 'motion))


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
    (let ((start-node (elpher-make-node "Elpher Start Page"
                                        (elpher-make-special-address 'start))))
      (elpher-visit-node start-node)))
  "Started Elpher.") ; Otherwise (elpher) evaluates to start page string.

;;; elpher.el ends here
