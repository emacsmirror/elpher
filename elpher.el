;;; elpher.el --- A friendly gopher and gemini client  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2022 Tim Vaughan <plugd@thelambdalab.xyz>
;; Copyright (C) 2020-2022 Elpher contributors (See info manual for full list)

;; Author: Tim Vaughan <plugd@thelambdalab.xyz>
;; Created: 11 April 2019
;; Version: 3.4.2
;; Keywords: comm gopher
;; Homepage: https://thelambdalab.xyz/elpher
;; Package-Requires: ((emacs "27.1"))

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

;; Elpher aims to provide a practical and friendly gopher and gemini
;; client for GNU Emacs.  It supports:

;; - intuitive keyboard and mouse-driven browsing,
;; - out-of-the-box compatibility with evil-mode,
;; - clickable web and gopher links *in plain text*,
;; - caching of visited sites,
;; - pleasant and configurable colouring of Gopher directories,
;; - direct visualisation of image files,
;; - gopher connections using TLS encryption,
;; - the fledgling Gemini protocol,
;; - the greybeard Finger protocol.

;; To launch Elpher, simply use 'M-x elpher'.  This will open a start
;; page containing information on key bindings and suggested starting
;; points for your gopher exploration.

;; Full instructions can be found in the Elpher info manual.

;; Elpher is under active development.  Any suggestions for
;; improvements are welcome, and can be made on the official project
;; page, gopher://thelambdalab.xyz/1/projects/elpher, or via the
;; project mailing list at https://lists.sr.ht/~michel-slm/elpher.

;;; Code:

(provide 'elpher)

;;; Dependencies
;;

(require 'seq)
(require 'shr)
(require 'url-util)
(require 'subr-x)
(require 'nsm)
(require 'gnutls)
(require 'socks)
(require 'bookmark)

;;; Global constants
;;

(defconst elpher-version "3.4.2"
  "Current version of elpher.")

(defconst elpher-margin-width 6
  "Width of left-hand margin used when rendering indicies.")

(defconst elpher-type-map
  '(((gopher ?0) elpher-get-gopher-page elpher-render-text "txt" elpher-text)
    ((gopher ?1) elpher-get-gopher-page elpher-render-index "/" elpher-index)
    ((gopher ?4) elpher-get-gopher-page elpher-render-download "bin" elpher-binary)
    ((gopher ?5) elpher-get-gopher-page elpher-render-download "bin" elpher-binary)
    ((gopher ?7) elpher-get-gopher-query-page elpher-render-index "?" elpher-search)
    ((gopher ?9) elpher-get-gopher-page elpher-render-download "bin" elpher-binary)
    ((gopher ?g) elpher-get-gopher-page elpher-render-image "img" elpher-image)
    ((gopher ?p) elpher-get-gopher-page elpher-render-image "img" elpher-image)
    ((gopher ?I) elpher-get-gopher-page elpher-render-image "img" elpher-image)
    ((gopher ?d) elpher-get-gopher-page elpher-render-download "doc" elpher-binary)
    ((gopher ?P) elpher-get-gopher-page elpher-render-download "doc" elpher-binary)
    ((gopher ?s) elpher-get-gopher-page elpher-render-download "snd" elpher-binary)
    ((gopher ?h) elpher-get-gopher-page elpher-render-html "htm" elpher-html)
    (gemini elpher-get-gemini-page elpher-render-gemini "gem" elpher-gemini)
    (finger elpher-get-finger-page elpher-render-text "txt" elpher-text)
    (telnet elpher-get-telnet-page nil "tel" elpher-telnet)
    (other-url elpher-get-other-url-page nil "url" elpher-other-url)
    (file elpher-get-file-page nil "~" elpher-gemini)
    ((about welcome) elpher-get-welcome-page nil "E" elpher-index)
    ((about bookmarks) elpher-get-bookmarks-page nil "E" elpher-index)
    ((about history) elpher-get-history-page nil "E" elpher-index)
    ((about visited-pages) elpher-get-visited-pages-page nil "E" elpher-index))
  "Association list from types to getters, renderers, margin codes and index faces.")


;;; Declarations to avoid compiler warnings.
;;

(eval-when-compile
  (declare-function ansi-color-filter-apply "ansi-color")
  (declare-function ansi-color-apply "ansi-color")
  (declare-function bookmark-store "bookmark")
  (declare-function org-link-store-props "ol")
  (declare-function org-link-set-parameters "ol")
  (defvar ansi-color-context)
  (defvar xterm-color--current-fg)
  (defvar xterm-color--current-bg)
  (defvar bookmark-make-record-function)
  (defvar mu4e~view-beginning-of-url-regexp)
  (defvar eww-use-browse-url)
  (defvar thing-at-point-uri-schemes))


;;; Customization group
;;

(defgroup elpher nil
  "A gopher and gemini client."
  :group 'applications)

;; General appearance and customizations

(defcustom elpher-open-urls-with-eww nil
  "If non-nil, open URL selectors using eww.
Otherwise, use the system browser via the `browse-url' function."
  :type '(boolean))

(defcustom elpher-use-header t
  "If non-nil, display current page information in buffer header."
  :type '(boolean))

(defcustom elpher-auto-disengage-TLS nil
  "If non-nil, automatically disengage TLS following an unsuccessful connection.
While enabling this may seem convenient, it is also potentially
dangerous as it allows switching from an encrypted channel back to
plain text without user input."
  :type '(boolean))

(defcustom elpher-connection-timeout 5
  "Specifies the number of seconds to wait for a network connection to time out."
  :type '(integer))

(defcustom elpher-filter-ansi-from-text nil
  "If non-nil, filter ANSI escape sequences from text.
The default behaviour is to use the ansi-color package (or xterm-color if it is
available) to interpret these sequences."
  :type '(boolean))

(defcustom elpher-certificate-directory
  (file-name-as-directory (locate-user-emacs-file "elpher-certificates"))
  "Specify the name of the directory where client certificates will be stored.
These certificates may be used for establishing authenticated TLS connections."
  :type '(directory))

(defcustom elpher-openssl-command "openssl"
  "The command used to launch openssl when generating TLS client certificates."
  :type '(file))

(defcustom elpher-default-url-type "gopher"
  "Default URL type (i.e. scheme) to assume if not explicitly given."
  :type '(choice (const "gopher")
                 (const "gemini")))

(defcustom elpher-gemini-TLS-cert-checks nil
  "If non-nil, verify gemini server TLS certs using the default security level.
Otherwise, certificate verification is disabled.

This defaults to off because it is standard practice for Gemini servers
to use self-signed certificates, meaning that most servers provide what
EMACS considers to be an invalid certificate."
  :type '(boolean))

(defcustom elpher-gemini-max-fill-width 80
  "Specify the maximum default width (in columns) of text/gemini documents.
The actual width used is the minimum of this value and the window width at
the time when the text is rendered."
  :type '(integer))

(defcustom elpher-gemini-link-string "→ "
  "Specify the string used to indicate links when rendering gemini maps.
May be empty."
  :type '(string))

(defcustom elpher-gemini-bullet-string "•"
  "Specify the string used for bullets when rendering gemini maps."
  :type '(string))

(defcustom elpher-ipv4-always nil
  "If non-nil, elpher will always use IPv4 to establish network connections.
This can be useful when browsing from a computer that supports IPv6, because
some servers which do not support IPv6 can take a long time to time-out."
  :type '(boolean))

(defcustom elpher-socks-always nil
  "If non-nil, elpher will establish network connections over a SOCKS proxy.
Otherwise, the SOCKS proxy is only used for connections to onion services."
  :type '(boolean))

(defcustom elpher-use-emacs-bookmark-menu nil
  "If non-nil, elpher will only use the native Emacs bookmark menu.
Otherwise, \\[elpher-show-bookmarks] will visit a special elpher bookmark
page within which all of the standard elpher keybindings are active."
  :type '(boolean))

(defcustom elpher-start-page-url "about:welcome"
  "Specify the page displayed initially by elpher.
The default welcome screen is \"about:welcome\", while the bookmarks list
is \"about:bookmarks\".  You can also specify local files via \"file:\".

Beware that using \"about:bookmarks\" as a start page in combination with
the `elpher-use-bookmark-menu' variable set to non-nil will prevent the
Emacs bookmark menu being accessible via \\[elpher-show-bookmarks] from
the start page."
  :type '(string))

(defcustom elpher-gemini-hide-preformatted nil
  "Cause elpher to hide preformatted gemini text by default.
When this option is enabled, preformatted text in text/gemini documents
is replaced with a button which can be used to toggle its display.

This is intended to improve accessibility, as preformatted text often
includes art which can be difficult for screen readers to interpret
meaningfully."
  :type '(boolean))

(defcustom elpher-gemini-preformatted-toggle-bullet "‣ "
  "Margin symbol used to distinguish the preformatted text toggle."
  :type '(string))

(defcustom elpher-gemini-preformatted-toggle-label "[Toggle Preformatted Text]"
  "Label of button used to toggle formatted text."
  :type '(string))

;; Face customizations

(defgroup elpher-faces nil
  "Elpher face customizations."
  :group 'elpher)

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
  '((t :inherit font-lock-constant-face))
  "Face used for Gemini type directory records.")

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

(defface elpher-gemini-heading1
  '((t :inherit bold :height 1.8))
  "Face used for gemini heading level 1.")

(defface elpher-gemini-heading2
  '((t :inherit bold :height 1.5))
  "Face used for gemini heading level 2.")

(defface elpher-gemini-heading3
  '((t :inherit bold :height 1.2))
  "Face used for gemini heading level 3.")

(defface elpher-gemini-quoted
  '((t :inherit font-lock-doc-face))
  "Face used for gemini quoted texts.")

(defface elpher-gemini-preformatted
  '((t :inherit default))
  "Face used for gemini preformatted text.")

(defface elpher-gemini-preformatted-toggle
  '((t :inherit button))
  "Face used for buttons used to toggle display of preformatted text.")

;;; Model
;;

;; Address

;; An elpher "address" object is either a url object or a symbol.
;; Addresses with the "about" type, corresponding to pages generated
;; dynamically for and by elpher.  All others represent pages which
;; rely on content retrieved over the network.

(defun elpher-address-from-url (url-string &optional default-scheme)
  "Create a ADDRESS object corresponding to the given URL-STRING.
If DEFAULT-SCHEME is non-nil, this sets the scheme of the URL when one
is not explicitly given."
  (let ((data (match-data))) ; Prevent parsing clobbering match data
    (unwind-protect
        (let ((url (url-generic-parse-url url-string)))
          (unless (and (not (url-fullness url)) (url-type url))
            (unless (url-type url)
              (setf (url-type url) default-scheme))
            (unless (url-host url)
              (let ((p (split-string (url-filename url) "/" nil nil)))
                (setf (url-host url) (car p))
                (setf (url-filename url)
                      (if (cdr p)
                          (concat "/" (mapconcat #'identity (cdr p) "/"))
                        ""))))
            (when (not (string-empty-p (url-host url)))
              (setf (url-fullness url) t)
              (setf (url-host url) (puny-encode-domain (url-host url))))
            (when (or (equal "gopher" (url-type url))
                      (equal "gophers" (url-type url)))
              ;; Gopher defaults
              (when (or (equal (url-filename url) "")
                        (equal (url-filename url) "/"))
                (setf (url-filename url) "/1")))
            (when (equal "gemini" (url-type url))
              ;; Gemini defaults
              (if (equal (url-filename url) "")
                  (setf (url-filename url) "/"))))
          (elpher-remove-redundant-ports url))
      (set-match-data data))))

(defun elpher-remove-redundant-ports (address)
  "Remove redundant port specifiers from ADDRESS.
Here 'redundant' means that the specified port matches the default
for that protocol, eg 70 for gopher."
  (if (and (not (elpher-address-about-p address))
           (eq (url-portspec address) ; (url-port) is too slow!
               (pcase (url-type address)
                 ("gemini" 1965)
                 ((or "gopher" "gophers") 70)
                 ("finger" 79)
                 (_ -1))))
      (setf (url-portspec address) nil))
  address)

(defun elpher-make-gopher-address (type selector host port &optional tls)
  "Create an ADDRESS object using gopher directory record attributes.
The basic attributes include: TYPE, SELECTOR, HOST and PORT.
If the optional attribute TLS is non-nil, the address will be marked as
requiring gopher-over-TLS."
  (cond
   ((equal type ?i) nil)
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

(defun elpher-make-about-address (type)
  "Create an ADDRESS object corresponding to the given about address TYPE."
  (elpher-address-from-url (concat "about:" (symbol-name type))))

(defun elpher-address-to-url (address)
  "Get string representation of ADDRESS."
  (url-encode-url (url-recreate-url address)))

(defun elpher-address-type (address)
  "Retrieve type of ADDRESS object.
This is used to determine how to retrieve and render the document the
address refers to, via the table `elpher-type-map'."
  (pcase (url-type address)
    ("about"
     (list 'about (intern (url-filename address))))
    ((or "gopher" "gophers")
     (list 'gopher
           (if (member (url-filename address) '("" "/"))
               ?1
             (string-to-char (substring (url-filename address) 1)))))
    ("gemini" 'gemini)
    ("telnet" 'telnet)
    ("finger" 'finger)
    ("file" 'file)
    (_ 'other-url)))

(defun elpher-address-about-p (address)
  "Return non-nil if ADDRESS is an about address."
  (pcase (elpher-address-type address) (`(about ,_) t)))

(defun elpher-address-gopher-p (address)
  "Return non-nil if ADDRESS object is a gopher address."
  (pcase (elpher-address-type address) (`(gopher ,_) t)))

(defun elpher-address-protocol (address)
  "Retrieve the transport protocol for ADDRESS."
  (url-type address))

(defun elpher-address-filename (address)
  "Retrieve the filename component of ADDRESS.
For gopher addresses this is a combination of the selector type and selector."
  (url-unhex-string (url-filename address)))

(defun elpher-address-host (address)
  "Retrieve host from ADDRESS object."
  (let ((host-pre (url-host address)))
    ;; The following strips out square brackets which sometimes enclose IPv6
    ;; addresses.  Doing this here rather than at the parsing stage may seem
    ;; weird, but this lets us way we avoid having to muck with both URL parsing
    ;; and reconstruction.  It's also more efficient, as this method is not
    ;; called during page rendering.
    (if (and (> (length host-pre) 2)
             (eq (elt host-pre 0) ?\[)
             (eq (elt host-pre (- (length host-pre) 1)) ?\]))
        (substring host-pre 1 (- (length host-pre) 1))
      host-pre)))

(defun elpher-address-user (address)
  "Retrieve user from ADDRESS object."
  (url-user address))

(defun elpher-address-port (address)
  "Retrieve port from ADDRESS object.
If no address is defined, returns 0.  (This is for compatibility with
the URL library.)"
  (let ((port (url-portspec address))) ; (url-port) is too slow!
    (if port port 0)))

(defun elpher-gopher-address-selector (address)
  "Retrieve gopher selector from ADDRESS object."
  (if (member (url-filename address) '("" "/"))
      ""
    (url-unhex-string (substring (url-filename address) 2))))


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


;; Page

(defun elpher-make-page (display-string address)
  "Create a page with DISPLAY-STRING and ADDRESS."
  (list display-string address))

(defun elpher-make-start-page ()
  "Create the start page."
  (elpher-make-page "Start Page"
                    (elpher-address-from-url elpher-start-page-url)))

(defun elpher-page-display-string (page)
  "Retrieve the display string corresponding to PAGE."
  (elt page 0))

(defun elpher-page-address (page)
  "Retrieve the address corresponding to PAGE."
  (elt page 1))

(defun elpher-page-set-address (page new-address)
  "Set the address corresponding to PAGE to NEW-ADDRESS."
  (setcar (cdr page) new-address))

(defun elpher-page-from-url (url &optional default-scheme)
  "Create a page with address and display string defined by URL.
The URL is unhexed prior to its use as a display string to improve
readability.

If DEFAULT-SCHEME is non-nil, this scheme is applied to the URL
in the instance that URL itself doesn't specify one."
  (let ((address (elpher-address-from-url url default-scheme)))
    (elpher-make-page (elpher-address-to-iri address) address)))

(defun elpher-address-to-iri (address)
  "Return an IRI for ADDRESS.
Decode percent-escapes and handle punycode in the domain name.
Drop the password, if any."
  (let ((data (match-data)) ; Prevent parsing clobbering match data
        (host (url-host address))
        (pass (url-password address)))
    (unwind-protect
        (let* ((host (url-host address))
               (pass (url-password address)))
          (when host
            (setf (url-host address) (puny-decode-domain host)))
          (when pass				 ; RFC 3986 says we should not render
            (setf (url-password address) nil)) ; the password as clear text
          (elpher-decode (url-unhex-string (url-recreate-url address))))
      (setf (url-host address) host)
      (setf (url-password address) pass)
      (set-match-data data))))

(defvar elpher-current-page nil
  "The current page for this Elpher buffer.")

(defvar elpher-history nil
  "The local history stack for this Elpher buffer.
This variable is used by `elpher-back' and
`elpher-show-history'.")

(defvar elpher-visited-pages nil
  "The global history for all Elpher buffers.
This variable is used by `elpher-show-visited-pages'.")

(defun elpher-visit-page (page &optional renderer no-history)
  "Visit PAGE using its own renderer or RENDERER, if non-nil.
Additionally, push PAGE onto the history stack and the list of
previously-visited pages,unless NO-HISTORY is non-nil."
  (elpher-save-pos)
  (elpher-process-cleanup)
  (unless no-history
    (unless (or (not elpher-current-page)
                 (equal (elpher-page-address elpher-current-page)
                        (elpher-page-address page)))
      (push elpher-current-page elpher-history)
      (unless (or (elpher-address-about-p (elpher-page-address page))
                  (and elpher-visited-pages
                       (equal page (car elpher-visited-pages))))
        (push page elpher-visited-pages))))
  (setq-local elpher-current-page page)
  (let* ((address (elpher-page-address page))
         (type (elpher-address-type address))
         (type-record (cdr (assoc type elpher-type-map))))
    (if type-record
        (funcall (car type-record)
                 (if renderer
                     renderer
                   (cadr type-record)))
      (elpher-visit-previous-page)
      (pcase type
        (`(gopher ,type-char)
         (error "Unsupported gopher selector type '%c' for '%s'"
                type-char (elpher-address-to-url address)))
        (other
         (error "Unsupported address type '%S' for '%s'"
                other (elpher-address-to-url address)))))))

(defun elpher-visit-previous-page ()
  "Visit the previous page in the history."
  (if elpher-history
      (elpher-visit-page (pop elpher-history) nil t)
    (error "No previous page")))

(defun elpher-reload-current-page ()
  "Reload the current page, discarding any existing cached content."
  (elpher-cache-content (elpher-page-address elpher-current-page) nil)
  (elpher-visit-page elpher-current-page))

(defun elpher-save-pos ()
  "Save the current position of point to the current page."
  (when elpher-current-page
    (elpher-cache-pos (elpher-page-address elpher-current-page) (point))))

(defun elpher-restore-pos ()
  "Restore the position of point to that cached in the current page."
  (let ((pos (elpher-get-cached-pos (elpher-page-address elpher-current-page))))
    (if pos
        (goto-char pos)
      (goto-char (point-min)))))

(defun elpher-get-default-url-scheme ()
  "Suggest default URL scheme for visiting addresses based on the current page."
  (if elpher-current-page
      (let* ((address (elpher-page-address elpher-current-page))
             (current-type (elpher-address-type address)))
        (pcase current-type
          ((or (and 'file (guard (not elpher-history)))
               `(about ,_))
           elpher-default-url-type)
          (_
           (url-type address))))
      elpher-default-url-type))


;;; Buffer preparation
;;

(defvar elpher-buffer-name "*elpher*"
  "The default name of the Elpher buffer.")

(defun elpher-update-header ()
  "If `elpher-use-header' is true, display current page info in window header."
  (if (and elpher-use-header elpher-current-page)
      (let* ((display-string (elpher-page-display-string elpher-current-page))
             (sanitized-display-string (replace-regexp-in-string "%" "%%" display-string))
             (address (elpher-page-address elpher-current-page))
             (tls-string (if (and (not (elpher-address-about-p address))
                                  (member (elpher-address-protocol address)
                                          '("gophers" "gemini")))
                             " [TLS encryption]"
                           ""))
             (header (concat sanitized-display-string
                             (propertize tls-string 'face 'bold))))
        (setq header-line-format header))))

(defmacro elpher-with-clean-buffer (&rest args)
  "Evaluate ARGS with a clean *elpher* buffer as current."
  (declare (debug (body))) ;; Allow edebug to step through body
  `(with-current-buffer elpher-buffer-name
     (unless (eq major-mode 'elpher-mode)
       ;; avoid resetting buffer-local variables
       (elpher-mode))
     (let ((inhibit-read-only t)
           (ansi-color-context nil)) ;; clean ansi interpreter state (also next 2 lines)
       (setq-local xterm-color--current-fg nil)
       (setq-local xterm-color--current-bg nil)
       (setq-local network-security-level
                   (default-value 'network-security-level))
       (erase-buffer)
       (elpher-update-header)
       ,@args)))

(defun elpher-buffer-message (string &optional line)
  "Replace first line in elpher buffer with STRING.
If LINE is non-nil, replace that line instead."
  (with-current-buffer elpher-buffer-name
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (if line
          (forward-line line))
      (let ((data (match-data)))
        (unwind-protect
            (progn
              (re-search-forward "^.*$")
              (replace-match string))
          (set-match-data data))))))


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
  (elpher-decode (replace-regexp-in-string "\n\\.\n$" "\n"
                                           (replace-regexp-in-string "\r" "" string))))

;;; Buttonify urls

(defconst elpher-url-regex
  "\\([a-zA-Z]+\\)://\\([a-zA-Z0-9.-]*[a-zA-Z0-9-]\\|\\[[a-zA-Z0-9:]+\\]\\)\\(:[0-9]+\\)?\\(/\\([0-9a-zA-Z_~?/@|:.%#=&-]*[0-9a-zA-Z_~?/@|#-]\\)?\\)?"
  "Regexp used to locate and buttonify URLs in text files loaded by elpher.")

(defun elpher-buttonify-urls (string)
  "Turn substrings which look like urls in STRING into clickable buttons."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward elpher-url-regex nil t)
      (let ((page (elpher-page-from-url (substring-no-properties (match-string 0)))))
        (make-text-button (match-beginning 0)
                          (match-end 0)
                          'elpher-page  page
                          'action #'elpher-click-link
                          'follow-link t
                          'help-echo #'elpher--page-button-help
                          'face 'button)))
    (buffer-string)))


;; ANSI colors or XTerm colors (application and filtering)

(or (require 'xterm-color nil t)
    (require 'ansi-color))

(defalias 'elpher-color-filter-apply
  (if (fboundp 'xterm-color-filter)
      (lambda (s)
        (let ((_xterm-color-render nil))
          (xterm-color-filter s)))
    #'ansi-color-filter-apply)
  "A function to filter out ANSI escape sequences.")

(defalias 'elpher-color-apply
  (if (fboundp 'xterm-color-filter)
      #'xterm-color-filter
    #'ansi-color-apply)
  "A function to apply ANSI escape sequences.")

(defun elpher-text-has-ansi-escapes-p (string)
  "Return non-nil if STRING includes an ANSI escape code."
  (save-match-data
    (string-match "\x1b\\[" string)))


;; Processing text for display

(defun elpher-process-text-for-display (string)
  "Perform any desired processing of STRING prior to display as text.
Currently includes buttonifying URLs and processing ANSI escape codes."
  (elpher-buttonify-urls (if (elpher-text-has-ansi-escapes-p string)
                             (if elpher-filter-ansi-from-text
                                 (elpher-color-filter-apply string)
                               (elpher-color-apply string))
                           string)))


;;; General network communication
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


(defvar elpher-network-timer nil
  "Timer used for network connections.")

(defvar elpher-use-tls nil
  "If non-nil, use TLS to communicate with gopher servers.")

(defvar elpher-client-certificate nil
  "If non-nil, contains client certificate details to use for TLS connections.")

(defun elpher-process-cleanup ()
  "Immediately shut down any extant elpher process and timers."
  (let ((p (get-process "elpher-process")))
    (if p (delete-process p)))
  (if (timerp elpher-network-timer)
      (cancel-timer elpher-network-timer)))

(defun elpher-make-network-timer (thunk)
  "Create a timer to run the THUNK after `elpher-connection-timeout' seconds.
This is just a wraper around `run-at-time' which additionally sets the
buffer-local variable `elpher-network-timer' to allow
`elpher-process-cleanup' to also clear the timer."
  (let ((timer (run-at-time elpher-connection-timeout nil thunk)))
    (setq-local elpher-network-timer timer)
    timer))

(defun elpher-get-host-response (address default-port query-string response-processor
                                         &optional use-tls force-ipv4)
  "Generic function for retrieving data from ADDRESS.

When ADDRESS lacks a specific port, DEFAULT-PORT is used instead.
QUERY-STRING is a string sent to the host specified by ADDRESS to
illicet a response.  This response is passed as an argument to the
function RESPONSE-PROCESSOR.

If non-nil, USE-TLS specifies that the connection is to be made over
TLS.  If set to gemini, the certificate verification will be disabled
unless `elpher-gemini-TLS-cert-checks' is non-nil.

If non-nil, FORCE-IPV4 causes the network connection to be made over
ipv4 only.  (The default behaviour when this is not set depends on
the host operating system and the local network capabilities.)"
  (if (and use-tls (not (gnutls-available-p)))
      (error "Use of TLS requires Emacs to be compiled with GNU TLS support")
    (unless (< (elpher-address-port address) 65536)
      (error "Cannot establish network connection: port number > 65536"))
    (when (and (eq use-tls 'gemini) (not elpher-gemini-TLS-cert-checks))
      (setq-local network-security-level 'low)
      (setq-local gnutls-verify-error nil))
    (condition-case nil
        (let* ((kill-buffer-query-functions nil)
               (port (elpher-address-port address))
               (host (elpher-address-host address))
               (service (if (> port 0) port default-port))
               (response-string-parts nil)
               (bytes-received 0)
               (hkbytes-received 0)
               (socks (or elpher-socks-always (string-suffix-p ".onion" host)))
               (gnutls-params (list :type 'gnutls-x509pki
                                    :hostname host
                                    :keylist
                                    (elpher-get-current-keylist address)))
               (timer (elpher-make-network-timer
                                   (lambda ()
                                     (elpher-process-cleanup)
                                     (cond
                                        ; Try again with IPv4
                                      ((not (or elpher-ipv4-always force-ipv4 socks))
                                       (message "Connection timed out.  Retrying with IPv4.")
                                       (elpher-get-host-response address default-port
                                                                 query-string
                                                                 response-processor
                                                                 use-tls t))
                                      ((and use-tls
                                            (not (eq use-tls 'gemini))
                                            (or elpher-auto-disengage-TLS
                                                (y-or-n-p
                                                 "TLS connetion failed.  Disable TLS mode and retry? ")))
                                       (setq elpher-use-tls nil)
                                       (elpher-get-host-response address default-port
                                                                 query-string
                                                                 response-processor
                                                                 nil force-ipv4))
                                      (t
                                       (elpher-network-error address "Connection time-out."))))))
               (proc (if socks (socks-open-network-stream "elpher-process" nil host service)
                       (make-network-process :name "elpher-process"
                                             :host host
                                             :family (and (or force-ipv4
                                                              elpher-ipv4-always)
                                                          'ipv4)
                                             :service service
                                             :buffer nil
                                             :nowait t
                                             :tls-parameters
                                             (and use-tls
                                                  (cons 'gnutls-x509pki
                                                        (apply #'gnutls-boot-parameters
                                                               gnutls-params)))))))
          (setq elpher-network-timer timer)
          (set-process-coding-system proc 'binary 'binary)
          (set-process-query-on-exit-flag proc nil)
          (elpher-buffer-message (concat "Connecting to " host "..."
                                         " (press 'u' to abort)"))
          (set-process-filter proc
                              (lambda (_proc string)
                                (when timer
                                  (cancel-timer timer)
                                  (setq timer nil))
                                (setq bytes-received (+ bytes-received (length string)))
                                (let ((new-hkbytes-received (/ bytes-received 102400)))
                                  (when (> new-hkbytes-received hkbytes-received)
                                    (setq hkbytes-received new-hkbytes-received)
                                    (elpher-buffer-message
                                     (concat "("
                                             (number-to-string (/ hkbytes-received 10.0))
                                             " MB read)")
                                     1)))
                                (setq response-string-parts
                                      (cons string response-string-parts))))
          (set-process-sentinel proc
                                (lambda (proc event)
                                  (when timer
                                    (cancel-timer timer))
                                  (condition-case the-error
                                      (cond
                                       ((string-prefix-p "open" event)    ; request URL
                                        (elpher-buffer-message
                                         (concat "Connected to " host ". Receiving data..."
                                                 " (press 'u' to abort)"))
                                        (let ((inhibit-eol-conversion t))
                                          (process-send-string proc query-string)))
                                       ((string-prefix-p "deleted" event)) ; do nothing
                                       ((and (not response-string-parts)
                                             (not (or elpher-ipv4-always force-ipv4 socks)))
                                        ; Try again with IPv4
                                        (message "Connection failed. Retrying with IPv4.")
                                        (elpher-get-host-response address default-port
                                                                  query-string
                                                                  response-processor
                                                                  use-tls t))
                                       (response-string-parts
                                        (elpher-with-clean-buffer
                                         (insert "Data received.  Rendering..."))
                                        (funcall response-processor
                                                 (apply #'concat (reverse response-string-parts)))
                                        (elpher-restore-pos))
                                       (t
                                        (error "No response from server")))
                                    (error
                                     (elpher-network-error address the-error)))))
          (when socks
            (if use-tls (apply #'gnutls-negotiate :process proc gnutls-params))
            (funcall (process-sentinel proc) proc "open\n")))
      (error
       (elpher-process-cleanup)
       (error "Error initiating connection to server")))))


;;; Client-side TLS Certificate Management
;;

(defun elpher-generate-certificate (common-name key-file cert-file &optional temporary)
  "Generate a key and a self-signed client TLS certificate using openssl.

The Common Name field of the certificate is set to COMMON-NAME.  The
arguments KEY-FILE and CERT-FILE should contain the absolute paths of
the key and certificate files to write.

If TEMPORARY is non-nil, the certificate will be given an exporation
period of one day, and the key and certificate files will be deleted
when the certificate is no longer needed for the current session.

Otherwise, the certificate will be given a 100 year expiration period
and the files will not be deleted.

The function returns a list containing the current host name, the
temporary flag, and the key and cert file names in the form required
by `gnutls-boot-parameters`."
  (let ((exp-key-file (expand-file-name key-file))
        (exp-cert-file (expand-file-name cert-file)))
    (condition-case nil
        (progn
          (call-process elpher-openssl-command nil nil nil
                        "req" "-x509" "-newkey" "rsa:2048"
                        "-days" (if temporary "1" "36500")
                        "-nodes"
                        "-subj" (concat "/CN=" common-name)
                        "-keyout" exp-key-file
                        "-out" exp-cert-file)
          (list (elpher-address-host (elpher-page-address elpher-current-page))
                temporary exp-key-file exp-cert-file))
      (error
       (message "Check that openssl is installed, or customize `elpher-openssl-command`.")
       (error "Program 'openssl', required for certificate generation, not found")))))

(defun elpher-generate-throwaway-certificate ()
  "Generate and return details of a throwaway certificate.
The key and certificate files will be deleted when they are no
longer needed for this session."
  (let* ((file-base (make-temp-name "elpher"))
         (key-file (concat temporary-file-directory file-base ".key"))
         (cert-file (concat temporary-file-directory file-base ".crt")))
    (elpher-generate-certificate file-base key-file cert-file t)))

(defun elpher-generate-persistent-certificate (file-base common-name)
  "Generate and return details of a persistent certificate.
The argument FILE-BASE is used as the base for the key and certificate
files, while COMMON-NAME specifies the common name field of the
certificate.

The key and certificate files are written to in `elpher-certificate-directory'."
  (let* ((key-file (concat elpher-certificate-directory file-base ".key"))
         (cert-file (concat elpher-certificate-directory file-base ".crt")))
    (elpher-generate-certificate common-name key-file cert-file)))

(defun elpher-get-existing-certificate (file-base)
  "Return a certificate object corresponding to an existing certificate.
It is assumed that the key files FILE-BASE.key and FILE-BASE.crt exist in
the directory `elpher-certificate-directory'."
  (let* ((key-file (concat elpher-certificate-directory file-base ".key"))
         (cert-file (concat elpher-certificate-directory file-base ".crt")))
    (list (elpher-address-host (elpher-page-address elpher-current-page))
          nil
          (expand-file-name key-file)
          (expand-file-name cert-file))))

(defun elpher-install-and-use-existing-certificate (key-file-src cert-file-src file-base)
  "Install a key+certificate file pair in `elpher-certificate-directory'.
The strings KEY-FILE-SRC and CERT-FILE-SRC are the existing key and
certificate files to install.  The argument FILE-BASE is used as the
base for the installed key and certificate files."
  (let* ((key-file (concat elpher-certificate-directory file-base ".key"))
         (cert-file (concat elpher-certificate-directory file-base ".crt")))
    (if (or (file-exists-p key-file)
            (file-exists-p cert-file))
        (error "A certificate with base name %s is already installed" file-base))
    (copy-file key-file-src key-file)
    (copy-file cert-file-src cert-file)
    (list (elpher-address-host (elpher-page-address elpher-current-page))
          nil
          (expand-file-name key-file)
          (expand-file-name cert-file))))

(defun elpher-list-existing-certificates ()
  "Return a list of the persistent certificates in `elpher-certificate-directory'."
  (unless (file-directory-p elpher-certificate-directory)
    (make-directory elpher-certificate-directory))
  (mapcar
   (lambda (file)
     (file-name-sans-extension file))
   (directory-files elpher-certificate-directory nil "\\.key$")))

(defun elpher-forget-current-certificate ()
  "Causes any current certificate to be forgotten.)
In the case of throwaway certificates, the key and certificate files
are also deleted."
  (interactive)
  (when elpher-client-certificate
    (unless (and (called-interactively-p 'any)
                 (not (y-or-n-p (concat "Really forget client certificate? "
                                        "(Throwaway certificates will be deleted.)"))))
      (when (cadr elpher-client-certificate)
        (delete-file (elt elpher-client-certificate 2))
        (delete-file (elt elpher-client-certificate 3)))
      (setq elpher-client-certificate nil)
      (if (called-interactively-p 'any)
          (message "Client certificate forgotten.")))))

(defun elpher-get-current-keylist (address)
  "Retrieve the `gnutls-boot-parameters'-compatable keylist.

This is obtained from the client certificate described by
`elpher-current-certificate', if one is available and the host for
that certificate matches the host in ADDRESS.

If `elpher-current-certificate' is non-nil, and its host name doesn't
match that of ADDRESS, the certificate is forgotten."
  (if elpher-client-certificate
      (if (string= (car elpher-client-certificate)
                   (elpher-address-host address))
          (list (cddr elpher-client-certificate))
        (elpher-forget-current-certificate)
        (message "Disabling client certificate for new host")
        nil)
    nil))


;;; Gopher selector retrieval
;;

(defun elpher-get-gopher-response (address renderer)
  "Get response string from gopher server at ADDRESS and render using RENDERER."
  (elpher-get-host-response address 70
                            (concat (elpher-gopher-address-selector address) "\r\n")
                            renderer
                            (or (string= (elpher-address-protocol address) "gophers")
                                elpher-use-tls)))

(defun elpher-get-gopher-page (renderer)
  "Getter function for gopher pages.
The RENDERER procedure is used to display the contents of the page
once they are retrieved from the gopher server."
  (let* ((address (elpher-page-address elpher-current-page))
         (content (elpher-get-cached-content address)))
    (if (and content (funcall renderer nil))
        (elpher-with-clean-buffer
         (insert content)
         (elpher-restore-pos))
      (elpher-with-clean-buffer
       (insert "LOADING... (use 'u' to cancel)\n"))
      (condition-case the-error
          (elpher-get-gopher-response address renderer)
        (error
         (elpher-network-error address the-error))))))


;;; Gopher index rendering
;;

(defun elpher-insert-margin (&optional type-name)
  "Insert index margin, optionally containing the TYPE-NAME, into current buffer."
  (if type-name
      (progn
        (insert (format (concat "%" (number-to-string (- elpher-margin-width 1)) "s")
                        (concat
                         (propertize "[" 'face 'elpher-margin-brackets)
                         (propertize type-name 'face 'elpher-margin-key)
                         (propertize "]" 'face 'elpher-margin-brackets))))
        (insert " "))
    (insert (make-string elpher-margin-width ?\s))))

(defun elpher--page-button-help (_window buffer pos)
  "Function called by Emacs to generate mouse-over text.
The arguments specify the BUFFER and the POS within the buffer of the item
for which help is required.  The function returns the help to be
displayed.  The _WINDOW argument is currently unused."
  (with-current-buffer buffer
    (let ((button (button-at pos)))
      (when button
        (let* ((page (button-get button 'elpher-page))
               (address (elpher-page-address page)))
          (format "mouse-1, RET: open '%s'" (elpher-address-to-url address)))))))

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
               (filtered-display-string (elpher-color-filter-apply display-string))
               (page (elpher-make-page filtered-display-string address)))
          (elpher-insert-margin margin-code)
          (insert-text-button filtered-display-string
                              'face face
                              'elpher-page page
                              'action #'elpher-click-link
                              'follow-link t
                              'help-echo #'elpher--page-button-help))
      (pcase type
        ('nil ;; Information
         (elpher-insert-margin)
         (let ((propertized-display-string
                (propertize display-string 'face 'elpher-info)))
           (insert (elpher-process-text-for-display propertized-display-string))))
        (`(gopher ,selector-type) ;; Unknown
         (elpher-insert-margin (concat (char-to-string selector-type) "?"))
         (insert (propertize display-string
                             'face 'elpher-unknown)))))
    (insert "\n")))

(defun elpher-click-link (button)
  "Function called when the gopher link BUTTON is activated."
  (let ((page (button-get button 'elpher-page)))
    (elpher-visit-page page)))

(defun elpher-render-index (data &optional _mime-type-string)
  "Render DATA as an index.  MIME-TYPE-STRING is unused."
  (elpher-with-clean-buffer
   (if (not data)
       t
     (let ((data-processed (elpher-preprocess-text-response data)))
       (dolist (line (split-string data-processed "\n"))
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
               (elpher-insert-index-record display-string address))))))
     (elpher-cache-content (elpher-page-address elpher-current-page)
                           (buffer-string)))))


;;; Gopher text rendering
;;

(defun elpher-render-text (data &optional _mime-type-string)
  "Render DATA as text.  MIME-TYPE-STRING is unused."
  (elpher-with-clean-buffer
   (if (not data)
       t
     (insert (elpher-process-text-for-display (elpher-preprocess-text-response data)))
     (elpher-cache-content
      (elpher-page-address elpher-current-page)
      (buffer-string)))))


;;; Image retrieval
;;

(defun elpher-render-image (data &optional _mime-type-string)
  "Display DATA as image.  MIME-TYPE-STRING is unused."
  (if (not data)
      nil
    (if (display-images-p)
        (let* ((image (create-image
                       data
                       nil t)))
          (if (not image)
              (error "Unsupported image format")
            (let ((window (get-buffer-window elpher-buffer-name)))
              (when window
                (setf (image-property image :max-width) (window-body-width window t))
                (setf (image-property image :max-height) (window-body-height window t))))
            (elpher-with-clean-buffer
             (insert-image image)
             (elpher-restore-pos))))
      (elpher-render-download data))))


;;; Gopher search retrieval and rendering
;;

(defun elpher-get-gopher-query-page (renderer)
  "Getter for gopher addresses requiring input.
The response is rendered using the rendering function RENDERER."
  (let* ((address (elpher-page-address elpher-current-page))
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
            (elpher-get-gopher-response search-address renderer))
        (if aborted
            (elpher-visit-previous-page))))))


;;; Raw server response rendering
;;

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


;;; File save "rendering"
;;

(defun elpher-render-download (data &optional _mime-type-string)
  "Save DATA to file.  MIME-TYPE-STRING is unused."
  (if (not data)
      nil
    (let* ((address (elpher-page-address elpher-current-page))
           (selector (if (elpher-address-gopher-p address)
                         (elpher-gopher-address-selector address)
                       (elpher-address-filename address))))
      (elpher-visit-previous-page) ; Do first in case of non-local exits.
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


;;; HTML rendering
;;

(defun elpher-render-html (data &optional _mime-type-string)
  "Render DATA as HTML using shr.  MIME-TYPE-STRING is unused."
  (elpher-with-clean-buffer
   (if (not data)
       t
     (let ((dom (with-temp-buffer
                  (insert data)
                  (libxml-parse-html-region (point-min) (point-max)))))
       (shr-insert-document dom)))))


;;; Gemini page retrieval
;;

(defvar elpher-gemini-redirect-chain)

(defun elpher-get-gemini-response (address renderer)
  "Get response string from gemini server at ADDRESS and render using RENDERER."
  (elpher-get-host-response address 1965
                            (concat (elpher-address-to-url address) "\r\n")
                            (lambda (response-string)
                              (elpher-process-gemini-response response-string renderer))
                            'gemini))

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
      (error "Malformed response: No CRLF-delimited header found in response %s" response))))

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
         (let* ((query-string
                 (if (eq (elt response-code 1) ?1)
                     (read-passwd (concat response-meta ": "))
                   (read-string (concat response-meta ": "))))
                (query-address (seq-copy (elpher-page-address elpher-current-page)))
                (old-fname (url-filename query-address)))
           (setf (url-filename query-address)
                 (concat old-fname "?" (url-build-query-string `((,query-string)))))
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
           (if (not (eq (elpher-address-type redirect-address) 'gemini))
               (error "Server tried to automatically redirect to non-gemini URL: %s"
                      response-meta))
           (elpher-page-set-address elpher-current-page redirect-address)
           (add-to-list 'elpher-gemini-redirect-chain redirect-address)
           (elpher-get-gemini-response redirect-address renderer)))
        (?4 ; Temporary failure
         (error "Gemini server reports TEMPORARY FAILURE for this request: %s %s"
                response-code response-meta))
        (?5 ; Permanent failure
         (error "Gemini server reports PERMANENT FAILURE for this request: %s %s"
                response-code response-meta))
        (?6 ; Client certificate required
         (elpher-with-clean-buffer
          (if elpher-client-certificate
              (insert "Gemini server does not recognise the provided TLS certificate:\n\n")
            (insert "Gemini server is requesting a valid TLS certificate:\n\n"))
          (auto-fill-mode 1)
          (elpher-gemini-insert-text response-meta))
         (let ((chosen-certificate (elpher-choose-client-certificate)))
           (unless chosen-certificate
             (error "Gemini server requires a client certificate and none was provided"))
           (setq elpher-client-certificate chosen-certificate))
         (elpher-with-clean-buffer)
         (elpher-get-gemini-response (elpher-page-address elpher-current-page) renderer))
        (_other
         (error "Gemini server response unknown: %s %s"
                response-code response-meta))))))

(defun elpher--read-answer-polyfill (question answers)
  "Polyfill for `read-answer' in Emacs 26.1.
QUESTION is a string containing a question, and ANSWERS
is a list of possible answers."
    (completing-read question (mapcar 'identity answers)))

(if (fboundp 'read-answer)
    (defalias 'elpher-read-answer 'read-answer)
  (defalias 'elpher-read-answer 'elpher--read-answer-polyfill))

(defun elpher-choose-client-certificate ()
  "Prompt for a client certificate to use to establish a TLS connection."
  (let* ((read-answer-short t))
    (pcase (read-answer "What do you want to do? "
                        '(("throwaway" ?t
                           "generate and use throw-away certificate")
                          ("persistent" ?p
                           "generate new or use existing persistent certificate")
                          ("abort" ?a
                           "stop immediately")))
      ("throwaway"
       (setq elpher-client-certificate (elpher-generate-throwaway-certificate)))
      ("persistent"
       (let* ((existing-certificates (elpher-list-existing-certificates))
              (file-base (completing-read
                          "Nickname for new or existing certificate (autocompletes, empty response aborts): "
                          existing-certificates)))
         (if (string-empty-p (string-trim file-base))
             nil
           (if (member file-base existing-certificates)
               (setq elpher-client-certificate
                     (elpher-get-existing-certificate file-base))
             (pcase (read-answer "Generate new certificate or install externally-generated one? "
                                 '(("new" ?n
                                    "generate new certificate")
                                   ("install" ?i
                                    "install existing certificate")
                                   ("abort" ?a
                                    "stop immediately")))
               ("new"
                (let ((common-name (read-string "Common Name field for new certificate: "
                                                file-base)))
                  (message "New key and self-signed certificate written to %s"
                           elpher-certificate-directory)
                  (elpher-generate-persistent-certificate file-base common-name)))
               ("install"
                (let* ((cert-file (read-file-name "Certificate file: " nil nil t))
                       (key-file (read-file-name "Key file: " nil nil t)))
                  (message "Key and certificate installed in %s for future use"
                           elpher-certificate-directory)
                  (elpher-install-and-use-existing-certificate key-file
                                                               cert-file
                                                               file-base)))
               ("abort" nil))))))
      ("abort" nil))))

(defun elpher-get-gemini-page (renderer)
  "Getter which retrieves and renders a Gemini page and renders it using RENDERER."
  (let* ((address (elpher-page-address elpher-current-page))
         (content (elpher-get-cached-content address)))
    (condition-case the-error
        (if (and content (funcall renderer nil))
            (elpher-with-clean-buffer
             (insert content)
             (elpher-restore-pos))
          (elpher-with-clean-buffer
           (insert "LOADING GEMINI... (use 'u' to cancel)\n"))
          (setq elpher-gemini-redirect-chain nil)
          (elpher-get-gemini-response address renderer))
      (error
       (elpher-network-error address the-error)))))

;;; Gemini page rendering
;;

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
         (elpher-render-download body))))))

(defun elpher-gemini-get-link-url (link-line)
  "Extract the url portion of LINK-LINE, a gemini map file link line.
Returns nil in the event that the contents of the line following the
=> prefix are empty."
  (let ((l (split-string (substring link-line 2))))
    (if l
        (string-trim (elt l 0))
      nil)))

(defun elpher-gemini-get-link-display-string (link-line)
  "Extract the display string portion of LINK-LINE, a gemini map file link line.
Return nil if this portion is not provided."
  (let* ((rest (string-trim (elt (split-string link-line "=>") 1)))
         (idx (string-match "[ \t]" rest)))
    (and idx
         (elpher-color-filter-apply (string-trim (substring rest (+ idx 1)))))))

(defun elpher-collapse-dot-sequences (filename)
  "Collapse dot sequences in the (absolute) FILENAME.
For instance, the filename \"/a/b/../c/./d\" will reduce to \"/a/c/d\""
  (let* ((path (split-string filename "/" t))
         (is-directory (string-match-p (rx (: (or "." ".." "/") line-end)) filename))
         (path-reversed-normalized
          (seq-reduce (lambda (a b)
                        (cond ((equal b "..") (cdr a))
                              ((equal b ".") a)
                              (t (cons b a))))
                      path nil))
         (path-normalized (reverse path-reversed-normalized)))
    (if path-normalized
        (concat "/" (string-join path-normalized "/") (and is-directory "/"))
      "/")))

(defun elpher-address-from-gemini-url (url)
  "Extract address from URL with defaults as per gemini map files.
While there's obviously some redundancy here between this function and
`elpher-address-from-url', gemini map file URLs require enough special
treatment that a separate function is warranted."
  (let ((address (url-generic-parse-url url))
        (current-address (elpher-page-address elpher-current-page)))
    (unless (and (url-type address) (not (url-fullness address))) ;avoid mangling mailto: urls
      (if (url-host address) ;if there is an explicit host, filenames are absolute
          (if (string-empty-p (url-filename address))
              (setf (url-filename address) "/")) ;ensure empty filename is marked as absolute
        (setf (url-host address) (url-host current-address))
        (setf (url-fullness address) (url-host address)) ; set fullness to t if host is set
        (setf (url-portspec address) (url-portspec current-address)) ; (url-port) too slow!
        (unless (string-prefix-p "/" (url-filename address)) ;deal with relative links
          (setf (url-filename address)
                (concat (file-name-directory (url-filename current-address))
                        (url-filename address)))))
      (when (url-host address)
        (setf (url-host address) (puny-encode-domain (url-host address))))
      (unless (url-type address)
        (setf (url-type address) (url-type current-address)))
      (when (equal (url-type address) "gemini")
        (setf (url-filename address)
              (elpher-collapse-dot-sequences (url-filename address)))))
    (elpher-remove-redundant-ports address)))

(defun elpher-gemini-insert-link (link-line)
  "Insert link described by LINK-LINE into a text/gemini document."
  (let ((url (elpher-gemini-get-link-url link-line)))
    (when url
      (let* ((given-display-string (elpher-gemini-get-link-display-string link-line))
             (address (elpher-address-from-gemini-url url))
             (type (if address (elpher-address-type address) nil))
             (type-map-entry (cdr (assoc type elpher-type-map)))
             (fill-prefix (make-string (+ 1 (length elpher-gemini-link-string)) ?\s)))
        (when type-map-entry
          (insert elpher-gemini-link-string)
          (let* ((face (elt type-map-entry 3))
                 (display-string (or given-display-string
                                     (elpher-address-to-iri address)))
                 (page (elpher-make-page display-string
                                         address)))
            (insert-text-button display-string
                                'face face
                                'elpher-page page
                                'action #'elpher-click-link
                                'follow-link t
                                'help-echo #'elpher--page-button-help))
          (newline))))))

(defun elpher-gemini-insert-header (header-line)
  "Insert header described by HEADER-LINE into a text/gemini document.
The gemini map file line describing the header is given
by HEADER-LINE."
  (when (string-match "^\\(#+\\)[ \t]*" header-line)
    (let* ((level (length (match-string 1 header-line)))
           (header (substring header-line (match-end 0)))
           (face (pcase level
                   (1 'elpher-gemini-heading1)
                   (2 'elpher-gemini-heading2)
                   (3 'elpher-gemini-heading3)
                   (_ 'default)))
           (fill-column (if (display-graphic-p)
                            (/ (* fill-column
                                  (font-get (font-spec :name (face-font 'default)) :size))
                               (font-get (font-spec :name (face-font face)) :size)) fill-column)))
      (unless (display-graphic-p)
        (insert (make-string level ?#) " "))
      (insert (propertize header
                          'face face
                          'gemini-heading t
                          'rear-nonsticky t))
      (newline))))

(defun elpher-gemini-insert-text (text-line)
  "Insert a plain non-preformatted TEXT-LINE into a text/gemini document.
This function uses Emacs' auto-fill to wrap text sensibly to a maximum
width defined by `elpher-gemini-max-fill-width'."
  (string-match
   (rx (: line-start
          (optional
           (group (or (: "*" (+ (any " \t")))
                      (: ">" (* (any " \t"))))))))
   text-line)
  (let* ((line-prefix (match-string 1 text-line))
         (processed-text-line
          (if line-prefix
              (cond ((string-prefix-p "*" line-prefix)
                     (concat
                      (replace-regexp-in-string "\\*"
                                                elpher-gemini-bullet-string
                                                (match-string 0 text-line))
                      (substring text-line (match-end 0))))
                    ((string-prefix-p ">" line-prefix)
                     (propertize text-line 'face 'elpher-gemini-quoted))
                    (t text-line))
            text-line))
         (fill-prefix (if line-prefix
                          (make-string (length (match-string 0 text-line)) ?\s)
                        "")))
    (insert (elpher-process-text-for-display processed-text-line))
    (newline)))

(defun elpher-gemini-pref-expand-collapse (button)
  "Function called when the preformatted text toggle BUTTON is activated."
  (let ((id (button-get button 'pref-id)))
    (if (invisible-p id)
        (remove-from-invisibility-spec id)
      (add-to-invisibility-spec id))
    (redraw-display)))

(defun elpher-gemini-insert-preformatted-toggler (alt-text)
  "Insert a button for toggling the visibility of preformatted text.
If non-nil, ALT-TEXT is displayed alongside the button."
  (let* ((url-string (url-recreate-url (elpher-page-address elpher-current-page)))
         (pref-id (intern (concat "pref-"
                                  (number-to-string (point))
                                  "-"
                                  url-string))))
    (insert elpher-gemini-preformatted-toggle-bullet)
    (when alt-text
      (insert (propertize (concat alt-text " ")
                          'face 'elpher-gemin-preformatted)))
    (insert-text-button elpher-gemini-preformatted-toggle-label
                        'action #'elpher-gemini-pref-expand-collapse
                        'pref-id pref-id
                        'face 'elpher-gemini-preformatted-toggle)
    (add-to-invisibility-spec pref-id)
    (newline)
    pref-id))

(defun elpher-gemini-insert-preformatted-line (line &optional pref-id)
  "Insert a LINE of preformatted text.
PREF-ID is the value assigned to the \"invisible\" text attribute, which
can be used to toggle the display of the preformatted text."
  (insert (propertize (concat (elpher-process-text-for-display
                               (propertize line 'face 'elpher-gemini-preformatted))
                              "\n")
                      'invisible pref-id
                      'rear-nonsticky t)))

(defun elpher-render-gemini-map (data _parameters)
  "Render DATA as a gemini map file, PARAMETERS is currently unused."
  (elpher-with-clean-buffer
   (auto-fill-mode 1)
   (setq-local buffer-invisibility-spec nil)
   (let ((preformatted nil)
         (adaptive-fill-mode nil)) ;Prevent automatic setting of fill-prefix
     (setq-local fill-column (min (window-width) elpher-gemini-max-fill-width))
     (dolist (line (split-string data "\n"))
       (pcase line
         ((rx (: "```" (opt (let alt-text (+ any)))))
          (setq preformatted
                (if preformatted
                    nil
                  (if elpher-gemini-hide-preformatted
                      (elpher-gemini-insert-preformatted-toggler alt-text)
                    t))))
         ((guard  preformatted)
          (elpher-gemini-insert-preformatted-line line preformatted))
         ((pred (string-prefix-p "=>"))
          (elpher-gemini-insert-link line))
         ((pred (string-prefix-p "#"))
          (elpher-gemini-insert-header line))
         (_ (elpher-gemini-insert-text line))))
   (elpher-cache-content
    (elpher-page-address elpher-current-page)
    (buffer-string)))))

(defun elpher-render-gemini-plain-text (data _parameters)
  "Render DATA as plain text file.  PARAMETERS is currently unused."
  (elpher-with-clean-buffer
   (insert (elpher-process-text-for-display data))
   (elpher-cache-content
    (elpher-page-address elpher-current-page)
    (buffer-string))))

(defun elpher-build-current-imenu-index ()
  "Build imenu index for current elpher buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((match nil)
          (headers nil))
      (while (setq match (text-property-search-forward 'gemini-heading t t))
        (push (cons
               (buffer-substring-no-properties (prop-match-beginning match)
                                               (prop-match-end match))
               (prop-match-beginning match))
              headers))
      (reverse headers))))


;;; Finger page connection
;;

(defun elpher-get-finger-page (renderer)
  "Opens a finger connection to the current page address.
The result is rendered using RENDERER."
  (let* ((address (elpher-page-address elpher-current-page))
         (content (elpher-get-cached-content address)))
    (if (and content (funcall renderer nil))
        (elpher-with-clean-buffer
         (insert content)
         (elpher-restore-pos))
      (elpher-with-clean-buffer
       (insert "LOADING... (use 'u' to cancel)\n"))
      (condition-case the-error
          (let* ((kill-buffer-query-functions nil)
                 (user (let ((filename (elpher-address-filename address)))
                         (if (> (length filename) 1)
                             (substring filename 1)
                           (elpher-address-user address)))))
            (elpher-get-host-response address 79
                                      (concat user "\r\n")
                                      renderer))
        (error
         (elpher-network-error address the-error))))))


;;; Telnet page connection
;;

(defun elpher-get-telnet-page (renderer)
  "Opens a telnet connection to the current page address (RENDERER must be nil)."
  (when renderer
    (elpher-visit-previous-page)
    (error "Command not supported for telnet URLs"))
  (let* ((address (elpher-page-address elpher-current-page))
         (host (elpher-address-host address))
         (port (elpher-address-port address)))
    (elpher-visit-previous-page)
    (if (> port 0)
        (telnet host port)
      (telnet host))))


;;; Other URL page opening
;;

(defun elpher-get-other-url-page (renderer)
  "Getter which attempts to open the URL specified by the current page.
The RENDERER argument to this getter must be nil."
  (when renderer
    (elpher-visit-previous-page)
    (error "Command not supported for general URLs"))
  (let* ((address (elpher-page-address elpher-current-page))
         (url (elpher-address-to-url address)))
    (elpher-visit-previous-page) ; Do first in case of non-local exits.
    (message "Opening URL...")
    (if elpher-open-urls-with-eww
        (browse-web url)
      (browse-url url))))


;;; File page
;;

(defun elpher-get-file-page (renderer)
  "Getter which renders a local file using RENDERER.
Assumes UTF-8 encoding for all text files."
  (let* ((address (elpher-page-address elpher-current-page))
         (filename (elpher-address-filename address)))
    (unless (file-exists-p filename)
      (elpher-visit-previous-page)
        (error "File not found"))
    (unless (file-readable-p filename)
      (elpher-visit-previous-page)
        (error "Could not read from file"))
    (let ((body (with-temp-buffer
       (let ((coding-system-for-read 'binary)
             (coding-system-for-write 'binary))
         (insert-file-contents-literally filename)
         (encode-coding-string (buffer-string) 'raw-text)))))
       (if renderer
           (funcall renderer body nil)
         (pcase (file-name-extension filename)
           ((or  "gmi" "gemini")
            (elpher-render-gemini-map (decode-coding-string body 'utf-8) nil))
           ((or "htm" "html")
            (elpher-render-html (decode-coding-string body 'utf-8)))
           ((or "txt" "")
            (elpher-render-text (decode-coding-string body 'utf-8)))
           ((or "jpg" "jpeg" "gif" "png" "bmp" "tif" "tiff")
            (elpher-render-image body))
           ((or "gopher" "gophermap")
            (elpher-render-index (elpher-decode body)))
           (_
            (elpher-render-download body))))
       (elpher-restore-pos))))


;;; Welcome page retrieval
;;

(defun elpher-get-welcome-page (renderer)
  "Getter which displays the welcome page (RENDERER must be nil)."
  (when renderer
    (elpher-visit-previous-page)
    (error "Command not supported for welcome page"))
  (elpher-with-clean-buffer
   (insert "     --------------------------------------------\n"
           "           Elpher Gopher and Gemini Client       \n"
           "                   version " elpher-version "\n"
           "     --------------------------------------------\n"
           "\n"
           "Default bindings:\n"
           "\n"
           " - TAB/Shift-TAB: next/prev item on current page\n"
           " - RET/mouse-1: open item under cursor\n"
           " - m: select an item on current page by name (autocompletes)\n"
           " - u/mouse-3/U: return to previous page or to the start page\n"
           " - g: go to a particular address (gopher, gemini, finger)\n"
           " - o/O: open a different address selector or the root menu of the current server\n"
           " - d/D: download item under cursor or current page\n"
           " - i/I: info on item under cursor or current page\n"
           " - c/C: copy URL representation of item under cursor or current page\n"
           " - a/A: bookmark the item under cursor or current page\n"
           " - B: list all bookmarks\n"
           " - s/S: show current history stack or all previously visted pages\n"
           " - r: redraw current page (using cached contents if available)\n"
           " - R: reload current page (regenerates cache)\n"
           " - !: set character coding system for gopher (default is to autodetect)\n"
           " - T: toggle TLS gopher mode\n"
           " - F: forget/discard current TLS client certificate\n"
           " - .: display the raw server response for the current page\n"
           "\n"
           "Start your exploration of gopher space and gemini:\n")
   (elpher-insert-index-record "Floodgap Systems Gopher Server"
                               (elpher-make-gopher-address ?1 "" "gopher.floodgap.com" 70))
   (elpher-insert-index-record "Project Gemini home page"
                               (elpher-address-from-url "gemini://gemini.circumlunar.space/"))
   (insert "\n"
           "Alternatively, select a search engine and enter some search terms:\n")
   (elpher-insert-index-record "Gopher Search Engine (Veronica-2)"
                               (elpher-make-gopher-address ?7 "/v2/vs" "gopher.floodgap.com" 70))
   (elpher-insert-index-record "Gemini Search Engine (geminispace.info)"
                               (elpher-address-from-url "gemini://geminispace.info/search"))
   (insert "\n"
           "Your bookmarks are stored in your ")
   (insert-text-button "bookmark list"
                       'face 'link
                       'action #'elpher-click-link
                       'follow-link t
                       'help-echo #'elpher--page-button-help
                       'elpher-page
                       (elpher-make-page "Elpher Bookmarks"
                                         (elpher-make-about-address 'bookmarks)))
   (insert ".\n")
   (insert (propertize
            "(Bookmarks from legacy elpher-bookmarks files will be automatically imported.)\n"
            'face 'shadow))
   (insert "\n"
           "The gopher home of the Elpher project is here:\n")
   (elpher-insert-index-record "The Elpher Project Page"
                               (elpher-make-gopher-address ?1
                                                           "/projects/elpher/"
                                                           "thelambdalab.xyz"
                                                           70))
   (let ((help-string "RET,mouse-1: Open Elpher info manual (if available)"))
     (insert "\n"
             "The following info documentation is available:\n"
             "   - ")
     (insert-text-button "Elpher Manual"
                         'face 'link
                         'action (lambda (_)
                                   (interactive)
                                   (info "(elpher)"))
                         'follow-link t
                         'help-echo help-string)
     (insert "\n   - ")
     (insert-text-button "Changes introduced by the latest release"
                       'face 'link
                       'action (lambda (_)
                                 (interactive)
                                 (info "(elpher)News"))
                       'follow-link t
                       'help-echo help-string))
   (insert "\n")
   (insert (propertize
            (concat "(These documents should be available if you have installed Elpher\n"
                    " from MELPA or non-GNU ELPA. Otherwise you may have to install the\n"
                    " manual yourself.)\n")
            'face 'shadow))
   (elpher-restore-pos)))


;;; History page retrieval
;;

(defun elpher-show-history ()
  "Show the current contents of elpher's history stack.
Use \\[elpher-show-visited-pages] to see the entire history.
This is rendered using `elpher-get-history-page' via `elpher-type-map'."
  (interactive)
  (elpher-visit-page
   (elpher-make-page "Current History Stack"
		     (elpher-make-about-address 'history))))

(defun elpher-show-visited-pages ()
  "Show the all the pages you've visited using Elpher.
Use \\[elpher-show-history] to see just the current history stack.
This is rendered using `elpher-get-visited-pages-page' via `elpher-type-map'."
  (interactive)
  (elpher-visit-page
   (elpher-make-page "Elpher Visted Pages"
		     (elpher-make-about-address 'visited-pages))))

(defun elpher-get-history-page (renderer)
  "Getter which displays the history page (RENDERER must be nil)."
  (when renderer
    (elpher-visit-previous-page)
    (error "Command not supported for history page"))
  (elpher-display-history-links elpher-history "Current history stack"))

(defun elpher-get-visited-pages-page (renderer)
  "Getter which displays the list of visited pages (RENDERER must be nil)."
  (when renderer
    (elpher-visit-previous-page)
    (error "Command not supported for history page"))
  (elpher-display-history-links
   (seq-filter (lambda (page)
                 (not (elpher-address-about-p (elpher-page-address page))))
               elpher-visited-pages)
   "All visited pages"))

(defun elpher-display-history-links (pages title)
  "Show all PAGES in an Elpher buffer with a given TITLE."
  (let* ((title-line (concat " ---- " title " ----"))
         (footer-line (make-string (length title-line) ?-)))
    (elpher-with-clean-buffer
     (insert title-line "\n\n")
     (if pages
         (dolist (page pages)
	   (when page
             (let ((display-string (elpher-page-display-string page))
		   (address (elpher-page-address page)))
               (elpher-insert-index-record display-string address))))
       (insert "No history items found.\n"))
     (insert "\n " footer-line "\n"
             "Select an entry or press 'u' to return to the previous page.")
     (elpher-restore-pos))))


;;; Bookmarks
;;

;; This code allows Elpher to use the standard Emacs bookmarks: `C-x r
;; m' to add a bookmark, `C-x r l' to list bookmarks (which is where
;; you can anotate bookmarks!), `C-x r b' to jump to a bookmark, and
;; so on. See the Bookmarks section in the Emacs info manual for more.

(defvar elpher-bookmark-link nil
  "Prefer bookmarking a link or the current page.
Bind this variable dynamically, or set it to t.
If you set it to t, the commands \\[bookmark-set-no-overwrite]
and \\[elpher-set-bookmark-no-overwrite] do the same thing.")

(defun elpher-bookmark-make-record ()
  "Return a bookmark record.
If `elpher-bookmark-link' is non-nil and point is on a link button,
return a bookmark record for that link.  Otherwise, return a bookmark
record for the current elpher page."
  (let* ((button (and elpher-bookmark-link (button-at (point))))
         (page (if button
                   (button-get button 'elpher-page)
                 elpher-current-page)))
    (unless page
      (error "Cannot bookmark this link"))
    (let* ((address (elpher-page-address page))
           (url (elpher-address-to-url address))
           (display-string (elpher-page-display-string page))
           (pos (if button nil (point))))
      (if (elpher-address-about-p address)
          (error "Cannot bookmark %s" display-string)
        `(,display-string
          (defaults . (,display-string))
          (position . ,pos)
          (location . ,url)
          (handler . elpher-bookmark-jump))))))

;;;###autoload
(defun elpher-bookmark-jump (bookmark)
  "Handler used to open a bookmark using elpher.
The argument BOOKMARK is a bookmark record passed to the function.
This handler is responsible for loading the bookmark in some buffer,
then making that buffer the current buffer.  It should not switch
to the buffer."
  (let* ((url (cdr (assq 'location bookmark)))
         (cleaned-url (string-trim url))
         (page (elpher-page-from-url cleaned-url))
         (buffer (get-buffer-create elpher-buffer-name)))
    (elpher-with-clean-buffer
     (elpher-visit-page page))
    (set-buffer buffer)
    nil))

(defun elpher-bookmark-link ()
  "Bookmark the link at point.
To bookmark the current page, use \\[elpher-bookmark-current]."
  (interactive)
  (let ((elpher-bookmark-link t))
    (bookmark-set-no-overwrite)))

(defun elpher-bookmark-current ()
  "Bookmark the current page.
To bookmark the link at point use \\[elpher-bookmark-link]."
  (interactive)
  (call-interactively #'bookmark-set-no-overwrite))

(defun elpher-bookmark-import (file)
  "Import legacy Elpher bookmarks file FILE into Emacs bookmarks."
  (interactive (list (if (and (boundp 'elpher-bookmarks-file)
			      (file-readable-p elpher-bookmarks-file))
			 elpher-bookmarks-file
		       (read-file-name "Old Elpher bookmarks: "
				       user-emacs-directory nil t
				       "elpher-bookmarks"))))
  (dolist (bookmark (with-temp-buffer
		      (insert-file-contents file)
		      (read (current-buffer))))
    (let* ((display-string (car bookmark))
           (url (cadr bookmark))
	   (record `(,display-string
		     (location . ,url)
		     (handler . elpher-bookmark-jump))))
      (bookmark-store display-string (cdr record) t)))
  (bookmark-save))

(defun elpher-get-bookmarks-page (renderer)
  "Getter which displays the bookmarks (RENDERER must be nil)."
  (when renderer
    (elpher-visit-previous-page)
    (error "Command not supported for bookmarks page"))

  (let ((old-bookmarks-file (or (and (boundp 'elpher-bookmarks-file)
                                     elpher-bookmarks-file)
                                (locate-user-emacs-file "elpher-bookmarks"))))
    (when (and (file-readable-p old-bookmarks-file)
               (y-or-n-p (concat "Legacy elpher-bookmarks file \""
                                 old-bookmarks-file
                                 "\" found. Import now?")))
      (elpher-bookmark-import old-bookmarks-file)
      (rename-file old-bookmarks-file (concat old-bookmarks-file "-legacy"))))

  (if (and elpher-use-emacs-bookmark-menu
           elpher-history)
      (progn
        (elpher-visit-previous-page)
        (call-interactively #'bookmark-bmenu-list))
    (elpher-with-clean-buffer
     (insert " ---- Elpher Bookmarks ---- \n\n")
     (bookmark-maybe-load-default-file)
     (dolist (bookmark (bookmark-maybe-sort-alist))
       (when (eq #'elpher-bookmark-jump (alist-get 'handler (cdr bookmark)))
         (let* ((name (car bookmark))
                (url (alist-get 'location (cdr bookmark)))
                (address (elpher-address-from-url url)))
           (elpher-insert-index-record name address))))
     (when (<= (line-number-at-pos) 3)
       (insert "No bookmarked pages found.\n"))
     (insert "\n --------------------------\n\n"
             "Select an entry or press 'u' to return to the previous page.\n\n"
             "Bookmarks can be renamed or deleted via the ")
     (insert-text-button "Emacs bookmark menu"
                         'action (lambda (_)
                                   (interactive)
                                   (call-interactively #'bookmark-bmenu-list))
                         'follow-link t
                         'help-echo "RET,mouse-1: open Emacs bookmark menu")
     (insert (substitute-command-keys
              ",\nwhich can also be opened from anywhere using '\\[bookmark-bmenu-list]'."))
     (elpher-restore-pos))))

(defun elpher-show-bookmarks ()
  "Interactive function to display the current list of elpher bookmarks."
  (interactive)
  (elpher-visit-page
   (elpher-make-page "Elpher Bookmarks"
                     (elpher-make-about-address 'bookmarks))))


;;; Integrations
;;

;;; Org

(defun elpher-org-export-link (link description format protocol)
  "Export a LINK with DESCRIPTION for the given PROTOCOL and FORMAT.

FORMAT is an Org export backend.  DESCRIPTION may be nil.  PROTOCOL may be one
of gemini, gopher or finger."
  (let* ((url (if (equal protocol "elpher")
                  (string-remove-prefix "elpher:" link)
                (format "%s:%s" protocol link)))
         (desc (or description url)))
    (pcase format
      (`gemini (format "=> %s %s" url desc))
      (`html (format "<a href=\"%s\">%s</a>" url desc))
      (`latex (format "\\href{%s}{%s}" url desc))
      (_ (if (not description)
             url
           (format "%s (%s)" desc url))))))

(defun elpher-org-store-link ()
  "Store link to an `elpher' page in Org."
  (when (eq major-mode 'elpher-mode)
    (let* ((url (elpher-info-current))
           (desc (car elpher-current-page))
           (protocol (cond
                      ((string-prefix-p "gemini:" url) "gemini")
                      ((string-prefix-p "gopher:" url) "gopher")
                      ((string-prefix-p "finger:" url) "finger")
                      (t "elpher"))))
      (when (equal "elpher" protocol)
        ;; Weird link. Or special inner link?
        (setq url (concat "elpher:" url)))
      (org-link-store-props :type protocol :link url :description desc)
      t)))

(defun elpher-org-follow-link (link protocol)
  "Visit a LINK for the given PROTOCOL.

PROTOCOL may be one of gemini, gopher or finger.  This method also
supports the old protocol elpher, where the link is self-contained."
  (let ((url (if (equal protocol "elpher")
                 (string-remove-prefix "elpher:" link)
               (format "%s:%s" protocol link))))
    (elpher-go url)))

(defun elpher-org-mode-integration ()
  "Set up `elpher' integration for `org-mode'."
  (org-link-set-parameters
   "elpher"
   :store #'elpher-org-store-link
   :export (lambda (link description format _plist)
             (elpher-org-export-link link description format "elpher"))
   :follow (lambda (link _arg) (elpher-org-follow-link link "elpher")))
  (org-link-set-parameters
   "gemini"
   :export (lambda (link description format _plist)
             (elpher-org-export-link link description format "gemini"))
   :follow (lambda (link _arg) (elpher-org-follow-link link "gemini")))
  (org-link-set-parameters
   "gopher"
   :export (lambda (link description format _plist)
             (elpher-org-export-link link description format "gopher"))
   :follow (lambda (link _arg) (elpher-org-follow-link link "gopher")))
  (org-link-set-parameters
   "finger"
   :export (lambda (link description format _plist)
             (elpher-org-export-link link description format "finger"))
   :follow (lambda (link _arg) (elpher-org-follow-link link "finger"))))

(add-hook 'org-mode-hook #'elpher-org-mode-integration)

;; Browse URL

;;;###autoload
(defun elpher-browse-url-elpher (url &rest _args)
  "Browse URL using Elpher.  This function is used by `browse-url'."
  (interactive (browse-url-interactive-arg "Elpher URL: "))
  (elpher-go url))

;; Use elpher to open gopher, finger and gemini links
;; For recent version of `browse-url' package
(if (boundp 'browse-url-default-handlers)
    (add-to-list
     'browse-url-default-handlers
     '("^\\(gopher\\|finger\\|gemini\\)://" . elpher-browse-url-elpher))
  ;; Patch `browse-url-browser-function' for older ones. The value of
  ;; that variable is `browse-url-default-browser' by default, so
  ;; that's the function that gets advised. If the value is an alist,
  ;; however, we don't know what to do. Better not interfere?
  (when (and (symbolp browse-url-browser-function)
             (fboundp browse-url-browser-function))
    (advice-add browse-url-browser-function :before-while
		(lambda (url &rest _args)
		  "Handle gemini, gopher, and finger schemes using Elpher."
                  (let ((scheme (downcase (car (split-string url ":" t)))))
                    (if (member scheme '("gemini" "gopher" "finger"))
			;; `elpher-go' always returns nil, which will stop the
			;; advice chain here in a before-while
			(elpher-go url)
                      ;; chain must continue, then return t.
                      t))))))

;; Register "gemini://" as a URI scheme so `browse-url' does the right thing
(with-eval-after-load 'thingatpt
  (add-to-list 'thing-at-point-uri-schemes "gemini://"))

;; Mu4e:

;; Make mu4e aware of the gemini world
(setq mu4e~view-beginning-of-url-regexp
      "\\(?:https?\\|gopher\\|finger\\|gemini\\)://\\|mailto:")

;; eww:

;; Let elpher handle gemini, gopher links in eww buffer.
(setq eww-use-browse-url
      "\\`mailto:\\|\\(\\`gemini\\|\\`gopher\\|\\`finger\\)://")


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

;;;###autoload
(defun elpher-go (host-or-url)
  "Go to a particular gopher site HOST-OR-URL.
When run interactively HOST-OR-URL is read from the minibuffer."
  (interactive (list
                (read-string (format "Visit URL (default scheme %s): " (elpher-get-default-url-scheme)))))
  (let ((trimmed-host-or-url (string-trim host-or-url)))
    (unless (string-empty-p trimmed-host-or-url)
      (let ((page (elpher-page-from-url trimmed-host-or-url
                                        (elpher-get-default-url-scheme))))
        (unless (get-buffer-window elpher-buffer-name t)
          (switch-to-buffer elpher-buffer-name))
        (elpher-with-clean-buffer
         (elpher-visit-page page))
        nil)))) ; non-nil value is displayed by eshell

(defun elpher-go-current ()
  "Go to a particular URL which is read from the minibuffer.
Unlike `elpher-go', the reader is initialized with the URL of the
current page."
  (interactive)
  (let* ((address (elpher-page-address elpher-current-page))
         (url (read-string (format "Visit URL (default scheme %s): " (elpher-get-default-url-scheme))
                           (elpher-address-to-url address))))
    (unless (string-empty-p (string-trim url))
      (elpher-visit-page (elpher-page-from-url url)))))

(defun elpher-redraw ()
  "Redraw current page."
  (interactive)
  (elpher-visit-page elpher-current-page))

(defun elpher-reload ()
  "Reload current page."
  (interactive)
  (elpher-reload-current-page))

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
  (if (elpher-address-about-p (elpher-page-address elpher-current-page))
      (error "This page was not generated by a server")
    (elpher-visit-page elpher-current-page
                       #'elpher-render-raw)))

(defun elpher-back ()
  "Go to previous site."
  (interactive)
  (elpher-visit-previous-page))

(defun elpher-back-to-start ()
  "Go all the way back to the start page."
  (interactive)
  (setq-local elpher-current-page nil)
  (setq-local elpher-history nil)
  (elpher-visit-page (elpher-make-start-page)))

(defun elpher-download ()
  "Download the link at point."
  (interactive)
  (let ((button (button-at (point))))
    (if button
        (let ((page (button-get button 'elpher-page)))
          (unless page
            (error "Not an elpher page"))
          (when (elpher-address-about-p (elpher-page-address page))
            (error "Cannot download %s" (elpher-page-display-string page)))
          (elpher-visit-page (button-get button 'elpher-page)
                             #'elpher-render-download))
      (error "No link selected"))))

(defun elpher-download-current ()
  "Download the current page."
  (interactive)
  (if (elpher-address-about-p (elpher-page-address elpher-current-page))
      (error "Cannot download %s"
             (elpher-page-display-string elpher-current-page))
    (elpher-visit-page (elpher-make-page
                        (elpher-page-display-string elpher-current-page)
                        (elpher-page-address elpher-current-page))
                       #'elpher-render-download
                       t)))

(defun elpher--build-link-map ()
  "Build alist mapping link names to destination pages in current buffer."
  (let ((link-map nil)
        (b (next-button (point-min) t)))
    (while b
      (push (cons (button-label b) b) link-map)
      (setq b (next-button (button-start b))))
    link-map))

(defun elpher-jump ()
  "Select a directory entry by name.  Similar to the info browser (m)enu command."
  (interactive)
  (let* ((link-map (elpher--build-link-map)))
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
  (let ((address (elpher-page-address elpher-current-page)))
    (if (not (elpher-address-about-p address))
        (if (or (member (url-filename address) '("/" ""))
                (and (elpher-address-gopher-p address)
                     (= (length (elpher-gopher-address-selector address)) 0)))
            (error "Already at root directory of current server")
          (let ((address-copy (elpher-address-from-url
                               (elpher-address-to-url address))))
            (setf (url-filename address-copy) "")
            (elpher-go (elpher-address-to-url address-copy))))
      (error "Command invalid for %s" (elpher-page-display-string elpher-current-page)))))

(defun elpher-info-page (page)
  "Display URL of PAGE in minibuffer."
  (let* ((address (elpher-page-address page))
         (url (elpher-address-to-url address))
         (iri (elpher-address-to-iri address)))
    (if (equal url iri)
        (message "%s" url)
      (message "%s (Raw: %s)" iri url))))

(defun elpher-info-link ()
  "Display information on page corresponding to link at point."
  (interactive)
  (let ((button (button-at (point))))
    (unless button
      (error "No item selected"))
    (let ((page (button-get button 'elpher-page)))
      (unless page
        (error "Not an elpher page"))
      (elpher-info-page page))))

(defun elpher-info-current ()
  "Display information on current page."
  (interactive)
  (elpher-info-page elpher-current-page))

(defun elpher-copy-page-url (page)
  "Copy URL representation of address of PAGE to `kill-ring'."
  (let* ((address (elpher-page-address page))
         (url (elpher-address-to-url address)))
    (message "Copied \"%s\" to kill-ring/clipboard." url)
    (kill-new url)))

(defun elpher-copy-link-url ()
  "Copy URL of item at point to `kill-ring'."
  (interactive)
  (let ((button (button-at (point))))
    (unless button
      (error "No item selected"))
    (let ((page (button-get button 'elpher-page)))
      (unless page
        (error "Not an elpher page"))
      (elpher-copy-page-url page))))

(defun elpher-copy-current-url ()
  "Copy URL of current page to `kill-ring'."
  (interactive)
  (elpher-copy-page-url elpher-current-page))

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
    (define-key map (kbd "C-M-i") 'elpher-prev-link)
    (define-key map (kbd "u") 'elpher-back)
    (define-key map (kbd "-") 'elpher-back)
    (define-key map (kbd "^") 'elpher-back)
    (define-key map [mouse-3] 'elpher-back)
    (define-key map (kbd "U") 'elpher-back-to-start)
    (define-key map (kbd "g") 'elpher-go)
    (define-key map (kbd "o") 'elpher-go-current)
    (define-key map (kbd "O") 'elpher-root-dir)
    (define-key map (kbd "s") 'elpher-show-history)
    (define-key map (kbd "S") 'elpher-show-visited-pages)
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
    (define-key map (kbd "B") 'elpher-show-bookmarks)
    (define-key map (kbd "!") 'elpher-set-gopher-coding-system)
    (define-key map (kbd "F") 'elpher-forget-current-certificate)
    (when (fboundp 'evil-define-key*)
      (evil-define-key*
       'motion map
       (kbd "TAB") 'elpher-next-link
       (kbd "C-") 'elpher-follow-current-link
       (kbd "C-t") 'elpher-back
       (kbd "u") 'elpher-back
       (kbd "-") 'elpher-back
       (kbd "^") 'elpher-back
       [mouse-3] 'elpher-back
       (kbd "U") 'elpher-back-to-start
       (kbd "g") 'elpher-go
       (kbd "o") 'elpher-go-current
       (kbd "O") 'elpher-root-dir
       (kbd "s") 'elpher-show-history
       (kbd "S") 'elpher-show-visited-pages
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
       (kbd "B") 'elpher-show-bookmarks
       (kbd "!") 'elpher-set-gopher-coding-system
       (kbd "F") 'elpher-forget-current-certificate))
    map)
  "Keymap for gopher client.")

(define-derived-mode elpher-mode special-mode "elpher"
  "Major mode for elpher, an elisp gopher client.

This mode is automatically enabled by the interactive
functions which initialize the client, namely
`elpher', and `elpher-go'."
  (setq-local elpher-current-page nil)
  (setq-local elpher-history nil)
  (setq-local elpher-buffer-name (buffer-name))
  (setq-local bookmark-make-record-function #'elpher-bookmark-make-record)
  (setq-local imenu-create-index-function #'elpher-build-current-imenu-index))

(when (fboundp 'evil-set-initial-state)
  (evil-set-initial-state 'elpher-mode 'motion))


;;; Main start procedure
;;

;;;###autoload
(defun elpher (&optional arg)
  "Start elpher with default landing page.
The buffer used for Elpher sessions is determined by the value of
‘elpher-buffer-name’.  If there is already an Elpher session active in
that buffer, Emacs will simply switch to it.  Otherwise, a new session
will begin.  A numeric prefix ARG (as in ‘\\[universal-argument] 42
\\[execute-extended-command] elpher RET’) switches to the session with
that number, creating it if necessary.  A non numeric prefix ARG means
to create a new session.  Returns the buffer selected (or created)."
  (interactive "P")
  (let* ((name (default-value 'elpher-buffer-name))
         (buf (cond ((numberp arg)
                     (get-buffer-create (format "%s<%d>" name arg)))
                    (arg
                     (generate-new-buffer name))
                    (t
                     (get-buffer-create name)))))
    (pop-to-buffer-same-window buf)
    (unless (buffer-modified-p)
      (elpher-mode)
      (elpher-visit-page (elpher-make-start-page))
      "Started Elpher."))); Otherwise (elpher) evaluates to start page string.

;;; elpher.el ends here
