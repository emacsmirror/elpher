;;; elopher.el --- gopher client

;;; Commentary:

;; Simple gopher client in elisp.

;;; Code:

;;; Global constants
;;

(defconst elopher-version "1.0.0"
  "Current version of elopher.")

(defconst elopher-margin-width 6
  "Width of left-hand margin used when rendering indicies.")

(defconst elopher-start-index
  (string-join
   (list "i\tfake\tfake\t1"
         "i--------------------------------------------\tfake\tfake\t1"
         "i          Elopher Gopher Client             \tfake\tfake\t1"
         (format "i              version %s\tfake\tfake\t1" elopher-version)
         "i--------------------------------------------\tfake\tfake\t1"
         "i\tfake\tfake\t1"
         "iBasic usage:\tfake\tfake\t1"
         "i - tab/shift-tab: next/prev directory entry\tfake\tfake\t1"
         "i - RET/mouse-1: open directory entry\tfake\tfake\t1"
         "i - u: return to parent directory entry\tfake\tfake\t1"
         "i - g: go to a particular site\tfake\tfake\t1"
         "i - r: reload current page\tfake\tfake\t1"
         "i\tfake\tfake\t1"
         "iPlaces to start exploring Gopherspace:\tfake\tfake\t1"
         "1Floodgap Systems Gopher Server\t\tgopher.floodgap.com\t70"
         "1Super-Dimensional Fortress\t\tsdf.org\t70"
         "i\tfake\tfake\t1"
         "iTest entries:\tfake\tfake\t1"
         "pXKCD comic image\t/fun/xkcd/comics/2130/2137/text_entry.png\tgopher.floodgap.com\t70"
         "1Test server\t\tlocalhost\t70"
         ".")
   "\r\n"))


;;; Customization group
;;

(defgroup elopher nil
  "A simple gopher client."
  :group 'applications)

(defcustom elopher-index-face '(foreground-color . "cyan")
  "Face used for index records.")
(defcustom elopher-text-face '(foreground-color . "white")
  "Face used for text records.")
(defcustom elopher-info-face '(foreground-color . "gray")
  "Face used for info records.")
(defcustom elopher-image-face '(foreground-color . "green")
  "Face used for image records.")
(defcustom elopher-unknown-face '(foreground-color . "red")
  "Face used for unknown record types.")

;;; Model
;;

;; Address

(defun elopher-make-address (selector host port)
  (list selector host port))

(defun elopher-address-selector (address)
  (car address))

(defun elopher-address-host (address)
  (cadr address))

(defun elopher-address-port (address)
  (caddr address))

;; Node

(defun elopher-make-node (parent address getter &optional content pos)
  (list parent address getter content pos))

(defun elopher-node-parent (node)
  (elt node 0))

(defun elopher-node-address (node)
  (elt node 1))

(defun elopher-node-getter (node)
  (elt node 2))

(defun elopher-node-content (node)
  (elt node 3))

(defun elopher-node-pos (node)
  (elt node 4))

(defun elopher-set-node-content (node content)
  (setcar (nthcdr 3 node) content))

(defun elopher-set-node-pos (node pos)
  (setcar (nthcdr 4 node) pos))

(defun elopher-save-pos ()
  (when elopher-current-node
    (elopher-set-node-pos elopher-current-node (point))))

(defun elopher-restore-pos ()
  (let ((pos (elopher-node-pos elopher-current-node)))
    (if pos
        (goto-char pos)
      (goto-char (point-min)))))

;; Node graph traversal

(defvar elopher-current-node)

(defun elopher-visit-node (node)
  (elopher-save-pos)
  (elopher-prepare-buffer)
  (setq elopher-current-node node)
  (funcall (elopher-node-getter node)))

(defun elopher-visit-parent-node ()
  (let ((parent-node (elopher-node-parent elopher-current-node)))
    (when parent-node
      (elopher-visit-node parent-node))))
      
(defun elopher-reload-current-node ()
  (elopher-set-node-content elopher-current-node nil)
  (elopher-visit-node elopher-current-node))


;;; Buffer preparation
;;

(defun elopher-prepare-buffer ()
  (switch-to-buffer "*elopher*")
  (elopher-mode)
  (let ((inhibit-read-only t))
    (erase-buffer)))


;;; Index rendering
;;

(defun elopher-insert-index (string)
  "Inserts the index corresponding to STRING into the current buffer."
  (dolist (line (split-string string "\r\n"))
    (unless (string-empty-p line)
      (elopher-insert-index-record line))))

(defun elopher-insert-margin (&optional type-name)
  "Insert index margin, optionally containing the TYPE-NAME, into the current buffer."
  (if type-name
      (progn
        (insert (format (concat "%" (number-to-string (- elopher-margin-width 1)) "s")
                        (concat
                         (propertize "[" 'face '(foreground-color . "blue"))
                         (propertize type-name 'face '(foreground-color . "white"))
                         (propertize "]" 'face '(foreground-color . "blue")))))
        (insert " "))
    (insert (make-string elopher-margin-width ?\s))))

(defun elopher-insert-index-record (line)
  "Inserts the index record corresponding to LINE into the current buffer."
  (let* ((type (elt line 0))
         (fields (split-string (substring line 1) "\t"))
         (display-string (elt fields 0))
         (address (elopher-make-address (elt fields 1) (elt fields 2) (elt fields 3)))
         (help-string (format "mouse-1, RET: open %s on %s port %s"
                              (elopher-address-selector address)
                              (elopher-address-host address)
                              (elopher-address-port address))))
    (pcase type
      (?i (elopher-insert-margin)
          (insert (propertize display-string
                              'face elopher-info-face)))
      (?0 (elopher-insert-margin "T")
          (insert-text-button display-string
                              'face elopher-text-face
                              'elopher-node (elopher-make-node elopher-current-node
                                                               address
                                                               #'elopher-get-text-node)
                              'action #'elopher-click-link
                              'follow-link t
                              'help-echo help-string))
      (?1 (elopher-insert-margin "/")
          (insert-text-button display-string
                              'face elopher-index-face
                              'elopher-node (elopher-make-node elopher-current-node
                                                               address
                                                               #'elopher-get-index-node)
                              'action #'elopher-click-link
                              'follow-link t
                              'help-echo help-string))
      (?p (elopher-insert-margin "im")
          (insert-text-button display-string
                              'face elopher-image-face
                              'elopher-node (elopher-make-node elopher-current-node
                                                               address
                                                               #'elopher-get-image-node)
                              'action #'elopher-click-link
                              'follow-link t
                              'help-echo help-string))
      (?.) ; Occurs at end of index, can safely ignore.
      (tp (elopher-insert-margin (concat (char-to-string tp) "?"))
          (insert (propertize display-string
                              'face elopher-unknown-face))))
    (insert "\n")))


;;; Selector retrieval (all kinds)
;;

(defvar elopher-selector-string)

(defun elopher-get-selector (address after &optional binary)
  "Retrieve selector specified by ADDRESS and store it in the
string elopher-selector-string, then execute AFTER as the
sentinal function.

If BINARY is non-nil, the selector is expected to return a
binary result, otherwise otherwise utf-8 is assumed."
  (setq elopher-selector-string "")
  (let ((p (get-process "elopher-process")))
    (if p (delete-process p)))
  (make-network-process
   :name "elopher-process"
   :host (elopher-address-host address)
   :service (elopher-address-port address)
   :coding (if binary
               '(utf-8 . binary)
             '(utf-8 . utf-8))
   :filter (lambda (proc string)
             (setq elopher-selector-string (concat elopher-selector-string string)))
   :sentinel after)
  (process-send-string "elopher-process"
                       (concat (elopher-address-selector address) "\n")))

;; Index retrieval

(defun elopher-get-index-node ()
  (let ((content (elopher-node-content elopher-current-node))
        (address (elopher-node-address elopher-current-node)))
    (if content
        (progn
          (let ((inhibit-read-only t))
            (insert content))
          (elopher-restore-pos))
      (if address
          (elopher-get-selector address
                                (lambda (proc event)
                                  (let ((inhibit-read-only t))
                                    (elopher-insert-index elopher-selector-string))
                                  (elopher-restore-pos)
                                  (elopher-set-node-content elopher-current-node
                                                            (buffer-string))))
        (progn
          (let ((inhibit-read-only t))
            (elopher-insert-index elopher-start-index))
          (elopher-restore-pos)
          (elopher-set-node-content elopher-current-node
                                    (buffer-string)))))))

;; Text retrieval

(defun elopher-strip-CRs (string)
  (replace-regexp-in-string "\r" "" string))

(defun elopher-get-text-node ()
  (let ((content (elopher-node-content elopher-current-node))
        (address (elopher-node-address elopher-current-node)))
    (if content
        (progn
          (let ((inhibit-read-only t))
            (insert content))
          (elopher-restore-pos))
      (elopher-get-selector address
                            (lambda (proc event)
                              (let ((inhibit-read-only t))
                                (insert (elopher-strip-CRs elopher-selector-string)))
                              (elopher-restore-pos)
                              (elopher-set-node-content elopher-current-node
                                                        (buffer-string)))))))

;; Image retrieval

(defun elopher-get-image-node ()
  (let ((content (elopher-node-content elopher-current-node))
        (address (elopher-node-address elopher-current-node)))
    (if content
        (progn
          (let ((inhibit-read-only t))
            (insert-image content))
          (elopher-restore-pos))
      (elopher-get-selector address
                            (lambda (proc event)
                              (let ((image (create-image elopher-selector-string))
                                    (inhibit-read-only t))
                                (insert-image image)
                                (elopher-restore-pos)
                                (elopher-set-node-content image)))
                            'binary))))
        
  

;;; Navigation procedures
;;

(defun elopher-next-link ()
  (interactive)
  (forward-button 1))

(defun elopher-prev-link ()
  (interactive)
  (backward-button 1))

(defun elopher-click-link (button)
  (let ((node (button-get button 'elopher-node)))
    (elopher-visit-node node)))

(defun elopher-follow-closest-link ()
  (interactive)
  (push-button))

(defun elopher-go ()
  "Go to a particular gopher site."
  (interactive)
  (let* ((selector "")
         (hostname (read-from-minibuffer "Gopher host: "))
         (port 70)
         (address (list selector hostname port)))
    (elopher-visit-node
     (elopher-make-node elopher-current-node
                        address
                        #'elopher-get-index-node))))

(defun  elopher-reload ()
  "Reload current site."
  (interactive)
  (elopher-reload-current-node))

(defun elopher-back ()
  "Go to previous site."
  (interactive)
  (if (elopher-node-parent elopher-current-node)
      (elopher-visit-parent-node)
    (message "No previous site.")))


;;; Mode and keymap
;;

(defvar elopher-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") 'elopher-next-link)
    (define-key map (kbd "<S-tab>") 'elopher-prev-link)
    (define-key map (kbd "u") 'elopher-back)
    (define-key map (kbd "g") 'elopher-go)
    (define-key map (kbd "r") 'elopher-reload)
    (when (require 'evil nil t)
      (evil-define-key 'normal map
        (kbd "C-]") 'elopher-follow-closest-link
        (kbd "C-t") 'elopher-back
        (kbd "u") 'elopher-back
        (kbd "g") 'elopher-go
        (kbd "r") 'elopher-reload))
    map)
  "Keymap for gopher client.")

(define-derived-mode elopher-mode special-mode "elopher"
  "Major mode for elopher, an elisp gopher client.")


;;; Main start procedure
;;
(defun elopher ()
  "Start elopher with default landing page."
  (interactive)
  (setq elopher-current-node nil)
  (let ((start-node (elopher-make-node nil nil #'elopher-get-index-node)))
    (elopher-visit-node start-node)))

;;; elopher.el ends here
