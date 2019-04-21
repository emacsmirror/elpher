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

(defconst elopher-start-page
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

(defun elopher-make-node (parent address getter &optional content)
  (list parent address getter content))

(defun elopher-node-parent (node)
  (car node))

(defun elopher-node-address (node)
  (cadr node))

(defun elopher-node-getter (node)
  (caddr node))

(defun elopher-node-content (node)
  (cadddr node))

(defun elopher-set-node-content (node content)
  (setcar (cdddr node) content))

;; Node graph traversal

(defvar elopher-start-node (elopher-make-node nil nil #'elopher-get-index-node))
(defvar elopher-current-node)

(defun elopher-visit-node (node)
  (elopher-prepare-buffer)
  (setq elopher-current-node node)
  (funcall (elopher-node-getter node)))

(defun elopher-visit-parent-node ()
  (let ((parent-node (elopher-node-parent elopher-current-node)))
    (when parent-node
      (setq elopher-current-node parent-node)
      (elopher-visit-node elopher-current-node))))
      
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
         (address (elopher-make-address (elt fields 1) (elt fields 2) (elt fields 3))))
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
                              'follow-link t))
      (?1 (elopher-insert-margin "/")
          (insert-text-button display-string
                              'face elopher-index-face
                              'elopher-node (elopher-make-node elopher-current-node
                                                               address
                                                               #'elopher-get-index-node)
                              'action #'elopher-click-link
                              'follow-link t))
      (?.) ; Occurs at end of index, can safely ignore.
      (tp (elopher-insert-margin (concat (char-to-string tp) "?"))
          (insert (propertize display-string
                              'face elopher-unknown-face))))
    (insert "\n")))


;;; Selector retrieval (all kinds)
;;

(defvar elopher-selector-string)

(defun elopher-get-selector (address after)
  (setq elopher-selector-string "")
  (let ((p (get-process "elopher-process")))
    (if p (delete-process p)))
  (make-network-process
   :name "elopher-process"
   :host (elopher-address-host address)
   :service (elopher-address-port address)
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
        (let ((inhibit-read-only t))
          (save-excursion
            (insert content)))
      (if address
          (elopher-get-selector address
                                (lambda (proc event)
                                  (let ((inhibit-read-only t))
                                    (erase-buffer)
                                    (save-excursion
                                      (elopher-insert-index elopher-selector-string)))
                                  (elopher-set-node-content elopher-current-node
                                                            (buffer-string))))
        (progn
          (let ((inhibit-read-only t))
            (erase-buffer)
            (save-excursion
              (elopher-insert-index elopher-start-page)))
          (elopher-set-node-content elopher-current-node
                                    (buffer-string)))))))

;; Text retrieval

(defun elopher-strip-CRs (string)
  (replace-regexp-in-string "\r" "" string))

(defun elopher-get-text-node ()
  (let ((content (elopher-node-content elopher-current-node))
        (address (elopher-node-address elopher-current-node)))
    (if content
        (let ((inhibit-read-only t))
          (save-excursion
            (insert content)))
      (elopher-get-selector address
                            (lambda (proc event)
                              (let ((inhibit-read-only t))
                                (erase-buffer)
                                (save-excursion
                                  (insert
                                   (elopher-strip-CRs elopher-selector-string))))
                              (elopher-set-node-content elopher-current-node
                                                        (buffer-string)))))))

;;; Navigation methods
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
  (elopher-visit-parent-node))

;;; Main start procedure
;;
(defun elopher ()
  "Start elopher with default landing page."
  (interactive)
  (elopher-visit-node elopher-start-node))


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


;;; elopher.el ends here
