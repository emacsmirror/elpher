;;; elopher.el --- gopher client

;;; Commentary:

;; Simple gopher client in elisp.

;;; Code:

(setq elopher-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") 'elopher-next-link)
    (define-key map (kbd "<S-tab>") 'elopher-prev-link)
    (define-key map (kbd "u") 'elopher-history-back)
    (define-key map (kbd "g") 'elopher-go)
    (when (require 'evil nil t)
      (evil-define-key 'normal map
        (kbd "C-]") 'elopher-follow-closest-link
        (kbd "C-t") 'elopher-history-back
        (kbd "u") 'elopher-history-back
        (kbd "g") 'elopher-go))
    map))
  ;; "Keymap for gopher client.")

(define-derived-mode elopher-mode special-mode "elopher"
  "Major mode for elopher, an elisp gopher client.")

(defvar elopher-margin-width 5)

(defun elopher-insert-margin (&optional type-name)
  (if type-name
      (insert (propertize
               (format (concat "%" (number-to-string elopher-margin-width) "s")
                       (concat "[" type-name "] "))
               'face '(foreground-color . "orange")))
    (insert (make-string elopher-margin-width ?\s))))

(defun elopher-render-record (line)
  (let* ((type (elt line 0))
         (fields (split-string (substring line 1) "\t"))
         (display-string (elt fields 0))
         (selector (elt fields 1))
         (hostname (elt fields 2))
         (port (elt fields 3))
         (address (list selector hostname port)))
    (pcase type
      (?i (elopher-insert-margin)
          (insert (propertize display-string
                              'face '(foreground-color . "gray"))))
      (?0 (elopher-insert-margin "T")
          (insert-text-button display-string
                              'face '(foreground-color . "white")
                              'link-getter #'elopher-get-text
                              'link-address address
                              'action #'elopher-click-link
                              'follow-link t))
      (?1 (elopher-insert-margin "/")
          (insert-text-button display-string
                              'face '(foreground-color . "cyan")
                              'link-getter #'elopher-get-index
                              'link-address address
                              'action #'elopher-click-link
                              'follow-link t))
      (?p (elopher-insert-margin "img")
          (insert-text-button display-string
                             'face '(foreground-color . "cyan")
                             'link-getter #'elopher-get-image
                             'link-address address
                             'action #'elopher-click-link
                             'follow-link t))
      (?.) ; Occurs at end of index, can safely ignore.
      (tp (elopher-insert-margin (concat (char-to-string tp) "?"))
          (insert (propertize display-string
                              'face '(foreground-color . "red")))))
    (insert "\n")))

(defvar elopher-incomplete-record "")

(defun elopher-render-complete-records (string)
  (let* ((til-now (string-join (list elopher-incomplete-record string)))
         (lines (split-string til-now "\r\n")))
    (dotimes (idx (length lines))
      (if (< idx (- (length lines) 1))
          (let ((line (elt lines idx)))
            (unless (string-empty-p line)
              (elopher-render-record line)))
        (setq elopher-incomplete-record (elt lines idx))))))

(defun elopher-get-selector (selector host port filter &optional sentinel)
  (switch-to-buffer "*elopher*")
  (elopher-mode)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (make-network-process
   :name "elopher-process"
   :host host
   :service (if port port 70)
   :filter filter
   :sentinel sentinel)
  (process-send-string "elopher-process" (concat selector "\n")))

(defun elopher-index-filter (proc string)
  (with-current-buffer (get-buffer "*elopher*")
    (let ((marker (process-mark proc))
          (inhibit-read-only t))
      (if (not (marker-position marker))
          (set-marker marker 0 (current-buffer)))
      (save-excursion
        (goto-char marker)
        (elopher-render-complete-records string)
        (set-marker marker (point))))))
    
(defun elopher-get-index (selector host port)
  (setq elopher-incomplete-record "")
  (elopher-get-selector selector host port #'elopher-index-filter))

(defun elopher-text-filter (proc string)
  (with-current-buffer (get-buffer "*elopher*")
    (let ((marker (process-mark proc))
          (inhibit-read-only t))
      (if (not (marker-position marker))
          (set-marker marker 0 (current-buffer)))
      (save-excursion
        (goto-char marker)
        (dolist (line (split-string string "\r"))
          (insert line))
        (set-marker marker (point))))))

(defun elopher-get-text (selector host port)
  (elopher-get-selector selector host port #'elopher-text-filter))

(defvar elopher-image-buffer "")

(defun elopher-image-filter (proc string)
  (setq elopher-image-buffer (concat elopher-image-buffer string)))

(defun elopher-image-sentinel (proc event)
  (let ((inhibit-read-only t))
    (insert-image (create-image elopher-image-buffer))))

(defun elopher-get-image (selector host port)
  (setq elopher-image-buffer "")
  (elopher-get-selector selector host port #'elopher-image-filter #'elopher-image-sentinel))

(defun elopher-history-back ()
  (interactive)
  (let ((inhibit-read-only t))
    (undo)))

(defun elopher-next-link ()
  (interactive)
  (forward-button 1))

(defun elopher-prev-link ()
  (interactive)
  (backward-button 1))

(defun elopher-click-link (button)
  (apply (button-get button 'link-getter) (button-get button 'link-address)))

(defun elopher-follow-closest-link ()
  (interactive)
  (push-button))

(defun elopher-go ()
  "Go to a particular gopher site."
  (interactive)
  (elopher-get-index "" (read-from-minibuffer "Gopher host: ") 70))

(defun elopher ()
  "Start elopher with default landing page."
  (interactive)
  (switch-to-buffer "*elopher*")
  (elopher-mode)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (save-excursion
      (elopher-render-complete-records
       (concat "i\tfake\tfake\t1\r\n"
               "i--------------------------------------------\tfake\tfake\t1\r\n"
               "i          Elopher Gopher Client             \tfake\tfake\t1\r\n"
               "i              version 1.0                   \tfake\tfake\t1\r\n"
               "i--------------------------------------------\tfake\tfake\t1\r\n"
               "i\tfake\tfake\t1\r\n"
               "iBasic usage:\tfake\tfake\t1\r\n"
               "i - tab/shift-tab: next/prev directory entry\tfake\tfake\t1\r\n"
               "i - RET/mouse-1: open directory entry\tfake\tfake\t1\r\n"
               "i - u: return to parent directory entry\tfake\tfake\t1\r\n"
               "i - g: go to a particular site\tfake\tfake\t1\r\n"
               "i\tfake\tfake\t1\r\n"
               "iPlaces to start exploring Gopherspace:\tfake\tfake\t1\r\n"
               "1Floodgap Systems Gopher Server\t\tgopher.floodgap.com\t70\r\n"
               "1Super-Dimensional Fortress\t\tgopher.floodgap.com\t70\r\n"
               "i\tfake\tfake\t1\r\n"
               "iTest entries:\tfake\tfake\t1\r\n"
               "pXKCD comic image\t/fun/xkcd/comics/2130/2137/text_entry.png\tgopher.floodgap.com\t70\r\n")))))

(elopher)
;; (elopher-get-index "" "gopher.floodgap.com" 70)
;; (elopher-get-image "/fun/xkcd/comics/2130/2137/text_entry.png" "gopher.floodgap.com" 70)

;;; elopher.el ends here
