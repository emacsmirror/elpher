;;; elopher.el --- gopher client

;;; Commentary:

;; Simple gopher client in elisp.

;;; Code:

;; (defvar elopher-mode-map nil "Keymap for gopher client.")

;; (define-derived-mode elopher-mode special-mode "elopher"
;;   "Major mode for elopher, an elisp gopher client.")

(defvar elopher-margin-width 5)

(defun elopher-insert-margin (&optional type-name)
  (if type-name
      (insert (propertize
               (format (concat "%" (number-to-string elopher-margin-width) "s")
                       (concat "[" type-name "] "))
               'face '(foreground-color . "yellow")))
    (insert (make-string elopher-margin-width ?\s))))

(defun elopher-follow-index-link (button)
  (apply #'elopher-get-index (button-get button 'link-address)))

(defun elopher-follow-text-link (button)
  (apply #'elopher-get-text (button-get button 'link-address)))

(defun elopher-follow-image-link (button)
  (apply #'elopher-get-image (button-get button 'link-address)))

(defun elopher-process-record (line)
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
                              'face '(foreground-color. "white"))))
      (?0 (elopher-insert-margin "T")
          (insert-text-button display-string
                              'face '(foreground-color . "gray")
                              'link-address address
                              'action #'elopher-follow-text-link
                              'follow-link t))
      (?1 (elopher-insert-margin "/")
          (insert-text-button display-string
                              'face '(foreground-color . "cyan")
                              'link-address address
                              'action #'elopher-follow-index-link
                              'follow-link t))
      (?p (elopher-insert-margin "img")
          (insert-text-button display-string
                             'face '(foreground-color . "gray")
                             'link-address address
                             'action #'elopher-follow-image-link
                             'follow-link t))
      (?.) ; Occurs at end of index, can safely ignore.
      (tp (elopher-insert-margin (concat (char-to-string tp) "?"))
          (insert (propertize display-string
                              'face '(foreground-color . "red")))))
    (insert "\n")))

(defvar elopher-incomplete-record "")

(defun elopher-process-complete-records (string)
  (let* ((til-now (string-join (list elopher-incomplete-record string)))
         (lines (split-string til-now "\r\n")))
    (dotimes (idx (length lines))
      (if (< idx (- (length lines) 1))
          (let ((line (elt lines idx)))
            (unless (string-empty-p line)
              (elopher-process-record line)))
        (setq elopher-incomplete-record (elt lines idx))))))

(defun elopher-index-filter (proc string)
  (with-current-buffer (get-buffer "*elopher*")
    (let ((marker (process-mark proc)))
      (if (not (marker-position marker))
          (set-marker marker 0 (current-buffer)))
      (save-excursion
        (goto-char marker)
        (elopher-process-complete-records string)
        (set-marker marker (point))))))
    
(defun elopher-get-index (selector host port)
  (switch-to-buffer "*elopher*")
  (erase-buffer)
  (setq elopher-incomplete-record "")
  (make-network-process
   :name "elopher-process"
   :host host
   :service (if port port 70)
   :filter #'elopher-index-filter)
  (process-send-string "elopher-process" (concat selector "\n")))

(defun elopher-text-filter (proc string)
  (with-current-buffer (get-buffer "*elopher*")
    (let ((marker (process-mark proc)))
      (if (not (marker-position marker))
          (set-marker marker 0 (current-buffer)))
      (save-excursion
        (goto-char marker)
        (dolist (line (split-string string "\r"))
          (insert line))
        (set-marker marker (point))))))

(defun elopher-get-text (selector host port)
  (switch-to-buffer "*elopher*")
  (erase-buffer)
  (make-network-process
   :name "elopher-process"
   :host host
   :service port
   :filter #'elopher-text-filter)
  (process-send-string "elopher-process" (concat selector "\n")))

(defvar elopher-image-buffer "")

(defun elopher-image-filter (proc string)
  (setq elopher-image-buffer (concat elopher-image-buffer string)))

(defun elopher-get-image (selector host port)
  (switch-to-buffer "*elopher*")
  (erase-buffer)
  (setq elopher-image-buffer "")
  (make-network-process
   :name "elopher-process"
   :host host
   :service port
   :filter #'elopher-image-filter)
  (process-send-string "elopher-process" (concat selector "\n"))
  (insert-image (create-image elopher-image-buffer)))

(defun elopher ()
  "Start gopher client."
  (interactive)
  (elopher-get-index "" (read-from-minibuffer "Gopher host: ") 70))

(elopher-get-index "" "cosmic.voyage" 70)
;; (elopher-get-image "/fun/xkcd/comics/2130/2137/text_entry.png" "gopher.floodgap.com" 70)

;;; elopher.el ends here
