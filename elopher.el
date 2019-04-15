;;; elopher.el --- gopher client

;;; Commentary:

;; Simple gopher client in elisp.

;;; Code:

;; (defvar elopher-mode-map nil "Keymap for gopher client.")
;; (define-key elopher-mode-map (kbd "p") 'elopher-quit)

;; (define-derived-mode elopher-mode special-mode "elopher"
;;   "Major mode for elopher, an elisp gopher client.")

;; (global-set-key (kbd "C-c C-b") 'eval-buffer)

(defvar elopher-type-margin-width 5)

(defvar elopher-history '())

(defun elopher-type-margin (&optional type-name)
  (if type-name
      (insert (propertize
               (format (concat "%" (number-to-string elopher-type-margin-width) "s")
                       (concat "[" type-name "] "))
               'face '(foreground-color . "yellow")))
    (insert (make-string elopher-type-margin-width ?\s))))

(defun elopher-follow-index-link (button)
  (apply #'elopher-get-index (button-get button 'link-address)))

(defun elopher-follow-text-link (button)
  (apply #'elopher-get-text (button-get button 'link-address)))

(defun elopher-process-record (line)
  (let* ((type (elt line 0))
         (fields (split-string (substring line 1) "\t"))
         (display-string (elt fields 0))
         (selector (elt fields 1))
         (hostname (elt fields 2))
         (port (elt fields 3))
         (address (list selector hostname port)))
    (pcase type
      (?i (elopher-type-margin)
          (insert (propertize display-string
                              'face '(foreground-color. "white"))))
      (?0 (elopher-type-margin "T")
          (insert-text-button display-string
                              'face '(foreground-color . "gray")
                              'link-address address
                              'action #'elopher-follow-text-link
                              'follow-link t))
      (?1 (elopher-type-margin "/")
          (insert-text-button display-string
                              'face '(foreground-color . "cyan")
                              'link-address address
                              'action #'elopher-follow-index-link
                              'follow-link t)))
    (insert "\n")))

(defvar elopher-incomplete-record "")

(defun elopher-process-complete-records (string)
  (let* ((til-now (string-join (list elopher-incomplete-record string)))
         (lines (split-string til-now "\r\n")))
    (dotimes (idx (length lines))
      (if (< idx (- (length lines) 1))
          (elopher-process-record (elt lines idx))
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
  (setq elopher-incomplete-record "")
  (make-network-process
   :name "elopher-process"
   :host host
   :service port
   :filter #'elopher-text-filter)
  (process-send-string "elopher-process" (concat selector "\n")))

(defun elopher ()
  "Start gopher client."
  (interactive)
  (elopher-get-index "" (read-from-minibuffer "Gopher host: ") 70))

;; (elopher-get-index "" "cosmic.voyage" 70)
;; (elopher-get-index "" "gopher.floodgap.com" 70)

;;; elopher.el ends here
