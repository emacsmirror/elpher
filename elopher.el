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

(defun elopher-make-clickable (string link-function mouse-help)
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] link-function)
    (define-key map (kbd "RET") link-function)
    (propertize string
                'mouse-face 'highlight
                'help-echo (concat "mouse-1: " mouse-help)
                'keymap map)))

(defun elopher-format-record (margin-key color &optional getter)
  (elopher-type-margin margin-key)
  (insert (propertize
           (if getter
               (elopher-make-clickable display-string
                                       `(lambda () (interactive)
                                          (,getter ,hostname ,port ,selector))
                                       (format "open \"%s\" on %s port %s"
                                               selector hostname port))
             display-string)
           'face `(foreground-color . ,color)))
  (insert "\n"))

(defun elopher-format-i (display-string)
  (elopher-format-record nil "white"))

(defun elopher-format-0 (display-string selector hostname port)
  (elopher-format-record "T" "gray" 'elopher-get-text))

(defun elopher-format-1 (display-string selector hostname port)
  (elopher-format-record "/" "cyan" 'elopher-get-index))

(defun elopher-process-record (line)
  (let* ((type (elt line 0))
         (fields (split-string (substring line 1) "\t"))
         (g-display-string (elt fields 0))
         (g-selector (elt fields 1))
         (g-hostname (elt fields 2))
         (g-port (elt fields 3)))
    (pcase type
      (?i (elopher-format-i g-display-string))
      (?0 (elopher-format-0 g-display-string g-selector g-hostname g-port))
      (?1 (elopher-format-1 g-display-string g-selector g-hostname g-port)))))

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
    
(defun elopher-get-index (host &optional port path)
  (switch-to-buffer "*elopher*")
  (erase-buffer)
  (setq elopher-incomplete-record "")
  (make-network-process
   :name "elopher-process"
   :host host
   :service (if port port 70)
   :filter #'elopher-index-filter)
  (process-send-string "elopher-process" (concat path "\n")))

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

(defun elopher-get-text (host port selector)
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
  (elopher-get-index (read-from-minibuffer "Gopher host: ") 70))

;; (elopher-get-index "cosmic.voyage")
(elopher-get-index "gopher.floodgap.com")
;; (elopher-get-index "maurits.id.au")

(defun elopher-quit ()
  (interactive)
  (kill-buffer "*elopher*"))

;;; elopher.el ends here
