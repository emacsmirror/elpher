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

(defun elopher-format-record (display-string margin-key color &optional getter help-text)
  (elopher-type-margin margin-key)
  (insert (propertize
           (if getter
               (elopher-make-clickable display-string
                                       getter
                                       help-text)
             display-string)
           'face `(foreground-color . ,color)))
  (insert "\n"))

(defun elopher-make-getter (func address)
  (let ((selector (car address))
        (hostname (cadr address))
        (port (caddr address)))
    `(lambda ()
       (interactive)
       (,func ,hostname ,port ,selector))))

(defun elopher-make-help (address)
  (let ((selector (car address))
        (hostname (cadr address))
        (port (caddr address)))
    (format "open \"%s\" on %s port %s"
            selector hostname port)))

(defun elopher-process-record (line)
  (let* ((type (elt line 0))
         (fields (split-string (substring line 1) "\t"))
         (display-string (elt fields 0))
         (selector (elt fields 1))
         (hostname (elt fields 2))
         (port (elt fields 3))
         (address (list selector hostname port)))
    (pcase type
      (?i (elopher-format-record display-string nil "white"))
      (?0 (elopher-format-record display-string "T" "gray"
                                 (elopher-make-getter 'elopher-get-text address)
                                 (elopher-make-help address)))
      (?1 (elopher-format-record display-string "/" "cyan"
                                 (elopher-make-getter 'elopher-get-index address)
                                 (elopher-make-help address))))))

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
