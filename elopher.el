;;; elopher.el --- gopher client

;;; Commentary:

;; Simple gopher client in elisp.

;;; Code:

;; (defvar elopher-mode-map nil "Keymap for gopher client.")
;; (define-key elopher-mode-map (kbd "p") 'elopher-quit)

;; (define-derived-mode elopher-mode special-mode "elopher"
;;   "Major mode for elopher, an elisp gopher client.")

(defun )

(defun elopher-filter (proc string)
  (with-current-buffer (get-buffer "*elopher*")
    (let ((marker (process-mark proc)))
      (if (not (marker-position marker))
          (set-marker marker 0 (current-buffer)))
      (save-excursion
        (goto-char marker)
        (insert (propertize string 'face '(foreground-color . "magenta")))
        (set-marker marker (point))))))
    
(defun elopher-get-index (host &optional port path)
  (switch-to-buffer-other-window "*elopher*")
  (erase-buffer)
  (make-network-process
   :name "elopher-process"
   :host host
   :service (if port port 70)
   :filter #'elopher-filter)
  (process-send-string "elopher-process" (format "%s\n" (if path path ""))))

(defun elopher ()
  "Start gopher client."
  (interactive)
  (elopher-get-index (read-from-minibuffer "Gopher host: ") 70))

(elopher-get-index "cosmic.voyage")

(format "%s\n" nil)

(delete-process "elopher-process")
  
  ;; (address (read-from-minibuffer "Address of gopher server: ")))
  ;; (message "Connecting to '%s' ..." address)
  ;; (erase-buffer)
  ;; (insert (propertize "Hello, world." 'face '(foreground-color . "red")))
  ;; (newline)
  ;; (insert (propertize "Hello, Tim." 'face '(foreground-color . "yellow"))))

(defun elopher-quit ()
  (interactive)
  (kill-buffer "*elopher*"))

(start-process "ls" "*elopher*" "/bin/date")

;;; elopher.el ends here

(list-processes)
