;;; elopher.el --- gopher client

;;; Commentary:

;; Simple gopher client in elisp.

;;; Code:

;; (defvar elopher-mode-map nil "Keymap for gopher client.")
;; (define-key elopher-mode-map (kbd "p") 'elopher-quit)

;; (define-derived-mode elopher-mode special-mode "elopher"
;;   "Major mode for elopher, an elisp gopher client.")

(defvar my-marker nil)

(defun my-filter (proc string)
  (with-current-buffer (get-buffer "*elopher*")
    (let ((marker (process-mark proc)))
      (if (not (marker-position marker))
          (set-marker marker 0 (current-buffer)))
      (save-excursion
        (goto-char marker)
        (insert (propertize string 'face '(foreground-color . "yellow")))
        (set-marker marker (point))))))
    

(defun elopher ()
  "Start gopher client."
  (interactive)
  (switch-to-buffer-other-window "*elopher*")
  (setq my-marker (make-marker))
  (erase-buffer)
  (make-process
   :name "date"
   :command (list "/bin/ls" "-l" "/")
   :filter #'my-filter))

(elopher)
  
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
