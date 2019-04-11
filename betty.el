;;; betty.el --- gopher client

;;; Commentary:

;; Simple gopher client in elisp.

;;; Code:

(defvar betty-mode-map nil "Keymap for gopher client.")
(define-key betty-mode-map (kbd "p") 'betty-quit)

(define-derived-mode betty-mode special-mode "betty"
  "Major mode for betty, an elisp gopher client.

  \\{betty-mode-map}")

(defun betty ()
  "Start gopher client."
  (interactive)
  (switch-to-buffer "*betty*")
  (betty-mode)
  (let ((inhibit-read-only t)
        (make-process
         :name "ls"
         :command "ls -l"
         :filter))
         
        ;; (address (read-from-minibuffer "Address of gopher server: ")))
    (message "Connecting to '%s' ..." address)
    (erase-buffer)
    (insert (propertize "Hello, world." 'face '(foreground-color . "red")))
    (newline)
    (insert (propertize "Hello, Tim." 'face '(foreground-color . "yellow")))))

(defun betty-quit ()
  (interactive)
  (kill-buffer "*betty*"))


;;; betty.el ends here
