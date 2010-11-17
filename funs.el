(defun pretty-print-xml-region (begin end)
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))

;; (global-set-key (kbd "C-c s") 'swap-windows)

;; source: http://blog.tuxicity.se/elisp/emacs/2010/03/26/rename-file-and-buffer-in-emacs.html
(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))
;; (global-set-key (kbd "C-c r") 'rename-file-and-buffer)

(defun duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the original"
  (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
    (comment-region (region-beginning) (region-end)))
    (insert-string
      (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))

;; or choose some better bindings....

;; duplicate a line
(global-set-key (kbd "C-c y") 'duplicate-line)

;; duplicate a line and comment the first
(global-set-key (kbd "C-c d") (lambda()(interactive)(duplicate-line t)))

;;; inserts date
(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun switch-to-term-buffer ()
  "Switch to ansi term buffer."
  (interactive)
  (setq terms
	(delq nil (mapcar (lambda (x)
			    (setq n (buffer-name x))
			    (if (string-match "\\*terminal\\|ansi-term\\|e?shell\\*" n)
				n
			      nil))
			  (delq (current-buffer) (buffer-list)))))
  (if terms
      (switch-to-buffer (completing-read "select terminal: " terms nil t nil nil (car terms)))
    (princ "no terminal buffers")))

(defun run-ansi-term ()
  "Runs ansi term."
  (interactive)
  (ansi-term "/bin/bash")
  ;; (text-scale-decrease 1)
  )

(defun run-shell ()
  "Runs new shell with uniq name."
  (interactive)
  (shell (generate-new-buffer-name "*shell*")))

(global-set-key (kbd "C-c t") 'switch-to-term-buffer)
(global-set-key (kbd "C-c T") 'run-ansi-term)
(global-set-key (kbd "C-c S") 'run-shell)

(provide 'funs)