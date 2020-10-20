(add-hook 'after-save-hook '(lambda () (princ (event-json))))
(add-hook 'find-file-hook
          '(lambda ()
             (setq-local git-remote-url (car (split-string (shell-command-to-string "git config --get remote.origin.url"))))))

(defun event-json ()
  (let ((result (list
    (cons 'hostname (system-name))
    (cons 'editor "emacs")
    (cons 'emacs-version emacs-version)
    (cons 'file-name (buffer-file-name))
    (cons 'file-size (buffer-size))
    (cons 'emacs-major-mode (symbol-name major-mode))
    (cons 'timezone (car (cdr (current-time-zone))))
    (cons 'system-type (symbol-name system-type))
    (cons 'package-manager (symbol-name system-packages-package-manager))
    (cons 'linux-kernel-version linux-kernel-version)
    (cons 'timestamp (format-time-string "%s%N")))))
    (when (boundp 'git-remote-url)
      (setq result (append (list (cons 'git-origin-url git-remote-url)) result)))
    (shell-command (format "~/.emacs.d/record-event '%s' >> /tmp/test.txt" (json-serialize result)))))

(provide 'events)
