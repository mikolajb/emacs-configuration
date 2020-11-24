(add-hook 'after-save-hook '(lambda () (princ (event-json))))
(add-hook 'find-file-hook
          '(lambda ()
             (setq-local git-remote-url (car (split-string (shell-command-to-string "git config --get remote.origin.url"))))))

(defun event-json ()
  (let ((result (list
    (cons 'hostname (system-name))
    (cons 'editor "emacs")
    (cons 'username user-login-name)
    (cons 'editor_version emacs-version)
    (cons 'file_name (buffer-file-name))
    (cons 'file_size (buffer-size))
    (cons 'editor_mode (symbol-name major-mode))
    (cons 'timezone (car (cdr (current-time-zone))))
    (cons 'system_type (symbol-name system-type))
    (cons 'package_manager (symbol-name system-packages-package-manager))
    (cons 'kernel_version linux-kernel-version)
    (cons 'timestamp (string-to-number (format-time-string "%s%N"))))))
    (when (boundp 'git-remote-url)
      (setq result (append (list (cons 'git_remote_url git-remote-url)) result)))
    (shell-command (format "curl --data '%s' localhost:13591" (json-serialize result)))))

(provide 'events)
