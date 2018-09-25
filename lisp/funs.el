(unless (boundp 'latex-editor)
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

  ;; source: http://rawsyntax.com/post/6756157015/learn-emacs-delete-file-and-buffer
  (defun delete-file-and-buffer ()
    "Deletes the current file and buffer, assumes file exists"
    (interactive)
    (move-file-to-trash buffer-file-name)
    (kill-buffer (buffer-name)))

  (defun duplicate-line (&optional commentfirst)
    "comment line at point; if COMMENTFIRST is non-nil, comment the original"
    (interactive)
    (beginning-of-line)
    (push-mark)
    (end-of-line)
    (let ((str (buffer-substring (region-beginning) (region-end))))
      (when commentfirst
        (comment-region (region-beginning) (region-end)))
      (insert
       (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
      (forward-line -1)))

  ;; or choose some better bindings....

  ;; duplicate a line
  (global-set-key (kbd "C-c D") 'duplicate-line)

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

(defun helm-source-shell-buffers-list ()
  (helm-make-source "Shell Buffers" 'helm-source-buffers
    :buffer-list
    (lambda ()
      (mapcar #'buffer-name
              (cl-remove-if-not
               (lambda (buf)
                 (with-current-buffer buf
                   (eq major-mode 'term-mode)))
               (buffer-list))))))

(defun helm-shell-buffers-list ()
  (interactive)
  (helm :sources (helm-source-shell-buffers-list)
        :buffer "*shells*"
        :keymap helm-buffer-map
        :truncate-lines helm-buffers-truncate-lines))

  (defun run-ansi-term ()
    "Runs ansi term."
    (interactive)
    (ansi-term "/usr/bin/zsh"))

  (defun run-shell ()
    "Runs new shell with uniq name."
    (interactive)
    (shell (generate-new-buffer-name "*shell*")))

  (global-set-key (kbd "C-c t") 'helm-shell-buffers-list)
  (global-set-key (kbd "C-c T") 'run-ansi-term)
  (global-set-key (kbd "C-c S") 'run-shell)

  (defun shell-on-top ()
    "Puts last used shell on top and restores the previous window configuration."
    (interactive)
    (if (member (with-current-buffer (current-buffer) major-mode)
                '(eshell-mode shell-mode term-mode))
        (jump-to-register ?1)

      (window-configuration-to-register ?1)
      (setq term-buffer
            (car (delq nil (mapcar
                            (lambda (x)
                              (setq n (buffer-name x))
                              (if (string-match "\\*terminal\\|ansi-term\\|e?shell\\*" n)
                                  n
                                nil))
                            (delq (current-buffer) (buffer-list))))))
      (if term-buffer (switch-to-buffer term-buffer) (run-ansi-term))
      (delete-other-windows)))

  (global-set-key [f12] 'shell-on-top)

  (defun rename-term-buffer ()
    "renames terminal buffer"
    (interactive)
    (unless (member major-mode '(eshell-mode shell-mode term-mode))
      (signal 'quit '("Not a terminal buffer")))
    (setq buffer_names '((eshell-mode . "eshell") (shell-mode . "shell") (term-mode . "ansi-term")))
    (setq new_name_part (read-from-minibuffer "name> "))
    (rename-buffer (generate-new-buffer-name
                    (concat "*"
                            (cdr (assoc major-mode buffer_names))
                            "-"
                            new_name_part
                            "*")))
    )

  (add-hook 'term-mode-hook
            (lambda ()
              (define-key term-raw-map (kbd "C-c n") 'rename-term-buffer)
              (define-key term-mode-map (kbd "C-c n") 'rename-term-buffer)))

  (add-hook 'shell-mode-hook
            (lambda ()
              (define-key shell-mode-map (kbd "C-c n") 'rename-term-buffer)))

  (defun set-encoding-in-comment ()
    "Sets coding"
    (interactive)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "[^\0-\177]" nil t)
        (goto-char (point-min))
        (let ((coding-system
               (or coding-system-for-write
                   buffer-file-coding-system)))
          (if coding-system
              (setq coding-system
                    (or (coding-system-get coding-system 'mime-charset)
                        (coding-system-change-eol-conversion coding-system nil))))
          (setq coding-system
                (if coding-system
                    (symbol-name coding-system)
                  "ascii-8bit"))
          (if (looking-at "^#!") (beginning-of-line 2))
          (cond ((looking-at "\\s *#.*-\*-\\s *\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)\\s *\\(;\\|-\*-\\)")
                 (unless (string= (match-string 2) coding-system)
                   (goto-char (match-beginning 2))
                   (delete-region (point) (match-end 2))
                   (and (looking-at "-\*-")
                        (let ((n (skip-chars-backward " ")))
                          (cond ((= n 0) (insert "  ") (backward-char))
                                ((= n -1) (insert " "))
                                ((forward-char)))))
                   (insert coding-system)))
                ((looking-at "\\s *#.*coding\\s *[:=]"))
                (t (insert "# -*- coding: " coding-system " -*-\n"))
                )))))

  (defun tramp-cleanup-star ()
    "Cleanup all connections and buffers"
    (interactive)
    (tramp-cleanup-all-connections)
    (tramp-cleanup-all-buffers))

  (defun astyle-beautify-region ()
    "Astyle region"
    (interactive)
    (let ((cmd "astyle --style=1tbs --pad-oper --pad-header --unpad-paren --indent=spaces=2 --break-blocks"))
      (shell-command-on-region (region-beginning) (region-end) cmd (current-buffer) t)))

  (defun beautify-json ()
    (interactive)
    (let ((b (if mark-active (min (point) (mark)) (point-min)))
          (e (if mark-active (max (point) (mark)) (point-max))))
      (shell-command-on-region b e
                               "python -mjson.tool" (current-buffer) t))))

(defun insert-date ()
  "Insert current date."
  (interactive)
  (let ((system-time-locale "en_US"))
  (insert (format-time-string "%e-%b-%Y"))))

(global-set-key (kbd "M-<up>") 'move-region-up)
(global-set-key (kbd "M-<down>") 'move-region-down)

(defun camelcase-to-underscores ()
  "Replace camelcase with underscore formatting."
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (replace-regexp "\\([A-Z]\\)" "_\\1" nil
                      (1+ (car bounds)) (cdr bounds))
      (downcase-region (car bounds) (cdr bounds)))))

(defun getbibtext (doi)
  "Get bibtex of given DOI."
  (interactive)
  (request (concat "http://dx.doi.org/" doi)
           :headers '(("Accept" . "text/bibliography; ; style=bibtex"))
           :success (function* (lambda (&key data &allow-other-keys) data))))

(defun region-insert-sapce-every-4 ()
  ""
  (interactive)
  (save-restriction
    (narrow-to-region (region-beginning) (region-end))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\([^ ]\\) *\\([^ ]\\) *\\([^ ]\\) *\\([^ ]\\) *" nil t)
        (replace-match "\\1\\2\\3\\4 ")
        )
      )
    )
  )


;;; Insert Random Number
(defun xah-insert-random-number (NUM)
  "Insert NUM random digits.
NUM default to 5.
Call `universal-argument' before for different count.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
Version 2017-05-24"
  (interactive "P")
  (let (($charset "1234567890" )
        ($baseCount 10))
    (dotimes (_ (if (numberp NUM) (abs NUM) 5 ))
      (insert (elt $charset (random $baseCount))))))

(provide 'funs)


;;; Increment number decimal
(defun increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))
