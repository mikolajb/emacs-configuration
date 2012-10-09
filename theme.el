(load-theme 'sanityinc-solarized-light)

(defun select-cursor-color-according-to-mode ()
  (blink-cursor-mode 0)
  (cond
   (buffer-read-only
    (set-cursor-color "DodgerBlue"))
   (overwrite-mode
    (set-cursor-color "#E9E8E7"))
   ((buffer-modified-p)
    (set-cursor-color "#D4213D"))
   (t
    (set-cursor-color "#c61b6e"))))
(add-hook 'post-command-hook 'select-cursor-color-according-to-mode)

;;; terminal colors
(setq ansi-color-for-comint-mode t)
(setq ansi-color-for-comint-mode-on t)

(provide 'theme)
