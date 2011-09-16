(require 'color-theme)
(color-theme-initialize)

(add-to-list 'load-path "~/.emacs.d/plugins/color-theme-solarized")
(require 'color-theme-solarized)
(color-theme-solarized 'light)

(defun select-cursor-color-according-to-mode ()
  (blink-cursor-mode 0)
  (cond
   (buffer-read-only
    (set-cursor-color "blue"))
   (overwrite-mode
    (set-cursor-color "orange"))
   (t
    (set-cursor-color "#D4213D"))))
(add-hook 'post-command-hook 'select-cursor-color-according-to-mode)

(provide 'theme)
