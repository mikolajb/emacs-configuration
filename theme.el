(require 'color-theme)
(color-theme-initialize)

(load "~/.emacs.d/plugins/color-theme-irblack/color-theme-irblack")
(color-theme-irblack)

(defun select-cursor-color-according-to-mode ()
  (blink-cursor-mode 0)
  (cond
   (buffer-read-only
    (set-cursor-color "gold"))
   (overwrite-mode
    (set-cursor-color "magenta3"))
   (t
    (set-cursor-color "#D4213D"))))
(add-hook 'post-command-hook 'select-cursor-color-according-to-mode)

(provide 'theme)