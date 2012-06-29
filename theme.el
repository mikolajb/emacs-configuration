(load-theme 'tango-dark)

(defun select-cursor-color-according-to-mode ()
  (blink-cursor-mode 0)
  (cond
   (buffer-read-only
    (set-cursor-color "DodgerBlue"))
   (overwrite-mode
    (set-cursor-color "orange"))
   ((buffer-modified-p)
    (set-cursor-color "#D4213D"))
   (t
    (set-cursor-color "#E9E8E7"))))
(add-hook 'post-command-hook 'select-cursor-color-according-to-mode)

;;; terminal colors
(setq ansi-color-for-comint-mode t)
(setq ansi-color-for-comint-mode-on t)

(defface term-color-black
  '((t :foreground "#555753" :background "#555753"))
  "Face used to render black color code."
  :group 'term)

(defface term-color-red
  '((t :foreground "#EF2929" :background "#EF2929"))
  "Face used to render red color code."
  :group 'term)

(defface term-color-green
  '((t :foreground "#8AE234" :background "#8AE234"))
  "Face used to render green color code."
  :group 'term)

(defface term-color-yellow
  '((t :foreground "#FCE94F" :background "#FCE94F"))
  "Face used to render yellow color code."
  :group 'term)

(defface term-color-blue
  '((t :foreground "#729FCF" :background "#729FCF"))
  "Face used to render blue color code."
  :group 'term)

(defface term-color-magenta
  '((t :foreground "#AD7FA8" :background "#AD7FA8"))
  "Face used to render magenta color code."
  :group 'term)

(defface term-color-cyan
  '((t :foreground "#34E2E2" :background "#34E2E2"))
  "Face used to render cyan color code."
  :group 'term)

(defface term-color-white
  '((t :foreground "#EEEEEC" :background "#EEEEEC"))
  "Face used to render white color code."
  :group 'term)

(provide 'theme)
