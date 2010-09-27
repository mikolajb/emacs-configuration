(require 'color-theme)
(color-theme-initialize)

;; (load "~/.emacs.d/plugins/color-theme-irblack/color-theme-irblack")
;; (color-theme-irblack)s

(load "~/.emacs.d/plugins/twilight-emacs/color-theme-twilight.el")
(color-theme-twilight)

;; Change cursor color according to mode
(defvar hcz-set-cursor-color-color "")
(defvar hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
	 (if buffer-read-only "white"
	   (if overwrite-mode "red"
	     "yellow green"))))
    (unless (and
	     (string= color hcz-set-cursor-color-color)
	     (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)

(provide 'theme)