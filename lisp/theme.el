(load-theme 'zenburn)
(blink-cursor-mode -1)

(set-face-attribute 'default nil :height 125)

(defun toggle-fullscreen ()
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (if (equal 'fullboth current-value)
        (set-face-attribute 'default nil :height 125))
    (toggle-frame-fullscreen)
    (if (not (equal 'fullboth current-value))
        (set-face-attribute 'default nil :height 140))))

(global-set-key [f11] 'toggle-fullscreen)

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
;; (add-hook 'post-command-hook 'select-cursor-color-according-to-mode)

;;; terminal colors
(setq ansi-color-for-comint-mode t)
(setq ansi-color-for-comint-mode-on t)

(custom-set-faces
 '(variable-pitch ((t (:family "Corbel")))))

(provide 'theme)
