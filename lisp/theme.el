(add-to-list 'load-path "~/.emacs.d/plugins/solarized-emacs/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/plugins/solarized-emacs/")

(add-to-list 'load-path "~/.emacs.d/plugins/dracula-theme/emacs/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/plugins/dracula-theme/emacs/")

(if (boundp 'latex-editor)
    (load-theme 'leuven)
    (load-theme 'dracula))
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
    (set-cursor-color "#E9E8E7"))
   (overwrite-mode
    (set-cursor-color "#c61b6e"))
   ;; ((buffer-modified-p)
   ;;  (set-cursor-color "#D4213D"))
   (t
    (set-cursor-color "dodger blue"))))
;; (add-hook 'post-command-hook 'select-cursor-color-according-to-mode)

;;; terminal colors
;; (setq ansi-color-for-comint-mode t)
;; (setq ansi-color-for-comint-mode-on t)

;; font for all unicode characters
(set-fontset-font t 'unicode "Segoe UI Symbol" nil 'prepend)

(custom-set-faces
 '(variable-pitch ((t (:family "Roboto Slab")))))

;;; hack to make emoji be visible when running as a daemon
(add-hook 'before-make-frame-hook
	  '(lambda ()
             (set-fontset-font t 'unicode "Segoe UI Symbol" nil 'prepend)))

(defun my-latex-mode-faces ()
  "Sets a face for a buffer."
  (face-remap-set-base
   'default '(:height 160)))
(when (boundp 'latex-editor)
  (add-hook 'LaTeX-mode-hook 'my-latex-mode-faces))

(provide 'theme)
