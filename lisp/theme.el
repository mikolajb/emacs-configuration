(use-package dracula-theme
  :init
  (add-to-list 'custom-theme-load-path "~/.emacs.d/plugins/dracula-theme/")
  ;; (load-theme 'dracula t)
  )

(use-package solarized-theme
  :ensure t
  :custom
  (solarized-use-variable-pitch nil)
  (solarized-use-more-italic t)
  (solarized-scale-markdown-headlines t))

(pixel-scroll-mode 1)
(blink-cursor-mode -1)

(set-face-attribute 'default nil :height 125)

(defun toggle-fullscreen ()
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (if (equal 'fullboth current-value)
        (set-face-attribute 'default nil :height 105))
    (toggle-frame-fullscreen)
    (if (not (equal 'fullboth current-value))
        (set-face-attribute 'default nil :height 120))))

(global-set-key (kbd "C-x <up>") 'toggle-fullscreen)

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

;; font for all unicode characters
(set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)
(set-fontset-font t 'unicode "Noto Sans Symbols" nil 'prepend)
(set-fontset-font t 'unicode "Noto Sans Display" nil 'prepend)
(set-fontset-font t 'unicode "Symbols Nerd Font" nil 'prepend)

(custom-set-faces
 '(variable-pitch ((t (:family "Museo")))))

;;; hack to make emoji be visible when running as a daemon
(add-hook 'before-make-frame-hook
          '(lambda ()
             (set-fontset-font t 'unicode "Segoe UI Symbol" nil 'prepend)))

(defun my-latex-mode-faces ()
  "Set a face for a buffer."
  (face-remap-set-base
   'default '(:height 160))
  (face-remap-set-base
   'default '(:family "iA Writer Duospace")))
(when (boundp 'latex-editor)
  (add-hook 'LaTeX-mode-hook 'my-latex-mode-faces))

(defun handle-theme-change (where what content)
  "Handle gnome theme changes."
  (message "Received an event %s %s %s" where what content)
  (if (and (string= where "org.gnome.desktop.interface") (string= what "gtk-theme"))
      (let ((theme (car content)))
        (message "Detected theme change to %s" theme)
        (cond ((string= theme "Adwaita") (load-theme 'solarized-gruvbox-light))
              ((string= theme "Adwaita-dark") (load-theme 'solarized-gruvbox-dark))))))

(require 'dbus)
(dbus-register-signal
 :session nil "/org/freedesktop/portal/desktop" "org.freedesktop.portal.Settings" "SettingChanged" #'handle-theme-change)

(provide 'theme)
