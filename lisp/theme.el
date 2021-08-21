(defun load-theme-basing-on-gnome-setup (gnome-theme)
  "Return right theme basing on given gnome theme name."
  (let ((dark 'dracula)
        (light 'doom-acario-light))
    (cond ((string= gnome-theme "Adwaita")
           (load-theme light)
           (disable-theme dark))
          ((string= gnome-theme "Adwaita-dark")
           (load-theme dark)
           (disable-theme light)))))

(defun handle-theme-change (where what content)
  "Handle gnome theme changes."
  (message "Received an event %s %s %s" where what content)
  (if (and (string= where "org.gnome.desktop.interface") (string= what "gtk-theme"))
      (let ((theme (car content)))
        (message "Detected theme change to %s" theme)
        (load-theme-basing-on-gnome-setup theme))))

(defun load-appropriate-theme ()
  (load-theme-basing-on-gnome-setup (substring (string-trim (shell-command-to-string "gsettings get org.gnome.desktop.interface gtk-theme")) 1 -1)))

(require 'dbus)
(dbus-register-signal
 :session nil "/org/freedesktop/portal/desktop" "org.freedesktop.portal.Settings" "SettingChanged" #'handle-theme-change)

(use-package dracula-theme
  :quelpa (dracula-theme :fetcher github-ssh :repo "mikolajb/emacs-dracula-theme"))

(use-package doom-themes
  :quelpa (doom-themes :fetcher github-ssh :repo "mikolajb/emacs-doom-themes" :files (:defaults "themes"))
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(load-appropriate-theme)

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
          #'(lambda ()
              (set-fontset-font t 'unicode "Segoe UI Symbol" nil 'prepend)))

(defun my-latex-mode-faces ()
  "Set a face for a buffer."
  (face-remap-set-base
   'default '(:height 160))
  (face-remap-set-base
   'default '(:family "iA Writer Duospace")))
(when (boundp 'latex-editor)
  (add-hook 'LaTeX-mode-hook 'my-latex-mode-faces))

(provide 'theme)
