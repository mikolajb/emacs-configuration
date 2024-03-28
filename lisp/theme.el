(setq my-theme-dark 'gruvbox-dark-hard) ;;doom-one-light
(setq my-theme-light 'gruvbox-light-hard) ;;dracula
(setq my-default-font "Comic Code Ligatures")

(defun load-my-theme (&optional dark)
  "Return right theme basing on a given argument."
  (set-frame-font my-default-font nil t)
  (set-face-attribute 'default nil :height 125)
  (if dark
    (progn
      (load-theme my-theme-dark)
      (disable-theme my-theme-light))
    (load-theme my-theme-light)
    (disable-theme my-theme-dark)))

(defun handle-theme-change-linux (where what content)
  "Handle gnome theme changes."
  (message "Received an event %s %s %s" where what content)
  (if (and (string= where "org.gnome.desktop.interface") (string= what "color-scheme"))
      (let ((theme (car content)))
        (message "Detected theme change to %s" theme)
        (load-my-theme (string= theme "prefer-dark")))))

(defun load-appropriate-theme-linux ()
  (load-my-theme
   (string=
    (substring (string-trim (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme")) 1 -1)
    "prefer-dark")))

(defun load-appropriate-theme-macos ()
  (load-my-theme
   (string=
    (string-trim (shell-command-to-string "dark-mode status"))
    "on")))

(unless (string= system-type "darwin")
  (require 'dbus)
  (dbus-register-signal
   :session nil "/org/freedesktop/portal/desktop" "org.freedesktop.portal.Settings" "SettingChanged" #'handle-theme-change-linux))

(use-package dracula-theme
  :straight (dracula-theme :type git :host github :repo "mikolajb/emacs-dracula-theme"))

(use-package doom-themes
  :straight (doom-themes :type git :host github :repo "mikolajb/emacs-doom-themes" :files (:defaults "themes"))
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(use-package gruvbox-theme
  :config
  (setq gruvbox-bold-constructs t))

(if (string= system-type "darwin")
    (load-appropriate-theme-macos)
    (load-appropriate-theme-linux))

(pixel-scroll-mode 1)
(blink-cursor-mode -1)

(defun toggle-fullscreen ()
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (if (equal 'fullboth current-value)
        (set-face-attribute 'default nil :height 110))
    (toggle-frame-fullscreen)
    (if (not (equal 'fullboth current-value))
        (set-face-attribute 'default nil :height 125))))

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

(custom-set-faces
 '(fixed-pitch ((t (:family "Comic Code Ligatures"))))
 '(fixed-pitch-serif ((t (:family "Comic Code Ligatures"))))
 '(variable-pitch ((t (:family "Bookerly")))))

(provide 'theme)
