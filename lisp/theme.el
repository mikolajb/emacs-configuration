(defun load-theme-basing-on-gnome-setup (gnome-theme)
  "Return right theme basing on given gnome theme name."
  (let ((dark 'dracula)
        (light 'doom-one-light))
    (cond ((or (string= gnome-theme "default") (string= gnome-theme "prefer-light"))
           (load-theme light)
           (disable-theme dark))
          ((string= gnome-theme "prefer-dark")
           (load-theme dark)
           (disable-theme light)))))

(defun handle-theme-change (where what content)
  "Handle gnome theme changes."
  (message "Received an event %s %s %s" where what content)
  (if (and (string= where "org.gnome.desktop.interface") (string= what "color-scheme"))
      (let ((theme (car content)))
        (message "Detected theme change to %s" theme)
        (load-theme-basing-on-gnome-setup theme))))

(defun load-appropriate-theme ()
  (load-theme-basing-on-gnome-setup (substring (string-trim (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme")) 1 -1)))

(unless (string= system-type "darwin")
  (require 'dbus)
  (dbus-register-signal
   :session nil "/org/freedesktop/portal/desktop" "org.freedesktop.portal.Settings" "SettingChanged" #'handle-theme-change))

(use-package dracula-theme
  :straight (dracula-theme :type git :host github :repo "mikolajb/emacs-dracula-theme"))

(use-package doom-themes
  :straight (doom-themes :type git :host github :repo "mikolajb/emacs-doom-themes" :files (:defaults "themes"))
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(if (string= system-type "darwin")
    (progn
      (load-theme 'dracula)
      (set-frame-font "Comic Code Ligatures-13" nil t))
    (load-appropriate-theme))

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

(custom-set-faces
 '(variable-pitch ((t (:family "Museo")))))

(defun my-latex-mode-faces ()
  "Set a face for a buffer."
  (face-remap-set-base
   'default '(:height 160))
  (face-remap-set-base
   'default '(:family "iA Writer Duospace")))
(when (boundp 'latex-editor)
  (add-hook 'LaTeX-mode-hook 'my-latex-mode-faces))

(provide 'theme)
