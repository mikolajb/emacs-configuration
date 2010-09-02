
;; Set encoding to UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Frame title : set to buffer name
(setq frame-title-format "Emacs - %f ")
(setq icon-title-format "Emacs - %b")

(when (>= emacs-major-version 23)
  (add-to-list 'default-frame-alist '(font . "Droid Sans Mono Slashed-10")))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; Desktops
(desktop-save-mode 1)

;; automatycznie wykonywalny:
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(column-number-mode t)
(line-number-mode t)
(mouse-wheel-mode t)
(transient-mark-mode t)
; stops selection with a mouse being immediately injected to the kill ring
(setq mouse-drag-copy-region nil)
; stops killing/yanking interacting with primary X11 selection
(setq x-select-enable-primary nil)
; makes killing/yanking interact with clipboard X11 selection
(setq x-select-enable-clipboard t)
(setq show-trailing-whitespace t)
; delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;;; Electric minibuffer!
;;; When selecting a file to visit, // will mean / and
;;; ~ will mean $HOME regardless of preceding text.
(setq file-name-shadow-tty-properties '(invisible t))
(file-name-shadow-mode 1)

;;; Z KREDITORA
;;; Annoying stuff
(setq visible-bell t)
(setq inhibit-startup-echo-area-message t
      inhibit-startup-message           t)
(scroll-bar-mode nil)
(tool-bar-mode 0)
(menu-bar-mode 0)
;; Don't insert instructions in the *scratch* buffer
(setq initial-scratch-message nil)
(provide 'global)
