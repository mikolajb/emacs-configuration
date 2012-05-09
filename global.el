;; Set encoding to UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Frame title : set to buffer name
(setq frame-title-format '("" invocation-name ": "(:eval (if (buffer-file-name)
							     (abbreviate-file-name (buffer-file-name))
							   "%b"))))

;; when minimized
(setq icon-title-format '("" invocation-name ": %b"))

;;; Default frame font and size
(when (>= emacs-major-version 23)
  ;; (add-to-list 'default-frame-alist '(font . "Consolas-11"))
  (add-to-list 'default-frame-alist '(font . "Cousine-10"))
  (add-to-list 'default-frame-alist '(height . 32))
  (add-to-list 'default-frame-alist '(width . 120)))

(show-paren-mode 1)
(setq show-paren-delay 0)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; Desktops
(setq desktop-restore-eager 20
      desktop-lazy-verbose nil)
(desktop-save-mode 1)

;;; desktop-override-stale-locks begins here
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
      (when (file-exists-p cmdline-file)
	(with-temp-buffer
	  (insert-file-contents-literally cmdline-file)
	  (goto-char (point-min))
	  (search-forward "emacs" nil t)
	  pid)))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))
;;; desktop-override-stale-locks ends here

;;; auto revert modified files
(global-auto-revert-mode t)

;;; savehist
(savehist-mode 1)

;; automatycznie wykonywalny:
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(column-number-mode t)
(line-number-mode t)
(mouse-wheel-mode t)
(transient-mark-mode t)
; stops selection with a mouse being immediately injected to the kill ring
(setq mouse-drag-copy-region nil)

;; (when (<= emacs-major-version 23)
;; stops killing/yanking interacting with primary X11 selection
(setq x-select-enable-primary nil)
;; makes killing/yanking interact with clipboard X11 selection
(setq x-select-enable-clipboard t)
;; )

; delete trailing whitespace before save
(setq whitespace-style '(trailing))
(add-hook 'before-save-hook 'whitespace-cleanup)
;; make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)
;; smooth scrolling
(setq scroll-step 1
      auto-window-vscroll nil
      scroll-conservatively 10000
      scroll-preserve-screen-position 'tak
      mouse-wheel-follow-mouse 't
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1)))
; allows scrolling during isearch
(put 'view-lossage 'isearch-scroll t)
;; kills whole line, including following newline
(setq kill-whole-line t)

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
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
;; Don't insert instructions in the *scratch* buffer
(setq initial-scratch-message nil)

;;; move to trash instead of deleting
(setq delete-by-moving-to-trash t)

(require 'saveplace)
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)

;;; ESHELL
(setq eshell-prefer-lisp-functions t)

;;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; comint env to work with node.js
(setenv "NODE_NO_READLINE" "1")

;;; enable narrowing
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; re-builder syntax choice
(setq reb-re-syntax 'string)

;;; fly-spell in comments
;(add-hook ‘prog-mode-hook ‘flyspell-prog-mode)

;;; midnight mode
(require 'midnight)
(setq midnight-period 3600)

;;; prelude
(size-indication-mode t)
;; Death to the tabs!
(setq-default indent-tabs-mode nil)
;; delete the selection with a keypress
(delete-selection-mode t)
(setq temporary-file-directory "~/.emacs.d/backups/")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; use shift + arrow keys to switch between visible buffers
(windmove-default-keybindings 'meta)
;; flyspell-mode does spell-checking on the fly as you type
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

(provide 'global)
