(when (string= system-type "darwin")
  (setenv "PINENTRY_PROGRAM" "pinentry-mac")
  (setq dired-use-ls-dired nil))

;; Set encoding to UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; open a list of a recently opened files in a initial buffer
(recentf-mode 1)
(setq initial-buffer-choice
      (let (defaultval)
        (condition-case ex
            (setq defaultval 'recentf-open-files)
          ('error
           (setq defaultval t)))
        defaultval))
(add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/.*" (getenv "HOME")))
(add-hook 'text-mode-hook 'flyspell-mode)

;; Frame title : set to buffer name
(setq frame-title-format '("" invocation-name ": "(:eval (if (buffer-file-name)
                                                             (abbreviate-file-name (buffer-file-name))
                                                           "%b"))))

;; when minimized
(setq icon-title-format '("" invocation-name ": %b"))

;;; Default frame font and size
(if (boundp 'latex-editor)
    (progn
      (add-to-list 'default-frame-alist '(height . 24))
      (add-to-list 'default-frame-alist '(width . 100)))
  (add-to-list 'default-frame-alist '(height . 28))
  (add-to-list 'default-frame-alist '(width . 110)))

(show-paren-mode 1)
(setq show-paren-delay 0)
(if (boundp 'latex-editor)
    (setq custom-file "~/.emacs-latex.d/custom.el")
    (setq custom-file "~/.emacs.d/custom.el"))
(load custom-file 'noerror)

;;; auto revert modified files
(global-auto-revert-mode t)
(when (string= system-type "darwin")
  (setq auto-revert-use-notify nil))

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

(add-hook 'prog-mode-hook 'whitespace-mode)
(setq whitespace-style '(trailing empty missing-newline-at-eof face indentation space-after-tab space-before-tab))
;; delete trailing whitespace before save
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

;; C-n insert newlines if the point is at the end of the buffer
(setq next-line-add-newlines t)

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
(setq initial-major-mode 'org-mode)

;;; move to trash instead of deleting
(setq delete-by-moving-to-trash t)

;;; enable narrowing
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; re-builder syntax choice
(setq reb-re-syntax 'string)

;;; fly-spell in comments
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;; from prelude
(size-indication-mode t)
;; Death to the tabs!
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-always-indent 'complete)
;; delete the selection with a keypress
(delete-selection-mode t)
;; (if (boundp 'latex-editor)
;;     (setq temporary-file-directory "~/.emacs-latex.d/backups/")
;;     (setq temporary-file-directory "~/.emacs.d/backups/"))
;; (setq backup-directory-alist
;;       `((".*" . ,temporary-file-directory)))
;; (setq auto-save-file-name-transforms
;;       `((".*" ,temporary-file-directory t)))
;; use shift + arrow keys to switch between visible buffers
(windmove-default-keybindings 'meta)
;; flyspell-mode does spell-checking on the fly as you type
(setq ispell-program-name "aspell") ; use aspell instead of ispell
(if (boundp 'latex-editor)
  (setq ispell-extra-args '("--sug-mode=ultra"))
  (setq ispell-extra-args '("--sug-mode=ultra" "--run-together" "--run-together-limit=2" "--run-together-min=2")))

;;; from technomacy/better-defaults
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; enable downcase-region command C-x C-l
(put 'downcase-region 'disabled nil)

;;; bookmarks
(setq bookmark-save-flag 1)

;;; Else, every emacs restart, the sequence of random will be the same:
(random t) ; seed random number

;;; auth-sources
(setq auth-sources '("secrets:Login"))

;;; show battery status
(display-battery-mode t)

;;; make sure old byte code won't be loaded
(setq load-prefer-newer t)

;;; parentheses pairing mode
(electric-pair-mode t)

(provide 'global)
