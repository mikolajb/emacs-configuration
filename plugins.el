;;; dired-plus
(add-to-list 'load-path "~/.emacs.d/plugins/dired-plus")
(require 'dired+)
(toggle-diredp-find-file-reuse-dir 1)

;;; icicles
(add-to-list 'load-path "~/.emacs.d/plugins/icicles")
(require 'icicles)
(setq icicle-buffer-sort 'icicle-most-recent-first-p)
(setq icicle-saved-completion-sets
      '("projekty" "~/projekty"))
(icy-mode)

;;; predictive: http://www.emacswiki.org/emacs/PredictiveMode (needed by nXhtml)
(add-to-list 'load-path "~/.emacs.d/plugins/predictive")
(autoload 'predictive-mode "predictive" "predictive" t)
(set-default 'predictive-auto-add-to-dict t)
(setq predictive-main-dict 'rpg-dictionary
      predictive-auto-learn t
      predictive-add-to-dict-ask nil
      predictive-use-auto-learn-cache nil
      predictive-which-dict t)

;;; nXhtml: http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html
;; (load "~/.emacs.d/plugins/nxhtml/autostart.el")
;; (add-to-list 'auto-mode-alist '("\\.html\.erb$" . eruby-html-mumamo-mode))
;; (sml-modeline-mode 1)
;; ;; yas in nxhtml
;; (yas/define-snippets 'nxhtml-mode nil 'html-mode)

;;; Textile: http://dev.nozav.org/textile-mode.html
(add-to-list 'load-path "~/.emacs.d/plugins/textile-mode")
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;; magit
(add-to-list 'load-path "~/.emacs.d/plugins/magit")
(require 'magit)
(add-hook 'magit-log-edit-mode-hook 'flyspell-mode)
(add-hook 'magit-log-edit-mode-hook 'auto-fill-mode)

;;; cucumber
(add-to-list 'load-path "~/.emacs.d/plugins/cucumber")
(require 'feature-mode)

;;; autopair
(add-to-list 'load-path "~/.emacs.d/plugins/autopair")
(require 'autopair)
(autopair-global-mode)
(add-hook 'term-mode-hook
	  #'(lambda () (autopair-mode -1)))
(setq autopair-autowrap t)
(setq autopair-blink-delay 0.05)

;;; scratch-el
(add-to-list 'load-path "~/.emacs.d/plugins/scratch-el")
(require 'scratch)

;;; edit-server
(load-file "~/.emacs.d/plugins/emacs_chrome/servers/edit-server.el")
(require 'edit-server)
;; (setq edit-server-new-frame nil)
;; (add-hook 'edit-server-done-hook 'on-edit-server-done-do-backup)
(edit-server-start)

;;; sencodary selection
(add-to-list 'load-path "~/.emacs.d/plugins/second-sel")
(require 'second-sel)
;;; following taken from setup-keys.el
;; Additional definitions for some standard mouse commands:
;; SGI does not pass all ALT-mouse stuff thru to Emacs, so use C-M-mouse also:
(global-set-key [C-M-mouse-1] 'mouse-start-secondary)
(global-set-key [C-M-drag-mouse-1] 'mouse-set-secondary)
(global-set-key [C-M-down-mouse-1] 'mouse-drag-secondary)
(global-set-key [C-M-mouse-3] 'mouse-secondary-save-then-kill)
(global-set-key [C-M-mouse-2] 'mouse-yank-secondary)
(global-set-key [(control meta ?y)] 'secondary-dwim)
(define-key isearch-mode-map "\C-\M-y" 'isearch-yank-secondary)

;;; pretty-mode
(add-to-list 'load-path "~/.emacs.d/plugins/pretty-mode")
(require 'pretty-mode)
(pretty-mode)

;;; full-ack
(add-to-list 'load-path "~/.emacs.d/plugins/full-ack")
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;;; iedit
(add-to-list 'load-path "~/.emacs.d/plugins/iedit")
(require 'iedit)
(setq iedit-only-at-word-boundaries nil)
(define-key global-map (kbd "C-c C-;") 'iedit-mode)

;;; Sawzall
(load-file "~/.emacs.d/plugins/sawzall.el")

;;; Multiple cursors
(add-to-list 'load-path "~/.emacs.d/plugins/multiple-cursors")
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(provide 'plugins)
