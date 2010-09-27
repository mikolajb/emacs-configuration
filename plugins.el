;;; icicles
(add-to-list 'load-path "~/.emacs.d/plugins/icicles")
(require 'icicles)
(icy-mode)

;;; icomplete+ (downloaded from emacswiki)
(add-to-list 'load-path "~/.emacs.d/plugins/icomplete-plus")
(eval-after-load "icomplete" '(progn (require 'icomplete+)))

;;; synonyms (downloaded from emacs wiki)
;; download ftp://ibiblio.org/pub/docs/books/gutenberg/etext02/mthes10.zip
(add-to-list 'load-path "~/.emacs.d/plugins/synonyms")
(setq synonyms-file  "~/.emacs.d/plugins/synonyms/mthesaur.txt")
(setq synonyms-cache-file "~/.emacs.d/synonyms.cache")
(require 'synonyms)

;;; dired-plus
(add-to-list 'load-path "~/.emacs.d/plugins/dired-plus")
(require 'dired+)
(toggle-dired-find-file-reuse-dir 1)

;;; GRAPHVIZ
(load-file "~/.emacs.d/plugins/graphviz-dot-mode/graphviz-dot-mode.el")

;;; egg: http://github.com/bogolisk/egg
(add-to-list 'load-path "~/.emacs.d/plugins/egg")
; (load "~/.emacs.d/plugins/egg/egg.el")
(require 'egg)

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
(load "~/.emacs.d/plugins/nxhtml/autostart.el")
(add-to-list 'auto-mode-alist '("\\.html\.erb$" . eruby-html-mumamo-mode))

;;; Textile: http://dev.nozav.org/textile-mode.html
(add-to-list 'load-path "~/.emacs.d/plugins/textile-mode")
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;; gist
(add-to-list 'load-path "~/.emacs.d/plugins/gist")
(require 'gist)

;; magit
(add-to-list 'load-path "~/.emacs.d/plugins/magit")
(require 'magit)

;;; cucumber
(add-to-list 'load-path "~/.emacs.d/plugins/cucumber")
(require 'feature-mode)

;;; autopair
(add-to-list 'load-path "~/.emacs.d/plugins/autopair")
(require 'autopair)
(autopair-global-mode)

;;; undo-tree
(add-to-list 'load-path "~/.emacs.d/plugins/undo-tree")
(require 'undo-tree)
(global-undo-tree-mode)

;;; muti-term
(add-to-list 'load-path "~/.emacs.d/plugins/multi-term")
(require 'multi-term)

(setq multi-term-program "/bin/bash")   ;; use bash
;; (setq multi-term-program "/bin/zsh") ;; or use zsh...

;; only needed if you use autopair
(add-hook 'term-mode-hook
  #'(lambda () (setq autopair-dont-activate t)))


(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

;;; scratch-el
(add-to-list 'load-path "~/.emacs.d/plugins/scratch-el")
(require 'scratch)

;;; edit-server
(load-file "~/.emacs.d/plugins/emacs_chrome/servers/edit-server.el")
(require 'edit-server)
;; (setq edit-server-new-frame nil)
;; (add-hook 'edit-server-done-hook 'on-edit-server-done-do-backup)
(edit-server-start)

;;; browse-kill-ring
(add-to-list 'load-path "~/.emacs.d/plugins/browse-kill-ring")
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(provide 'plugins)
