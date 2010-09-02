;;; GRAPHVIZ
(load-file "~/.emacs.d/plugins/graphviz-dot-mode.el")

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
(load-file "~/.emacs.d/plugins/textile-mode.el")
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;; gist
(add-to-list 'load-path "~/.emacs.d/plugins/gist")
(require 'gist)

;; magit
(add-to-list 'load-path "~/.emacs.d/plugins/magit")
(require 'magit)

;;; nav-emacs
(add-to-list 'load-path "~/.emacs.d/plugins/emacs-nav")
(require 'nav)

;;; cucumber
(add-to-list 'load-path "~/.emacs.d/plugins/cucumber")
(require 'feature-mode)

;;; org-mode
;; (add-to-list 'load-path "~/.emacs.d/plugins/org-mode/lisp")
;; (require 'org-install)

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

;;; iedit
(load-file "~/.emacs.d/plugins/iedit.el")
(require 'iedit)
;; (define-key global-map (kdb "C-;") 'iedit-mode)

;;; edit-server
(load-file "~/.emacs.d/plugins/emacs_chrome/servers/edit-server.el")
(require 'edit-server)
;; (setq edit-server-new-frame nil)
;; (add-hook 'edit-server-done-hook 'on-edit-server-done-do-backup)
(edit-server-start)

;;; browse-kill-ring
(load-file "~/.emacs.d/plugins/browse-kill-ring.el")
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(provide 'plugins)