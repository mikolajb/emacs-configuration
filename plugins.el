;;; icicles
(add-to-list 'load-path "~/.emacs.d/plugins/icicles")
(require 'icicles)
(setq icicle-buffer-sort 'icicle-most-recent-first-p)
(icy-mode)

;;; icomplete+ (downloaded from emacswiki)
(add-to-list 'load-path "~/.emacs.d/plugins/icomplete-plus")
(eval-after-load "icomplete" '(progn (require 'icomplete+)))

;;; dired-plus
(add-to-list 'load-path "~/.emacs.d/plugins/dired-plus")
(require 'dired+)
(toggle-diredp-find-file-reuse-dir 1)

;;; egg: http://github.com/bogolisk/egg
(add-to-list 'load-path "~/.emacs.d/plugins/egg")
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
(add-hook 'term-mode-hook
  #'(lambda () (setq autopair-dont-activate t)))
(setq autopair-autowrap t)

;;; undo-tree
(add-to-list 'load-path "~/.emacs.d/plugins/undo-tree")
(require 'undo-tree)
(global-undo-tree-mode)

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

;;; browse-kill-ring
(add-to-list 'load-path "~/.emacs.d/plugins/browse-kill-ring")
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;; browse-kill-ring-plus
(add-to-list 'load-path "~/.emacs.d/plugins/browse-kill-ring-plus")
(require 'browse-kill-ring+)

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

(provide 'plugins)
