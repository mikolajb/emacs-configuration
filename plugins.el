;;; DVC
(load-file "~/.emacs.d/plugins/dvc/dvc-load.el")

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

;;; nav-emacs: http://code.google.com/p/emacs-nav
(add-to-list 'load-path "~/.emacs.d/plugins/emacs-nav")
(require 'nav)

(provide 'plugins)