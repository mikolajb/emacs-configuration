;;; Sawzall
(load-file "~/.emacs.d/plugins/sawzall.el")

;;; protobuf
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))

(provide 'plugins)
