;;; Sawzall
(load-file "~/.emacs.d/plugins/sawzall.el")
(load "~/.emacs.d/plugins/polish-holidays/polish-holidays.el")
(setq holiday-general-holidays (append swieta-panstwowe-pozostałe-święta
                                       ustawowo-wolne-od-pracy
                                       swieta-katolickie))
(load "~/.emacs.d/plugins/dutch-holidays/dutch-holidays.el")
(setq holiday-other-holidays dutch-national-holidays)

;;; protobuf
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))

(provide 'plugins)
