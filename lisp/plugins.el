;;; icicles
(add-to-list 'load-path "~/.emacs.d/plugins/icicles")
(require 'icicles)
(setq icicle-buffer-sort 'icicle-most-recent-first-p)
(icy-mode)

;;; dired-plus
(load-file "~/.emacs.d/plugins/dired+.el")
(require 'dired+)
(toggle-diredp-find-file-reuse-dir 1)

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
