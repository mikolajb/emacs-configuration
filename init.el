(require 'global)

(package-initialize)
;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/") t)
;; Add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; And Melpa
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; Org-mode
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(require 'theme)
(require 'modes)
(require 'plugins)
(require 'funs)
(require 'snippets)
(require 'orgmode-settings)

(provide 'init)
