(require 'global)

(package-initialize)
;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
;; Add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
;; And GNU
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))

(require 'theme)
(require 'modes)
(require 'plugins)
(require 'funs)
(require 'snippets)
(require 'orgmode-settings)
;; (require 'powerline)

(provide 'init)
