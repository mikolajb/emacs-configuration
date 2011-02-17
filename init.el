(require 'global)
(require 'theme)
(require 'modes)
(require 'plugins)
(require 'funs)
(require 'snippets)

;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
;; Add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))


(provide 'init)
