(require 'global)

;; And Melpa
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(require 'theme)
(require 'modes)
(require 'snippets)
(require 'plugins)
(require 'funs)
(require 'orgmode-settings)

(provide 'init)
