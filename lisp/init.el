(defconst linux-kernel-version (substring (shell-command-to-string "uname -r") 0 -1))

(require 'global)

;; And Melpa
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-compute-statistics t)

(require 'theme)
(require 'modes)
(require 'funs)
(require 'orgmode-settings)
(require 'events)

(provide 'init)
