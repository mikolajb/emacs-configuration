(defconst linux-kernel-version (substring (shell-command-to-string "uname -r") 0 -1))

;; And Melpa
(require 'package)
(add-to-list 'package-archives
         '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)
(setq use-package-compute-statistics t)

(require 'global)
(require 'theme)
(require 'modes)
(require 'funs)
(require 'orgmode-settings)
(require 'events)

(provide 'init)
