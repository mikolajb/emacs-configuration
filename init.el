;; Set encoding to UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Frame title : set to buffer name
(setq frame-title-format "Emacs - %f ")
(setq icon-title-format "Emacs - %b")

(when (>= emacs-major-version 23)
  (set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; AUTOINSTALL

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install"))

;;; ICICLES

(require 'icicles)
(icy-mode)

;;; LACARTE
(require 'lacarte)
(global-set-key [?\e ?\M-x] 'lacarte-execute-menu-command)

;;; RUBY
;; Based on http://infolab.stanford.edu/~manku/dotemacs.html
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("rakefile.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (inf-ruby-keys)))
;; usuwa dodawanie coding *** na poczatku pliku
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (remove-hook 'before-save-hook 'ruby-mode-set-encoding)))
;; If you have Emacs 19.2x or older, use rubydb2x
(autoload 'rubydb "rubydb3x" "Ruby debugger" t)
;; uncomment the next line if you want syntax highlighting
;;(add-hook 'ruby-mode-hook 'turn-on-font-lock)

;; automatycznie wykonywalny:
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; PYTHON
(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))

;;; HASKELL
   (setq auto-mode-alist
         (append auto-mode-alist
                 '(("\\.[hg]s$"  . haskell-mode)
                   ("\\.hi$"     . haskell-mode)
                   ("\\.l[hg]s$" . literate-haskell-mode))))

   (autoload 'haskell-mode "haskell-mode"
      "Major mode for editing Haskell scripts." t)
   (autoload 'literate-haskell-mode "haskell-mode"
      "Major mode for editing literate Haskell scripts." t)

;;; ERLANG
(setq load-path (cons  "/usr/lib/erlang/lib/tools-2.6.4/emacs"
		       load-path))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))

(require 'erlang-start)

;;; Z KREDITORA

;;; Erlang
(add-to-list 'auto-mode-alist '("\\.[eh]rl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.yaws$" . erlang-mode))

;;; Annoying stuff
(setq visible-bell t)
(setq inhibit-startup-echo-area-message t
      inhibit-startup-message           t)
(scroll-bar-mode nil)
(tool-bar-mode 0)
;(menu-bar-mode 0)

;;; color theme
(require 'color-theme)
;(add-to-list 'load-path "~/.emacs.d/color-theme/") ;; zahardkodowane!
;(require 'color-theme-hober2) ;; zahardkodowane!
(color-theme-initialize)
;(color-theme-hober2) ;; to tez zahardkodowane!
(load "~/.emacs.d/plugins/color-theme-ir-black/color-theme-ir-black")
(color-theme-ir-black)


;;; Global modes
(column-number-mode t)
(line-number-mode t)
(mouse-wheel-mode t)
(transient-mark-mode t)
; stops selection with a mouse being immediately injected to the kill ring
(setq mouse-drag-copy-region nil) 
; stops killing/yanking interacting with primary X11 selection 
(setq x-select-enable-primary nil)
; makes killing/yanking interact with clipboard X11 selection
(setq x-select-enable-clipboard t)

;;; Desktops
(desktop-save-mode 1)

;;; DIRED+
(require 'dired+)

;;; AUCTEX
(when (>= emacs-major-version 23)
  (add-to-list 'load-path (expand-file-name "/usr/share/emacs/site-lisp")))
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;;; CEDET
(load-file "/usr/share/emacs/site-lisp/cedet/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; COMPANY
(global-set-key [?\C-.] 'company-complete)

;;; MERCURIAL
(require 'mercurial)

;;; ECB
(require 'ecb)
(require 'ecb-autoloads)

;;; YAML
(add-to-list 'load-path "/home/mikolaj/.emacs.d/plugins/yaml")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda ()            
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;; DVC
(load-file "~/.emacs.d/plugins/dvc-snapshot/dvc-load.el")

;;; predictive
(add-to-list 'load-path "~/.emacs.d/plugins/predictive/")
(add-to-list 'load-path "~/.emacs.d/plugins/predictive/latex/")
(add-to-list 'load-path "~/.emacs.d/plugins/predictive/texinfo/")
(add-to-list 'load-path "~/.emacs.d/plugins/predictive/html/")
(require 'predictive)

;;; GRAPHVIZ
(load-file "~/.emacs.d/plugins/graphviz-dot-mode.el")

;;; PYMACS
(setenv "PYMACS_PYTHON" "python")
(add-to-list 'load-path "~/.emacs.d/plugins/Pymacs")
(require 'pymacs)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;;; ROPEMACS
(pymacs-load "ropemacs" "rope-")
(defun pymacs-reload-rope () 
  "Reload rope"
  (interactive)
  (pymacs-terminate-services )
  (pymacs-load "/home/anton/local/lib/python/ropemacs" "rope-"))
(global-set-key "\C-c\M-/" 'rope-code-assist)

;;; SLIME
(setq inferior-lisp-program "/path/to/lisp-executable")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(require 'slime)
(slime-setup)

;;; nXhtml
(load "/home/mikolaj/.emacs.d/plugins/nxhtml/autostart.el")
(add-to-list 'auto-mode-alist '("\\.html\.erb$" . eruby-html-mumamo-mode))
