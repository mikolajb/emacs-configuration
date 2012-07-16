;;; TERM
(add-hook 'term-mode-hook
          (function
           (lambda ()
             (setq term-prompt-regexp "^[^#$%>\n]*[#$%>ϱϑ] *")
             (make-local-variable 'mouse-yank-at-point)
             (make-local-variable 'transient-mark-mode)
             (setq mouse-yank-at-point t)
             (auto-fill-mode -1)
             (setq tab-width 8 ))))

;;; SHELL
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

;;; ESHELL
(setq eshell-prefer-lisp-functions t)

;;; TRAMP
(setq tramp-default-method "scpx")
(setq tramp-chunksize 150)

;;; RUBY: included in Emacs 23, Ruby package, also in ELPA
;; Based on http://infolab.stanford.edu/~manku/dotemacs.html
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("rakefile.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (inf-ruby-keys)))
;; removes hook which add 'coding ***' at the beggining of ruby files
;; (add-hook 'ruby-mode-hook
;; 	  '(lambda ()
;; 	     (remove-hook 'before-save-hook 'ruby-mode-set-encoding)))
;; If you have Emacs 19.2x or older, use rubydb2x
(autoload 'rubydb "rubydb3x" "Ruby debugger" t)
;; uncomment the next line if you want syntax highlighting
(add-hook 'ruby-mode-hook 'turn-on-font-lock)

;;; PYTHON: included in Emacs
(setq auto-mode-alist (append '(("^wscript$" . python-mode)) auto-mode-alist))

;;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;;; ropemacs
(defun load-ropemacs ()
  "Load ropemacs"
  (interactive)
  (pymacs-load "ropemacs" "rope-")
)

;;; ERLANG: included in Erlang/OTP
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(require 'erlang-start)
(add-to-list 'auto-mode-alist '("\\.[eh]rl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.yaws$" . erlang-mode))

;;; AUCTEX: http://www.gnu.org/software/auctex (pacman -S auctex)
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq reftex-plug-into-AUCTeX t) ; reftex
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'abbrev-mode)
(setq-default TeX-master nil) ; query for master file

;;; MERCURIAL: http://www.emacswiki.org/emacs/MercurialMode (included in Mercurial package)
(require 'mercurial)

;;; YAML: http://www.emacswiki.org/emacs/YamlMode (installed with ELPA)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;;; HAML mode (installed with ELPA)
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;;; Go lang mode (included in go language package)
(require 'go-mode-load)
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))

;;; midnight - clean-buffer-list
(require 'midnight)

;;; calendar
(require 'calendar)
(require 'holidays)
(calendar-set-date-style 'european)
(load "~/.emacs.d/plugins/polish-holidays/polish-holidays.el")
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(setq mark-holidays-in-calendar t)
(setq holiday-general-holidays (append swieta-panstwowe-pozostałe-święta
                                       ustawowo-wolne-od-pracy
                                       swieta-katolickie))
(setq all-christian-calendar-holidays t)
(load "~/.emacs.d/plugins/dutch-holidays/dutch-holidays.el")
(setq holiday-other-holidays dutch-national-holidays)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)
(setq holiday-oriental-holidays nil)
;; hack - remove when calendar will be fixed
(setq calendar-holidays
      (append holiday-general-holidays
              holiday-other-holidays
              holiday-christian-holidays
              holiday-solar-holidays))

;;; pkgbuild-mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))
;;(add-hook 'pkgbuild-mode-hook
;;          '(lambda ()
;;             (add-hook
;;              'kill-buffer-hook
;;              '(lambda ()
;;                 (let  (
;;                        (stderr-buffer (concat "*PKGBUILD(" (pkgbuild-get-directory (buffer-file-name)) ") stderr*"))
;;                        (stdout-buffer (concat "*PKGBUILD(" (pkgbuild-get-directory (buffer-file-name)) ") stdout*")))
;;                   (if (get-buffer stderr-buffer) (kill-buffer stderr-buffer))
;;                  (if (get-buffer stdout-buffer) (kill-buffer stdout-buffer))
;;                   )
;;                 ))))

;;; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; dired customizations
(setq dired-listing-switches "-alh")
(setq directory-free-space-args "-Ph")

(provide 'modes)
