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
(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))

;;; ERLANG: included in Erlang/OTP
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(require 'erlang-start)
(add-to-list 'auto-mode-alist '("\\.[eh]rl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.yaws$" . erlang-mode))

;;; AUCTEX: http://www.gnu.org/software/auctex (pacman -S auctex)
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;;; MERCURIAL: http://www.emacswiki.org/emacs/MercurialMode (included in Mercurial package)
(require 'mercurial)

;;; YAML: http://www.emacswiki.org/emacs/YamlMode (installed with ELPA)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;;; HAML mode (installed with ELPA)
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;;; org-mode (installed as archlinux AUR package: emacs-org-mode)
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-include-diary t)
(setq org-default-notes-file (concat "~/org/notes.org"))
(global-set-key "\C-cc" 'org-capture)
(defun org-capture-frame ()
  "turn the current frame into a small popup frame"
  (modify-frame-parameters nil
			   '((width . 120)
			     (height . 20)))
  (org-capture)
  (delete-other-windows)
 ;; (add-hook 'kill-buffer-hook '(lambda ()
  ;; 				 (delete-frame)))
  (raise-frame))


;;; Go lang mode (included in go language package)
(require 'go-mode-load)
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))

;;; midnight - clean-buffer-list
(require 'midnight)

;;; calendar
(load "~/.emacs.d/plugins/polish-holidays/polish-holidays.el")
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(setq mark-holidays-in-calendar t)

(provide 'modes)