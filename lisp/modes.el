(defvar my-packages
  '(magit
    autopair
    drag-stuff
    figlet
    go-mode
    markdown-mode
    multiple-cursors
    smex
    writeroom-mode
    ))

(unless (boundp 'latex-editor)
  (setq my-packages (append my-packages
                            '(
                              all-ext
                              auto-complete
                              beacon
                              clojure-mode
                              d-mode
                              dockerfile-mode
                              flycheck
                              flycheck-golangci-lint
                              full-ack
                              git-gutter
                              go-autocomplete
                              go-playground
                              godoctor
                              haml-mode
                              helm-ag
                              helm-descbinds
                              helm-ls-git
                              helm-pass
                              helm-projectile
                              helm-rg
                              helm-swoop
                              jedi
                              persistent-scratch
                              projectile
                              protobuf-mode
                              rg
                              scratch
                              string-inflection
                              wgrep
                              yaml-mode
                              yasnippet
                              ))))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))

(unless (boundp 'latex-editor)
  ;; TERM
  (add-hook 'term-mode-hook
            (lambda ()
              (setq term-prompt-regexp
                    "^[^#$%>🌚🌑🌒🌓🌔🌕🌝🌖🌗🌘\n]*[#$%>🌚🌑🌒🌓🌔🌕🌝🌖🌗🌘] *")
              ;; so it copies what mouse selects
              (make-local-variable 'mouse-yank-at-point)
              (setq mouse-yank-at-point t)
              (make-local-variable 'transient-mark-mode)
              ;; (auto-fill-mode -1)
              (setq tab-width 8)
              ;; let's make the font a bit smaller
              (buffer-face-set '(:height 100))
              (setq bidi-paragraph-direction 'left-to-right)
              (define-key term-raw-map (kbd "M-x") #'execute-extended-command)
              (define-key term-raw-map (kbd "C-c C-y") 'term-paste)
              (yas-minor-mode -1)))
  ;; SHELL
  (add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

  ;; ESHELL
  (setq eshell-prefer-lisp-functions t)
  ;; (add-to-list 'eshell-modules-list 'em-tramp)

  ;; TRAMP
  (setq tramp-default-method "ssh")
  (setq tramp-chunksize 500)
  (setq tramp-backup-directory-alist backup-directory-alist)

  ;; RUBY: included in Emacs 23, Ruby package, also in ELPA
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

  ;; PYTHON: included in Emacs
  (setq auto-mode-alist (append '(("^wscript$" . python-mode)) auto-mode-alist))

  ;; Jedi
  (require 'jedi)

  ;; auto-complete mode extra settings
  (setq
   ac-auto-start 2
   ac-override-local-map nil
   ac-use-menu-map t
   ac-candidate-limit 20)

  (add-hook 'python-mode-hook
            (lambda ()
              (jedi:setup)
              (jedi:ac-setup)
              (local-set-key "\C-cd" 'jedi:show-doc)
              (local-set-key (kbd "M-SPC") 'jedi:complete)
              (local-set-key (kbd "M-.") 'jedi:goto-definition)))

  ;; YAML: http://www.emacswiki.org/emacs/YamlMode (installed with ELPA)
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

  ;; HAML mode (installed with ELPA)
  (require 'haml-mode)
  (add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

  ;; Go lang mode (included in go language package)
  ;;
  ;; go get -u github.com/mdempsky/gocode
  ;; go get -u golang.org/x/tools/cmd/goimports
  ;; go get -u github.com/golang/lint/golint
  ;; go get -u github.com/kisielk/errcheck
  ;; go get -u github.com/rogpeppe/godef
  ;; go get -u github.com/golangci/golangci-lint
  ;; https://johnsogg.github.io/emacs-golang
  (require 'go-mode)
  ;; pkg go installation
  (setq exec-path (append '("/usr/local/go/bin") exec-path))
  (setenv "PATH" (concat "/usr/local/go/bin:" (getenv "PATH")))

  (add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
  (setq gofmt-command (expand-file-name "bin/goimports" (getenv "GOPATH")))
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-to-list 'exec-path (expand-file-name "bin/" (getenv "GOPATH")))
  (require 'go-autocomplete)
  (require 'auto-complete-config)
  (ac-config-default)
  (setq go-test-verbose t)

  ;; midnight - clean-buffer-list
  (require 'midnight)

  ;; calendar
  (require 'calendar)
  (require 'holidays)
  (calendar-set-date-style 'european)
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  (setq mark-holidays-in-calendar t)
  (setq all-christian-calendar-holidays t)
  (setq holiday-hebrew-holidays nil)
  (setq holiday-islamic-holidays nil)
  (setq holiday-oriental-holidays nil)
  ;; hack - remove when calendar will be fixed
  (setq calendar-holidays
        (append holiday-general-holidays
                holiday-other-holidays
                holiday-christian-holidays
                holiday-solar-holidays))

  ;; js2-mode
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  ;; dired customizations
  (setq dired-listing-switches "-alh --group-directories-first")
  (setq directory-free-space-args "-Ph")

  ;; scratch-el
  (require 'scratch)

  ;; full-ack
  (autoload 'ack-same "full-ack" nil t)
  (autoload 'ack "full-ack" nil t)
  (autoload 'ack-find-same-file "full-ack" nil t)
  (autoload 'ack-find-file "full-ack" nil t))

;; magit
(require 'magit)
(add-hook 'magit-log-edit-mode-hook 'flyspell-mode)
(add-hook 'magit-log-edit-mode-hook 'auto-fill-mode)
(global-set-key [f10] 'magit-status)
(setq magit-diff-refine-hunk 'all)
(setq magit-section-initial-visibility-alist
      '((stashes . show)
       (untracked . show)
       (unstaged . show)
       (unpushed . show)
       (unpulled . show)))

;;; autopair
(require 'autopair)
(autopair-global-mode)
(add-hook 'term-mode-hook
	  #'(lambda () (autopair-mode -1)))
(setq autopair-autowrap t)
(setq autopair-blink-delay 0.05)

;;; Multiple cursors
(require 'multiple-cursors)
(when (boundp 'latex-editor)
  (setq mc/list-file "~/.emacs-latex.d/.mc-lists.el"))
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

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
(eval-after-load "tex"
  '(progn
     (add-to-list 'TeX-expand-list
                  '("%(RubberPDF)"
                    (lambda ()
                      (if
                          (not TeX-PDF-mode)
                          ""
                        "--pdf"))))
     (add-to-list 'TeX-command-list
                  '("Rubber" "rubber %(RubberPDF) %t" TeX-run-shell nil t) t)))

(when (boundp 'latex-editor)
  ;; Write room
  (global-writeroom-mode)
  (custom-set-variables '(writeroom-major-modes '(latex-mode)))
  (custom-set-variables '(writeroom-extra-line-spacing 0.8))
  (custom-set-variables '(writeroom-width 100)))

;;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))
(setq flycheck-golangci-lint-fast t)

;; Drag stuff mode
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "my-style" my-protobuf-style t)))

;; Persistent scratch
;; https://github.com/Fanael/persistent-scratch
(persistent-scratch-setup-default)
(persistent-scratch-autosave-mode 1)

;; Save SQL history
(defun my-sql-save-history-hook ()
  (let ((lval 'sql-input-ring-file-name)
        (rval 'sql-product))
    (if (symbol-value rval)
        (let ((filename
               (concat "~/.emacs.d/"
                       (symbol-name (symbol-value rval))
                       "-history.sql")))
          (set (make-local-variable lval) filename))
      (error
       (format "SQL history will not be saved because %s is nil"
               (symbol-name rval))))))
(add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook)

;;; git gutter
(require 'git-gutter)
(custom-set-variables
 '(git-gutter:modified-sign " ")
 '(git-gutter:added-sign " ")
 '(git-gutter:deleted-sign " "))
(set-face-background 'git-gutter:modified "yellow")
(set-face-background 'git-gutter:added "green")
(set-face-background 'git-gutter:deleted "red")
(global-git-gutter-mode +1)

;;; helm
(require 'helm-config)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-l") 'helm-browse-project)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-split-window-default-side 'same)
(require 'helm-ls-git)
(require 'helm-descbinds)
(helm-descbinds-mode)

;;; beacon
(beacon-mode 1)

;;; all-ext
(require 'all-ext)

(provide 'modes)
