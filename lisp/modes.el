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
                              ace-window
                              all-ext
                              auto-complete
                              bazel-mode
                              beacon
                              clojure-mode
                              company-lsp
                              d-mode
                              dired-du
                              direnv
                              dockerfile-mode
                              flycheck
                              flycheck-golangci-lint
                              forge
                              full-ack
                              git-gutter
                              go-guru
                              go-playground
                              godoctor
                              haml-mode
                              helm-ag
                              helm-descbinds
                              helm-ls-git
                              helm-lsp
                              helm-pass
                              helm-projectile
                              helm-rg
                              helm-swoop
                              jedi
                              lsp-mode
                              lsp-treemacs
                              lsp-ui
                              magit-todos
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

(use-package tramp
  :custom
  (tramp-default-method "ssh")
  (tramp-chunksize 500)
  (tramp-backup-directory-alist backup-directory-alist))

(use-package comint
  :custom
  (comint-prompt-read-only t))

(use-package shell-mode
  :hook (shell-mode-hook . compilation-shell-minor-mode)
  :bind (:map shell-mode-map
              ("C-c n" . #'rename-term-buffer)))

(use-package term
  :bind (
         :map term-raw-map
              ("M-x" . #'helm-M-x)
              ("C-c C-y" . #'term-paste)
              ("C-c t" . #'helm-shell-buffers-list)
              ("C-c n" . #'rename-term-buffer)
              :map term-mode-map
              ("C-c n" . #'rename-term-buffer))
  :init
  (add-hook 'term-mode-hook
            (lambda ()
              (setq term-prompt-regexp " *[0-9a-zA-Z🅼]+ *[#$%>🌚🌑🌒🌓🌔🌕🌝🌖🌗🌘] *")
              (make-local-variable 'mouse-yank-at-point)
              (setq mouse-yank-at-point t)
              (make-local-variable 'transient-mark-mode)
              (setq tab-width 8)
              (buffer-face-set '(:height 95))
              (setq bidi-paragraph-direction 'left-to-right)
              (setq bidi-inhibit-bpa t)
              (yas-minor-mode -1))))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :requires lsp-ui
  :custom
  (lsp-enable-xref t)
  (lsp-log-io t))

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-flycheck-live-reporting nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-doc-enable nil))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package rust-mode
  :ensure t
  :ensure-system-package rls
  :hook ((rust-mode . lsp)
         (rust-mode . lsp-deferred)
         (rust-mode . lsp-toggle-symbol-highlight)))

(use-package go-mode
  :ensure t
  :ensure-system-package gopls
  :mode "\\.go$"
  :hook ((go-mode . lsp)
         (go-mode . lsp-deferred)
         (go-mode . lsp-toggle-symbol-highlight))
  :custom
  (exec-path (append '("/usr/local/go/bin") exec-path))
  (go-test-verbose t)
  (go-test-additional-arguments-function #'go-additional-arguments)
  :init
  (defun go-additional-arguments (suite-name test-name)
    "-count=1")
  (defun go-mode-before-save-hook ()
    (when (eq major-mode 'go-mode)
      (lsp-organize-imports)
      (lsp-format-buffer)))
  (add-hook 'before-save-hook #'go-mode-before-save-hook)
  (add-to-list 'exec-path (expand-file-name "bin/" (getenv "GOPATH")))
  (setenv "PATH" (concat "/usr/local/go/bin:" (getenv "PATH")))
  (setenv "GO111MODULE" "on")
  (when (string= (system-name) "utopiec")
    (setenv "USE_SYSTEM_GO" "yes")
    (setenv "GOFLAGS" "-mod=vendor")))

(use-package go-guru)

(unless (boundp 'latex-editor)
  ;; TERM

  ;; ESHELL
  (require 'eshell)
  (setq eshell-prefer-lisp-functions t)
  ;; (add-to-list 'eshell-modules-list 'em-tramp)
  (require 'em-smart)
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
              (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))
  (defun pcomplete/sudo ()
    (let ((prec (pcomplete-arg 'last -1)))
      (cond ((string= "sudo" prec)
             (while (pcomplete-here*
                     (funcall pcomplete-command-completion-function)
                     (pcomplete-arg 'last) t))))))

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

(use-package magit
  :ensure t
  :bind (([f10] . magit-status))
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-section-initial-visibility-alist
   '((stashes . show)
     (untracked . show)
     (unstaged . show)
     (unpushed . show)
     (unpulled . show)))
  (magit-pull-or-fetch t)
  (magit-status-goto-file-position t)
  (magit-revision-show-gravatars t)
  (magit-display-buffer-function #'display-buffer)
  (magit-commit-extend-override-date nil)
  (magit-commit-reword-override-date nil)
  (magit-refs-margin (quote (t age magit-log-margin-width t 18)))
  :config
  (magit-wip-mode 1)
  :init
  (add-hook 'magit-log-edit-mode-hook #'flyspell-mode)
  (add-hook 'magit-log-edit-mode-hook #'auto-fill-mode))

(use-package autopair
  :ensure t
  :custom
  (autopair-autowrap t)
  (autopair-blink-delay 0.05)
  :config
  (autopair-global-mode)
  :init
  (add-hook 'term-mode-hook
	        #'(lambda () (autopair-mode -1))))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<". 'mc/mark-all-like-this)))

;;; AUCTEX: http://www.gnu.org/software/auctex (pacman -S auctex)
;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
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

(use-package git-gutter
  :ensure t
  :custom
  (git-gutter:modified-sign " ")
  (git-gutter:added-sign " ")
  (git-gutter:deleted-sign " ")
  :custom-face
  (git-gutter:modified ((t (:background "yellow"))))
  (git-gutter:added ((t (:background "green"))))
  (git-gutter:deleted ((t (:background "red"))))
  :config
  (global-git-gutter-mode +1))

(use-package helm
  :ensure t
  :bind (("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x C-l" . helm-browse-project)
         ("M-x" . helm-M-x)))

(use-package helm-config
  :custom
  (helm-split-window-default-side 'same)
  :config
  (helm-mode 1))

(use-package helm-ls-git
  :ensure t
  :requires helm)

(use-package helm-descbinds
  :ensure t
  :requires helm
  :config
  (helm-descbinds-mode))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(use-package all-ext
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml$")

(provide 'modes)
