(use-package general
  :ensure t)

(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  (auto-package-update-interval 1)
  (auto-package-update-at-time "16:00")
  (auto-package-update-prompt-before-update t)
  :config
  (auto-package-update-maybe))

(use-package tramp
  :custom
  (tramp-default-method "ssh")
  (tramp-chunksize 500)
  (tramp-backup-directory-alist backup-directory-alist))

(use-package vc-hooks
  :custom
  (vc-follow-symlinks t))

(use-package comint
  :custom
  (comint-prompt-read-only t)
  :config
  ;; comint env to work with node.js
  (setenv "NODE_NO_READLINE" "1"))

(use-package shell
  :hook (shell-mode-hook . compilation-shell-minor-mode)
  :general
  (:keymaps 'shell-mode-map
            "C-c n" 'rename-term-buffer))

(use-package term
  :general
  (:keymaps '(term-raw-map term-mode-map)
            "M-x" 'helm-M-x)
  (:keymaps '(term-raw-map term-mode-map)
            :prefix "C-c"
            "t" 'helm-shell-buffers-list
            "n" 'rename-term-buffer)
  (:keymaps 'term-raw-map
            "C-c C-y" 'term-paste)
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

(use-package vterm
  :ensure t
  :custom
  (vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y"))
  ;; (vterm-buffer-name-string "vterm %s")
  :general
  (:keymaps 'vterm-mode-map
            "C-c n" 'rename-term-buffer
            "C-x C-m" 'magit-status
            "C-\\" 'shell-on-top)
  :init
  (add-hook 'vterm-mode-hook
            (lambda ()
              (buffer-face-set '(:height 95)))))

(use-package lsp-mode
  :ensure t
  :hook ((rust-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (sh-mode . lsp-deferred)
         (lsp-mode . lsp-toggle-symbol-highlight))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-x")
  (lsp-enable-xref t)
  (lsp-log-io t)
  (lsp-file-watch-threshold 10000))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-flycheck-live-reporting nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-doc-enable nil))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package use-package-ensure-system-package
  :ensure t)

(use-package rust-mode
  :ensure t
  :ensure-system-package rls
  :hook (rust-mode-hook . lsp-format-buffer)
  :mode "\\.rs$")

(use-package go-mode
  :ensure t
  :ensure-system-package gopls
  :mode "\\.go$"
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
  (setenv "GOPATH" (expand-file-name "go" (getenv "HOME")))
  (add-to-list 'exec-path (expand-file-name "bin/" (getenv "GOPATH")))
  (setenv "PATH" (concat "/usr/local/go/bin:" (getenv "PATH")))
  (setenv "GO111MODULE" "on")
  (when (string= (system-name) "utopiec")
    (setenv "USE_SYSTEM_GO" "yes")
    (setenv "GOFLAGS" "-mod=vendor")))

(use-package go-guru
  :ensure t)

(use-package go-playground
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package direnv
  :ensure t)

(use-package flycheck-golangci-lint
  :ensure t
  :requires flycheck)

(use-package ruby-mode
  :mode ("\\.rb$" "\\.ru$" "Rakefile$" "rakefile$" "Rakefile.rb$" "rakefile.rb$" "\\.rake$" "capfile$" "Capfile$" "Gemfile$")
  :interpreter "ruby"
  :functions inf-ruby-keys
  :config
  (add-hook 'ruby-mode-hook
            '(lambda ()
               (inf-ruby-keys))))

(use-package python
  :mode "^wscript$"
  :ensure-system-package pyls
  :interpreter ("python" . python-mode))

(use-package auto-complete
  :custom
  (ac-auto-start 2)
  (ac-override-local-map nil)
  (ac-use-menu-map t)
  (ac-candidate-limit 20))

(use-package eshell
  :custom
  (eshell-prefer-lisp-functions t)
  :init
  (defun pcomplete/sudo ()
    (let ((prec (pcomplete-arg 'last -1)))
      (cond ((string= "sudo" prec)
             (while (pcomplete-here*
                     (funcall pcomplete-command-completion-function)
                     (pcomplete-arg 'last) t))))))
  ;; :config
  ;; (add-to-list 'eshell-modules-list 'em-tramp)
  )

(use-package em-smart
  :requires eshell
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t)
  :init
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (eshell-cmpl-initialize)
                (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
                (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
                (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))))

(use-package js2-mode
  :ensure t
  :mode "\\.js$")

(use-package dired
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (directory-free-space-args "-Ph"))

(use-package dired+
  :load-path "~/.emacs.d/plugins/"
  :requires dired
  :config
  (toggle-diredp-find-file-reuse-dir 1))

(use-package scratch
  :ensure t)

(use-package rg
  :ensure t
  :ensure-system-package rg)

(use-package wgrep
  :ensure t)

(use-package magit
  :ensure t
  :general
  ("C-x C-m" 'magit-status)
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
  (unless (string= (system-name) "utopiec")
    (magit-wip-mode 1))
  :init
  (add-hook 'magit-log-edit-mode-hook #'flyspell-mode)
  (add-hook 'magit-log-edit-mode-hook #'auto-fill-mode))

(use-package projectile
  :ensure t)

(use-package vdiff-magit
  :ensure t)

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
  :general
  ("C-S-c C-S-c" 'mc/edit-lines)
  ("C->" 'mc/mark-next-like-this)
  ("C-<" 'mc/mark-previous-like-this)
  ("C-c C-<" 'mc/mark-all-like-this))

(use-package tex-mode
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (reftex-plug-into-AUCTeX t)
  ; (TeX-master nil "query for master file")
  :hook ((LaTeX-mode-hook . visual-line-mode)
         (LaTeX-mode-hook . flyspell-mode)
         (LaTeX-mode-hook . LaTeX-math-mode)
         (LaTeX-mode-hook . turn-on-reftex)
         (LaTeX-mode-hook . turn-on-auto-fill)
         (LaTeX-mode-hook . abbrev-mode))
  :no-require
  :config
  (add-to-list 'TeX-expand-list
               '("%(RubberPDF)"
                 (lambda ()
                   (if
                       (not TeX-PDF-mode)
                       ""
                     "--pdf"))))
  (add-to-list 'TeX-command-list
               '("Rubber" "rubber %(RubberPDF) %t" TeX-run-shell nil t) t)
  (when (boundp 'latex-editor)
    ;; Write room
    (global-writeroom-mode)
    (custom-set-variables '(writeroom-major-modes '(latex-mode)))
    (custom-set-variables '(writeroom-extra-line-spacing 0.8))
    (custom-set-variables '(writeroom-width 100))))

(use-package flyckeck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :no-require
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

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

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto$"
  :config
  (defconst my-protobuf-style
    '((c-basic-offset . 2)
      (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t))))

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
  :custom
  (history-delete-duplicates t)
  :general
  ("C-x b" 'helm-mini)
  ("C-x C-f" 'helm-find-files)
  ("C-x C-l" 'helm-browse-project)
  ("M-x" 'helm-M-x))

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

(use-package helm-swoop
  :ensure t
  :requires helm)

(use-package helm-rg
  :ensure t
  :requires (helm rg))

(use-package helm-projectile
  :ensure t
  :requires (helm projectile))

(use-package helm-pass
  :ensure t
  :requires helm)

(use-package helm-lsp
  :ensure t
  :requires (helm lsp-mode))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-helm
  :ensure t
  :after flyspell-correct
  :requires (helm lsp))

(use-package beacon
  :ensure t
  :custom
  (beacon-blink-delay 0)
  (beacon-blink-duration 0.6)
  (beacon-color "#ffb86c")
  (beacon-blink-when-point-moves-vertically 10)
  :config
  (beacon-mode 1))

(use-package all-ext
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml$")

(use-package holidays
  :defer t
  :custom
  (holiday-general-holidays nil)
  (holiday-hebrew-holidays nil)
  (holiday-islamic-holidays nil)
  (holiday-bahai-holidays nil)
  (holiday-oriental-holidays nil))

(use-package calendar
  :hook (calendar-today-visible-hook . #'calendar-mark-today)
  :custom
  (mark-holidays-in-calendar t)
  (calendar-day-name-array ["niedziela" "poniedziałek" "wtorek" "środa" "czwartek" "piątek" "sobota"])
  (calendar-month-name-array ["styczeń" "luty" "marzec" "kwiecięń" "maj" "czerwiec" "lipiec" "sierpień" "wrzesień" "październik" "listopad" "grudzień"])
  (calendar-week-start-day 1)
  :config
  (calendar-set-date-style 'european))

(use-package polish-holidays
  :requires holidays
  :load-path "~/.emacs.d/plugins/polish-holidays/"
  :config
  (setq holiday-other-holidays swieta-panstwowe-pozostałe-święta)
  (append holiday-other-holidays ustawowo-wolne-od-pracy)
  (append holiday-other-holidays swieta-katolickie))

(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))

(use-package string-inflection
  ;; Reformat strings, replaces my own hand-writter functions
  :ensure t)

(use-package bazel-mode
  :if (eq (system-name) "utopiec")
  :mode "\\.bzl$"
  :ensure t)

(use-package figlet
  :defer t
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode "\\.md$")

(when (boundp 'latex-editor)
  (use-package writeroom-mode
    :ensure t))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package saveplace
  :custom
  (save-place t)
  :init
  (if (boundp 'latex-editor)
      (setq save-place-file "~/.emacs-latex.d/saveplace")
    (setq save-place-file "~/.emacs.d/saveplace")))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package deft
  :ensure t
  :general
  ("C-c C-d" 'deft)
  :commands (deft)
  :config (setq deft-directory "~/notes"
                deft-use-filename-as-title t
                deft-extensions '("md" "org" "gpg" "rst")))

(use-package xref
  :custom
  (xref-search-program 'ripgrep))

(use-package time
  :custom
  (world-clock-list '(("Europe/Berlin" "Berlin")
                      ("America/New_York" "New York")
                      ("America/Los_Angeles" "San Francisco"))))

;;; https://github.com/microsoft/cascadia-code/issues/153
(use-package composite
  :defer t
  :init
  (defvar composition-ligature-table (make-char-table nil))
  :hook
  (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
    . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
  ;; support ligatures, some toned down to prevent hang
  (let ((alist
         '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
           (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
           (36 . ".\\(?:\\(>\\)>?\\)")
           (37 . ".\\(?:\\(%\\)%?\\)")
           (38 . ".\\(?:\\(&\\)&?\\)")
           (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
           ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
           (43 . ".\\(?:\\([>]\\)>?\\)")
           ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
           (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
           ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
           (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
           (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
           ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
           (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
           (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
           (59 . ".\\(?:\\(;\\);?\\)")
           (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
           (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
           (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
           (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
           (91 . ".\\(?:\\(|\\)[]|]?\\)")
           ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
           (94 . ".\\(?:\\(=\\)=?\\)")
           (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
           (119 . ".\\(?:\\(ww\\)w?\\)")
           (123 . ".\\(?:\\(|\\)[|}]?\\)")
           (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
           (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-ligature-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))
  (set-char-table-parent composition-ligature-table composition-function-table))

(provide 'modes)
