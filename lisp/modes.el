(use-package general)

(use-package tramp
  :straight nil
  :custom
  (tramp-default-method "ssh")
  (tramp-chunksize 500)
  (tramp-backup-directory-alist backup-directory-alist))

(use-package vc-hooks
  :straight nil
  :custom
  (vc-follow-symlinks t))

(use-package comint
  :straight nil
  :custom
  (comint-prompt-read-only t)
  :config
  ;; comint env to work with node.js
  (setenv "NODE_NO_READLINE" "1"))

(use-package shell
  :straight nil
  :hook (shell-mode-hook . compilation-shell-minor-mode)
  :general
  (:keymaps 'shell-mode-map
            "C-c n" 'rename-term-buffer))

(use-package term
  :straight nil
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
            #'(lambda ()
              (buffer-face-set '(:height 95)))))

(use-package lsp-mode
  :hook ((rust-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (sh-mode . lsp-deferred)
         (latex-mode . lsp-deferred)
         (lsp-mode . lsp-toggle-symbol-highlight))
  :commands (lsp lsp-deferred)
  :custom
  ;; install rust-analyzer: pacman -S rust-analyzer
  (lsp-keymap-prefix "C-x")
  (lsp-enable-xref t)
  (lsp-log-io t)
  (lsp-file-watch-threshold 10000)
  (lsp-ui-doc-delay 0)
  (lsp-go-use-gofumpt t)
  (lsp-go-analyses '(
                     (asmdecl . t)
                     (assign . t)
                     (atomic . t)
                     (atomicalign . t)
                     (bools . t)
                     (buildtag . t)
                     (cgocall . t)
                     (composites . t)
                     (copylocks . t)
                     (deepequalerrors . t)
                     (embed . t)
                     (errorsas . t)
                     (fieldalignment . t)
                     (httpresponse . t)
                     (ifaceassert . t)
                     (infertypeargs . t)
                     (loopclosure . t)
                     (lostcancel . t)
                     (nilfunc . t)
                     (nilness . t)
                     (printf . t)
                     (shadow . t)
                     (shift . t)
                     (simplifycompositelit . t)
                     (simplifyrange . t)
                     (simplifyslice . t)
                     (sortslice . t)
                     (stdmethods . t)
                     (stringintconv . t)
                     (structtag . t)
                     (testinggoroutine . t)
                     (tests . t)
                     (timeformat . t)
                     (unmarshal . t)
                     (unreachable . t)
                     (unsafeptr . t)
                     (unusedparams . t)
                     (unusedresult . t)
                     (unusedwrite . t)
                     (useany . t)
                     (fillreturns . t)
                     (nonewvars . t)
                     (noresultvalues . t)
                     (undeclaredname . t)
                     (unusedvariable . t)
                     (fillstruct . t)
                     (stubmethods . t)
                     ))
  (lsp-go-build-flags ["-tags=wireinject,mage"])
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t))))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-flycheck-live-reporting nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-doc-enable nil))

(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package rust-mode
  :mode "\\.rs$"
  :init
  (defun rust-mode-before-save-hook ()
    (when (eq major-mode 'rust-mode)
      (lsp-format-buffer)))
  (add-hook 'before-save-hook #'rust-mode-before-save-hook))

;; install gopls
(use-package go-mode
  :mode "\\.go$"
  :custom
  (exec-path (append (list "/usr/local/go/bin" (concat (getenv "HOME") "/go/bin")) exec-path))
  :init
  (defun go-mode-before-save-hook ()
    (when (eq major-mode 'go-mode)
      (lsp-organize-imports)
      (lsp-format-buffer)))
  (add-hook 'before-save-hook #'go-mode-before-save-hook)
  (setenv "PATH" (concat "/Users/mikolaj.baranowski/.local/share/devbox/global/default/.devbox/nix/profile/default/bin/:$HOME/go/bin:" (getenv "PATH")))
  (setenv "GO111MODULE" "on"))

(use-package gotest
  :custom
  (go-test-verbose t)
  (go-test-additional-arguments-function #'go-additional-arguments)
  :init
  (defun go-additional-arguments (suite-name test-name)
    "-count=1"))

(use-package go-guru)
(use-package go-playground)

(use-package dockerfile-mode)

(use-package direnv)

(use-package ruby-mode
  :straight nil
  :mode ("\\.rb$" "\\.ru$" "Rakefile$" "rakefile$" "Rakefile.rb$" "rakefile.rb$" "\\.rake$" "capfile$" "Capfile$" "Gemfile$")
  :interpreter "ruby"
  :functions inf-ruby-keys
  :config
  (add-hook 'ruby-mode-hook
            #'(lambda ()
               (inf-ruby-keys))))

;; install pyls
(use-package python
  :straight nil
  :mode "^wscript$"
  :interpreter ("python" . python-mode))

(use-package pinentry
  :ensure t
  :init
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

(use-package eshell
  :straight nil
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
  :straight nil
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
  :mode "\\.js$")

(use-package dired
  :straight nil
  :custom
  ;; (dired-listing-switches "-alh --group-directories-first")
  (directory-free-space-args "-Ph"))

(use-package dired+
  :straight nil
  :load-path "~/.emacs.d/plugins/"
  :requires dired
  :config
  (toggle-diredp-find-file-reuse-dir 1))

(use-package scratch)

;; install rg
(use-package rg)

(use-package wgrep)

(use-package deadgrep
  :straight (deadgrep :type git :host github :repo "Wilfred/deadgrep"))

(use-package magit
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
  (magit-repository-directories `(("~/projects" . 2)
                                  ("~/.password-store" . 1)
                                  ("~/notes" . 1)
                                  ("~/.emacs.d" . 1)))
  :config
  (unless (string= (system-name) "utopiec")
    (magit-wip-mode 1))
  :init
  (add-hook 'magit-log-edit-mode-hook #'flyspell-mode)
  (add-hook 'magit-log-edit-mode-hook #'auto-fill-mode))

(use-package forge
  :after closql)

(use-package closql
  :defer t)

;; (use-package projectile
;;   :init
;;   (projectile-mode +1)
;;   :bind (:map projectile-mode-map
;;               ("C-c p" . projectile-command-map)))

(use-package vdiff-magit)

(use-package multiple-cursors
  :general
  ("C-S-c C-S-c" 'mc/edit-lines)
  ("C->" 'mc/mark-next-like-this)
  ("C-<" 'mc/mark-previous-like-this)
  ("C-c C-<" 'mc/mark-all-like-this))

(use-package tex-mode
  :straight nil
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

(use-package flycheck
  :init
  (global-flycheck-mode))

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
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package protobuf-mode
  :mode "\\.proto$"
  :config
  (defconst my-protobuf-style
    '((c-basic-offset . 2)
      (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook
            #'(lambda () (c-add-style "my-style" my-protobuf-style t))))

(use-package git-gutter
  :custom
  (git-gutter:modified-sign " ")
  (git-gutter:added-sign " ")
  (git-gutter:deleted-sign " ")
  :config
  (global-git-gutter-mode +1))

(use-package helm
  :custom
  (history-delete-duplicates t)
  (helm-buffer-max-length nil)
  :general
  ("C-x b" 'helm-mini)
  ("C-x C-f" 'helm-find-files)
  ("C-x C-l" 'helm-browse-project)
  ("M-x" 'helm-M-x))

(use-package helm-config
  :straight nil
  :custom
  (helm-split-window-default-side 'same)
  :config
  (helm-mode 1))

(use-package helm-ls-git
  :requires helm)

(use-package helm-descbinds
  :requires helm
  :config
  (helm-descbinds-mode))

(use-package helm-swoop
  :requires helm)

(use-package helm-rg
  :requires (helm rg))

(use-package helm-projectile
  :requires (helm projectile)
  :bind (("C-<return>" . helm-projectile-rg))
  :config
  (helm-projectile-on))

(use-package helm-pass
  :requires helm)

(use-package helm-lsp
  :requires (helm lsp-mode))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-helm
  :after flyspell-correct
  :requires (helm lsp))

(use-package beacon
  :custom
  (beacon-blink-delay 0)
  (beacon-blink-duration 0.6)
  (beacon-color "#e2e2dc")
  (beacon-blink-when-point-moves-vertically 10)
  :config
  (beacon-mode 1))

;; (use-package all-ext)
(use-package yaml-mode
  :mode "\\.ya?ml$")

(use-package holidays
  :straight nil
  :defer t
  :custom
  (holiday-general-holidays nil)
  (holiday-hebrew-holidays nil)
  (holiday-islamic-holidays nil)
  (holiday-bahai-holidays nil)
  (holiday-oriental-holidays nil))

(use-package calendar
  :straight nil
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
  :straight (polish-holidays :type git :host github :repo "mikolajb/emacs-polish-holidays")
  :config
  (setq holiday-other-holidays swieta-panstwowe-pozostałe-święta)
  (append holiday-other-holidays ustawowo-wolne-od-pracy)
  (append holiday-other-holidays swieta-katolickie))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))

;; Reformat strings, replaces my own hand-writter functions
(use-package string-inflection)

(use-package figlet
  :defer t)

(use-package markdown-mode
  :mode "\\.md$")

(when (boundp 'latex-editor)
  (use-package writeroom-mode))

(use-package yasnippet
  :commands yas-minor-mode
  :hook
  (go-mode . yas-minor-mode))

(use-package saveplace
  :straight nil
  :config
  (save-place-mode 1)
  :init
  (if (boundp 'latex-editor)
      (setq save-place-file "~/.emacs-latex.d/saveplace")
    (setq save-place-file "~/.emacs.d/saveplace")))

(use-package uniquify
  :straight nil
  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package deft
  :general
  ("C-c C-d" 'deft)
  :commands (deft)
  :config (setq deft-directory "~/notes"
                deft-use-filename-as-title t
                deft-extensions '("md" "org" "gpg" "rst")))

(use-package xref
  :straight nil
  :custom
  (xref-search-program 'ripgrep))

(use-package time
  :straight nil
  :custom
  (world-clock-list '(("Europe/Berlin" "Berlin")
                      ("America/New_York" "New York")
                      ("America/Los_Angeles" "San Francisco"))))

;; (use-package auto-compile
;;   :config
;;   (auto-compile-on-load-mode)
;;   (auto-compile-on-save-mode)
;;   (setq auto-compile-display-buffer nil)
;;   (setq auto-compile-mode-line-counter t)
;;   (setq auto-compile-source-recreate-deletes-dest t)
;;   (setq auto-compile-toggle-deletes-nonlib-dest t)
;;   (setq auto-compile-update-autoloads t))

(use-package auth-source
  :straight nil
  :custom
  (auth-source-save-behavior nil)
  :config
  (add-to-list 'auth-source-protocols
               '(sudo "sudo" "")))

(use-package hl-line
  :straight nil
  :init
  (add-hook 'term-mode-hook
            #'(lambda () (global-hl-line-mode nil)))
  (add-hook 'vterm-mode-hook
            #'(lambda () (global-hl-line-mode nil)))
  :config
  (global-hl-line-mode t))

(use-package pkgbuild-mode
  :mode ("/PKGBUILD$"))

(use-package sh-script
  :straight nil
  :custom
  (sh-indentation 2))

(use-package magit-filenotify)

(use-package dired-atool
  :requires dired
  :init
  (dired-atool-setup))

;; (use-package bufler
;;   :quelpa (bufler :fetcher github :repo "alphapapa/bufler.el"
;;                   :files (:defaults (:exclude "helm-bufler.el")))
;;   :general
;;   ("C-x C-b" 'bufler))

;; (use-package helm-bufler
;;   :quelpa (helm-bufler :fetcher github :repo "alphapapa/bufler.el"
;;                        :files ("helm-bufler.el")))

(use-package ligature
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(use-package ctrlf
  :config
  (ctrlf-mode +1))

(use-package git-link
  :straight (git-link :type git :host github :repo "sshaw/git-link"))

(use-package helpful
  :general
  ("C-h f" 'helpful-callable)
  ("C-h v" 'helpful-variable)
  ("C-h k" 'helpful-key))

(use-package git-timemachine
  :straight (git-timemachine :type git :host github :repo "emacsmirror/git-timemachine"))

;;; (use-package systemd)

(use-package age
  :ensure t
  :demand t
  :config
  (age-file-enable)
  (setopt age-default-identity  "~/.age/identity.txt"
          age-default-recipient "~/.age/recipients.pub"
          age-pinentry-mode 'ask
          age-debug t
          age-program (executable-find "rage")))

(provide 'modes)
