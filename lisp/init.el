(require 'global)

;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/") t)
;; Add the user-contributed repository
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; And Melpa
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; Org-mode
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(require 'theme)
(require 'modes)
(require 'snippets)
(require 'funs)


(unless (boundp 'latex-editor)
  (require 'plugins)
  (require 'orgmode-settings)

  ;; workaround for epg
  (defun epg--list-keys-1 (context name mode)
    (let ((args (append (if (epg-context-home-directory context)
                            (list "--homedir"
                                  (epg-context-home-directory context)))
                        '("--with-colons" "--no-greeting" "--batch"
                          "--with-fingerprint" "--with-fingerprint")
                        (unless (eq (epg-context-protocol context) 'CMS)
                          '("--fixed-list-mode"))))
          (list-keys-option (if (memq mode '(t secret))
                                "--list-secret-keys"
                              (if (memq mode '(nil public))
                                  "--list-keys"
                                "--list-sigs")))
          (coding-system-for-read 'binary)
          keys string field index)
      (if name
          (progn
            (unless (listp name)
              (setq name (list name)))
            (while name
              (setq args (append args (list list-keys-option (car name)))
                    name (cdr name))))
        (setq args (append args (list list-keys-option))))
      (with-temp-buffer
        (apply #'call-process
               (epg-context-program context)
               nil (list t nil) nil args)
        (goto-char (point-min))
        (while (re-search-forward "^[a-z][a-z][a-z]:.*" nil t)
          (setq keys (cons (make-vector 15 nil) keys)
                string (match-string 0)
                index 0
                field 0)
          (while (and (< field (length (car keys)))
                      (eq index
                          (string-match "\\([^:]+\\)?:" string index)))
            (setq index (match-end 0))
            (aset (car keys) field (match-string 1 string))
            (setq field (1+ field))))
        (nreverse keys)))))

(provide 'init)
