(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-include-diary t)
(setq org-agenda-diary-file "~/org/diary.org")
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

(add-hook 'org-timer-done-hook
	  (lambda
	    (interactive)
	    (play-sound-file 'bell.mp3)))



;;; from orgmode.org/worg/org-faq.html

(setq org-latex-listings t)

;; Specify default packages to be included in every tex file, whether pdflatex or xelatex
(setq org-latex-packages-alist
      '(("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)))

;; Packages to include when xelatex is used
(setq org-latex-default-packages-alist
          '(("" "fontspec" t)
            ("" "xltxtra" t)
            ("" "listings" t)
            ("" "url" t)
            ("" "rotating" t)
            ("margin=2cm" "geometry")
            ("english" "babel" t)
            ("babel" "csquotes" t)
            ("" "soul" t)
            ("xetex,citecolor=black,linkcolor=black,urlcolor=black,colorlinks=true" "hyperref" nil)
            ))

(require 'ox-latex)
(setq org-latex-listings t)
(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass[12pt,a4paper]{article}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("report"
               "\\documentclass[12pt,a4paper]{report}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("commit-report"
               "\\documentclass[12pt]{commit-report}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation))
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

(defun org-export-to-latex-in-pwd
    (name &optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((outfile (concat (file-name-as-directory (getenv "PWD")) name)))
    (print outfile)
    (org-export-to-file 'latex outfile
      async subtreep visible-only body-only ext-plist)))

(provide 'orgmode-settings)
