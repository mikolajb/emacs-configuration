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

(setq org-export-latex-listings t)

;; Originally taken from Bruno Tavernier: http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
;; but adapted to use latexmk 4.20 or higher.
(defun my-auto-tex-cmd ()
  "When exporting from .org with latex, automatically run latex,
     pdflatex, or xelatex as appropriate, using latexmk."
  (let ((texcmd)))
  ;; default command: oldstyle latex via dvi
  (setq texcmd "latexmk -dvi -pdfps -quiet %f")
  ;; pdflatex -> .pdf
  (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
      (setq texcmd "latexmk -pdf -quiet %f"))
  ;; xelatex -> .pdf
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq texcmd "latexmk -pdflatex=xelatex -pdf -quiet %f"))
  ;; LaTeX compilation command
  (setq org-latex-to-pdf-process (list texcmd)))

;; Specify default packages to be included in every tex file, whether pdflatex or xelatex
(setq org-export-latex-packages-alist
      '(("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)))

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-cmd)

(defun my-auto-tex-parameters ()
      "Automatically select the tex packages to include."
      ;; default packages for ordinary latex or pdflatex export
      (setq org-export-latex-default-packages-alist
            '(("AUTO" "inputenc" t)
              ("T1"   "fontenc"   t)
              (""     "fixltx2e"  nil)
              (""     "wrapfig"   nil)
              (""     "soul"      t)
              (""     "textcomp"  t)
              (""     "marvosym"  t)
              (""     "wasysym"   t)
              (""     "latexsym"  t)
              (""     "amssymb"   t)
              (""     "hyperref"  nil)))

      ;; Packages to include when xelatex is used
      (if (string-match "LATEX_CMD: xelatex" (buffer-string))
          (setq org-export-latex-default-packages-alist
                '(("" "fontspec" t)
                  ("" "xltxtra" t)
                  ("" "fullpage" t)
                  ("" "listings" t)
                  ("" "url" t)
                  ("" "rotating" t)
                  ("american" "babel" t)
                  ("babel" "csquotes" t)
                  ("" "soul" t)
                  ("xetex,citecolor=black,linkcolor=black,urlcolor=black,colorlinks=true" "hyperref" nil)
                  )))

      (if (string-match "LATEX_CMD: xelatex" (buffer-string))
          (setq org-export-latex-classes
                (cons '("article"
                        "\\documentclass[12pt]{article}"
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                      org-export-latex-classes))))

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-parameters)

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation))
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(provide 'orgmode-settings)
