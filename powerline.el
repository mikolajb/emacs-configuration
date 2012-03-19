(defvar powerline-color1)
(defvar powerline-color2)

(setq powerline-color1 "#888A85")
(setq powerline-color2 "#555753")

(set-face-attribute 'mode-line nil
                    :background "#E9B96E"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :box nil)

(defun arrow-right-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (create-image
   (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\".           \",
\"..          \",
\"...         \",
\"....        \",
\".....       \",
\"......      \",
\".......     \",
\"........    \",
\".........   \",
\".........   \",
\"........    \",
\".......     \",
\"......      \",
\".....       \",
\"....        \",
\"...         \",
\"..          \",
\".           \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun arrow-left-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (create-image
   (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\"           .\",
\"          ..\",
\"         ...\",
\"        ....\",
\"       .....\",
\"      ......\",
\"     .......\",
\"    ........\",
\"   .........\",
\"   .........\",
\"    ........\",
\"     .......\",
\"      ......\",
\"       .....\",
\"        ....\",
\"         ...\",
\"          ..\",
\"           .\"};"
           (if color2 color2 "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defvar powerline-minor-modes nil)
(defun powerline-make-face
  (bg &optional fg)
  (if bg
      (let ((cface (intern (concat "powerline-" bg))))
        (make-face cface)
        (set-face-attribute cface nil
                            :foreground (if fg fg "white")
                            :background bg
                            :box nil)
        cface)
    nil))

(defun powerline-make-face-bold ()
  (let ((cface (intern "powerline-bold")))
        (make-face cface)
        (set-face-attribute cface nil
                            :weight 'bold
                            :box nil)
        cface))

(defun powerline-make-face-italics ()
  (let ((cface (intern "powerline-italics")))
        (make-face cface)
        (set-face-attribute cface nil
                            :slant 'italic
                            :box nil)
        cface))

(setq-default
 mode-line-format
 (list '(:eval (propertize (format " %s" buffer-file-coding-system)
                           'face (powerline-make-face-italics)))
       '(:eval (propertize " %b "
                           'face (powerline-make-face-bold)))
       '(:eval (propertize " "
                           'display (arrow-right-xpm nil powerline-color1)))
       ;; =>
       '(:eval
         (propertize " %[%m%] "
                     'face (powerline-make-face powerline-color1)))
       '(:eval (propertize
                (substring (format-mode-line minor-mode-alist) 1)
                'face (powerline-make-face powerline-color1)))
       '(:eval (propertize " "
                           'display (arrow-right-xpm
                                     powerline-color1
                                     powerline-color2)))
       ;; =>
       '(:eval (propertize "%n"
                           'face (powerline-make-face powerline-color2)))
       '(:eval (if (buffer-file-name (current-buffer))
                   (propertize (format "%s" (vc-mode-line (buffer-file-name (current-buffer))))
                           'face (powerline-make-face powerline-color2))
                 ""))
       '(:eval (propertize " "
                           'display '(space :align-to (- right-fringe 27))
                           'face (powerline-make-face powerline-color2)))
       ;; <=
       '(:eval (propertize " "
                           'display (arrow-left-xpm
                                     powerline-color2
                                     powerline-color1)))
       '(:eval (propertize " %6p "
                           'face (powerline-make-face powerline-color1)))
       ;; <=
       '(:eval (propertize " "
                           'display (arrow-left-xpm
                                     powerline-color1
                                     nil)))
       '(:eval (propertize " %5I"
                           'face (powerline-make-face-italics)))
       '(:eval " %4l")
       '(:eval " %3c")))

(provide 'powerline)
