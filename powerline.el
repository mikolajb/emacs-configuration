(defvar powerline-color1)
(defvar powerline-color2)

(setq powerline-color1 "grey22")
(setq powerline-color2 "grey40")

(set-face-attribute 'mode-line nil
                    :background "#FCAF3E"
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
(setq-default
 mode-line-format
 (list '(:eval " %I")
       '(:eval (format " %s" buffer-file-coding-system))
       '(:eval (propertize " %b " 'face (powerline-make-face-bold)))
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
                           'display '(space :align-to (- right-fringe 20))
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
       '(:eval " %4l")
       '(:eval " %3c")))

(provide 'powerline)
