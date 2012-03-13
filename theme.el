(load-theme 'tango-dark)

(defun select-cursor-color-according-to-mode ()
  (blink-cursor-mode 0)
  (cond
   (buffer-read-only
    (set-cursor-color "DodgerBlue"))
   (overwrite-mode
    (set-cursor-color "orange"))
   ((buffer-modified-p)
    (set-cursor-color "#D4213D"))
   (t
    (set-cursor-color "#E9E8E7"))))
(add-hook 'post-command-hook 'select-cursor-color-according-to-mode)

;;; terminal colors
(setq ansi-color-for-comint-mode t)
(setq ansi-color-for-comint-mode-on t)
(setq ansi-color-names-vector
      ["#000000"           ; black
       "#CC0000"           ; red
       "#4E9A06"           ; green
       "#C4A000"           ; yellow
       "#3465A4"           ; blue
       "#75507B"           ; magenta
       "#06989A"           ; cyan
       "#D3D7CF"]          ; white
      )
(setq ansi-term-color-vector
      [unspecified
       "#555753"           ; black
       "#EF2929"           ; red
       "#8AE234"           ; green
       "#FCE94F"           ; yellow
       "#729FCF"           ; blue
       "#AD7FA8"           ; magenta
       "#34E2E2"           ; cyan
       "#EEEEEC"]          ; white
      )

(provide 'theme)
