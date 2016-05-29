(setq scroll-preserve-screen-position t
      scroll-margin 2
      scroll-conservatively 101)
(blink-cursor-mode -1)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key (kbd "s-u") 'universal-argument)
(global-set-key (kbd "s-W") 'delete-frame)
(bind-key "s-k" 'bury-buffer)
(bind-key "s-K" 'kill-this-buffer)
(define-key evil-normal-state-map "zf" '(lambda () (interactive)
                                          (reposition-window)
                                          (reposition-window)))
(global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)
(set-fontset-font "fontset-default" 'hangul '("NanumGothic" . "unicode-bmp"))
(setq-default fringe-indicator-alist '((truncation left-arrow right-arrow)
                                       (continuation
                                        nil ;; left-curly-arrow
                                        right-curly-arrow)
                                       (overlay-arrow . right-triangle)
                                       (up . up-arrow)
                                       (down . down-arrow)
                                       (top top-left-angle top-right-angle)
                                       (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
                                       (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
                                       (empty-line . empty-line)
                                       (unknown . question-mark)))

(defun spacemacs/smart-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(bind-key "C-a" #'spacemacs/smart-move-beginning-of-line)

(defun soo--delete-window-or-bury-buffer (&optional window)
  (interactive)
  (let ((window (window-normalize-window window)))
    (if (not (window-parent window))
        (call-interactively 'bury-buffer)
      (call-interactively 'delete-window))))

(bind-key "s-w" #'soo--delete-window-or-bury-buffer)
