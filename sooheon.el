(setq scroll-preserve-screen-position t)
(blink-cursor-mode -1)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key (kbd "s-u") 'universal-argument)
(global-set-key (kbd "s-w") 'delete-window)
(global-set-key (kbd "s-W") 'delete-frame)
(global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)
(add-to-list 'default-frame-alist '(font . "Input Mono Narrow-12"))
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

(progn ;; Themes
  (dolist (elt '("themes" "lib/zenburn-theme" "lib/solarized-theme"
                 "lib/ample-theme" "lib/spacemacs-theme"
                 "lib/emacs-doom-theme"))
    (add-to-list 'custom-theme-load-path
                 (expand-file-name elt user-emacs-directory)))
  (setq custom-safe-themes t)
  (load-theme 'solarized-dark t))

(defun sooheon--gc-setup-minibuffer-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun sooheon--gc-exit-minibuffer-hook ()
  (setq gc-cons-threshold 8000000))
(add-hook 'minibuffer-setup-hook #'sooheon--gc-setup-minibuffer-hook)
(add-hook 'minibuffer-exit-hook #'sooheon--gc-exit-minibuffer-hook)
