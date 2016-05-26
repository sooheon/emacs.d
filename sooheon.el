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
