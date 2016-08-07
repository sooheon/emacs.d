(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key (kbd "s-u") 'universal-argument)
(global-set-key (kbd "s-W") 'delete-frame)
(bind-key "s-k" 'kill-this-buffer)
;; (bind-key "s-K" 'kill-this-buffer)
(define-key evil-normal-state-map "zf" '(lambda () (interactive)
                                          (reposition-window)
                                          (reposition-window)))
(global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)
(set-fontset-font "fontset-default" 'hangul '("NanumGothic" . "unicode-bmp"))
(evil-leader/set-key "t\C-o" 'sooheon--toggle-right-option-key)

(global-set-key [remap move-beginning-of-line] 'smart-move-beginning-of-line)

(bind-key "s-w" 'soo--close-window-dwim)

(bind-key [remap fill-paragraph] 'endless/fill-or-unfill)
(global-set-key (kbd "C-c d") 'counsel-descbinds)

(define-key evil-normal-state-map "got" 'soo--terminal-pop)

(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

;; This line actually replaces Emacs' entire narrowing keymap, that's how
;; much I like this command. Only copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)
(add-hook 'LaTeX-mode-hook (lambda () (define-key LaTeX-mode-map "\C-xn" nil)))


;; Other emacs-mac/OSX keybindings
(bind-key "s-s" 'save-buffer)
(bind-key "s-q" 'save-buffers-kill-terminal)
(bind-key "s-v" 'yank)
(bind-key "s-c" 'evil-yank)
(bind-key "s-a" 'mark-whole-buffer)
(bind-key "s-x" 'kill-region)
(bind-key "s-n" 'make-frame)
(bind-key "s-l" 'evil-avy-goto-line)
(bind-key "C-s-f" 'toggle-frame-fullscreen)
(evil-define-key 'insert global-map
  "\C-o" 'evil-execute-in-normal-state
  (kbd "<s-backspace>") 'sooheon--delete-to-bol)
(bind-key "s--" 'dec-face-height)
(bind-key "s-=" 'inc-face-height)

;; Evil surround textobjects
(with-eval-after-load 'evil
  (define-key evil-inner-text-objects-map "$" 'evil-inner-$)
  (define-key evil-outer-text-objects-map "$" 'evil-outer-$)

  (define-key evil-inner-text-objects-map "*" 'evil-inner-*)
  (define-key evil-outer-text-objects-map "*" 'evil-outer-*)

  (define-key evil-inner-text-objects-map "/" 'evil-inner-/)
  (define-key evil-outer-text-objects-map "/" 'evil-outer-/))

(with-eval-after-load 'evil-surround
  (push '(42 "*" . "*") evil-surround-pairs-alist)

  (push '(36 "$" . "$") evil-surround-pairs-alist)

  (push '(47 "/" . "/") evil-surround-pairs-alist))
