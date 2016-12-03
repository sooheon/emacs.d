(general-define-key
 "C-x C-b" 'ibuffer
 "s-u" 'universal-argument
 "s-W" 'delete-frame
 "s-k" 'kill-this-buffer
 [remap move-beginning-of-line] 'smart-move-beginning-of-line
 "s-w" 'soo--close-window-dwim
 [remap fill-paragraph] 'endless/fill-or-unfill
 [remap just-one-space] 'oscillate-spacing
 [remap apropos-command] 'apropos)

(defun oscillate-spacing (&optional n)
  (interactive "*p")
  (cycle-spacing 1 nil 'fast))

(nmap "zf" '(lambda () (interactive)
              (reposition-window)
              (reposition-window))
      "goT" 'soo-terminal-pop-project-root
      "got" 'soo-terminal-pop)

(nmap :prefix "SPC"
  "t\C-o" 'sooheon--toggle-right-option-key
  "tl" 'global-hl-line-mode
  "sc" 'evil-ex-nohighlight
  "TAB" 'evil-buffer
  "u" 'universal-argument
  "wl" 'evil-window-right
  "wh" 'evil-window-left
  "wk" 'evil-window-top
  "wj" 'evil-window-bottom
  "wL" 'evil-window-move-far-right
  "wH" 'evil-window-move-far-left
  "wK" 'evil-window-move-very-top
  "wJ" 'evil-window-move-very-bottom
  "w=" 'balance-windows
  "wm" 'delete-other-windows
  "wo" 'other-window
  "ww" 'evil-window-next
  "ws" 'evil-window-split
  "wv" 'evil-window-vsplit
  "wr" 'evil-window-rotate-downwards
  "wR" 'evil-window-rotate-upwards)

;; This line actually replaces Emacs' entire narrowing keymap, that's how
;; much I like this command. Only copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)
(add-hook 'LaTeX-mode-hook (lambda () (define-key LaTeX-mode-map "\C-xn" nil)))

;; Other emacs-mac/OSX keybindings
(general-define-key
 "s-s" 'save-buffer
 "s-q" 'save-buffers-kill-terminal
 "s-v" 'yank
 "s-c" 'kill-ring-save
 "s-a" 'mark-whole-buffer
 "s-x" 'kill-region
 "s-n" 'make-frame
 "s-l" 'evil-avy-goto-line
 "C-s-f" 'toggle-frame-fullscreen
 "s--" 'dec-face-height
 "s-=" 'inc-face-height
 "M-DEL" 'evil-delete-backward-word)
(imap "C-o" 'evil-execute-in-normal-state
      "<s-backspace>" 'sooheon--delete-to-bol)
(define-key universal-argument-map (kbd "s-u") 'universal-argument)
