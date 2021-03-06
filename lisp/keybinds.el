(setq mac-pass-command-to-system nil
      ;; mac-right-command-modifier 'control
      mac-command-modifier 'super
      mac-option-modifier 'meta)

(general-define-key
 "C-h C-h" nil
 "C-x C-b" 'ibuffer
 "C-x k" 'kill-this-buffer
 "s-u" 'universal-argument
 "s-w" 'soo--close-window-dwim
 "C-s-w" 'bury-buffer
 [remap move-beginning-of-line] 'smart-move-beginning-of-line
 [remap fill-paragraph] 'endless/fill-or-unfill
 [remap just-one-space] 'oscillate-spacing
 [remap apropos-command] 'apropos
 "s-C" 'count-words
 "s-g" 'keyboard-quit
 "s-h" 'evil-window-left
 "s-j" 'evil-window-down
 "s-k" 'evil-window-up
 "s-l" 'evil-window-right
 "C-s-h" 'evil-window-move-far-left
 "C-s-j" 'evil-window-move-very-bottom
 "C-s-k" 'evil-window-move-very-top
 "C-s-l" 'evil-window-move-far-right
 [C-s-268632072] 'evil-window-move-far-left
 [C-s-268632074] 'evil-window-move-very-bottom
 [C-s-268632075] 'evil-window-move-very-top
 [C-s-268632076] 'evil-window-move-far-right
 "<c-tab>" 'other-window
 "s-s" 'save-buffer
 "s-q" 'save-buffers-kill-terminal
 "s-v" 'yank
 "s-c" 'kill-ring-save
 "s-a" 'mark-whole-buffer
 "s-x" 'kill-region
 "s-z" 'undo
 "s-n" 'make-frame
 "C-s-f" 'toggle-frame-fullscreen
 [C-s-268632070] 'toggle-frame-fullscreen
 "s--" 'dec-face-height
 "s-=" 'inc-face-height
 "s-0" 'zero-face-height
 "s-[" 'previous-buffer
 "s-]" 'next-buffer
 "M-DEL" 'backward-kill-word
 "s-o" 'find-file)

(defun oscillate-spacing (&optional n)
  (interactive "*p")
  (cycle-spacing 1 nil 'fast))

(nmap "zf" '(lambda () (interactive)
              (reposition-window)
              (reposition-window))
      ;; "got" 'soo-terminal-pop
      )

(nvmap :prefix "SPC"
  "t\C-o" 'sooheon--toggle-right-option-key
  "tl" 'global-hl-line-mode
  "sc" 'evil-ex-nohighlight
  "TAB" 'evil-buffer
  "u" 'universal-argument
  "wl" 'evil-window-right
  "wh" 'evil-window-left
  "wk" 'evil-window-up
  "wj" 'evil-window-down
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
  "wR" 'evil-window-rotate-upwards
  "wc" 'soo--close-window-dwim
  "ESC" nil)

(mmap "<down>" 'next-line
      "<up>" 'previous-line)

;; This line actually replaces Emacs' entire narrowing keymap, that's how
;; much I like this command. Only copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)
(add-hook 'LaTeX-mode-hook (lambda () (define-key LaTeX-mode-map "\C-xn" nil)))

(imap "C-o" 'evil-execute-in-normal-state
      "<s-backspace>" 'sooheon--delete-to-bol)
(define-key universal-argument-map (kbd "s-u") 'universal-argument)

(general-define-key :keymaps 'minibuffer-local-map
  "C-p" 'previous-history-element
  "C-n" 'next-history-element)
