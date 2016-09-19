(general-define-key
 "C-x C-b" 'ibuffer
 "s-u" 'universal-argument
 "s-W" 'delete-frame
 "s-k" 'kill-this-buffer
 [remap move-beginning-of-line] 'smart-move-beginning-of-line
 "s-w" 'soo--close-window-dwim
 [remap fill-paragraph] 'endless/fill-or-unfill)

(nmap "zf" '(lambda () (interactive)
              (reposition-window)
              (reposition-window))
      "goT" 'soo-terminal-pop
      "got" 'soo-terminal-focus)

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

(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

;; This line actually replaces Emacs' entire narrowing keymap, that's how
;; much I like this command. Only copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)
(add-hook 'LaTeX-mode-hook (lambda () (define-key LaTeX-mode-map "\C-xn" nil)))

;; Other emacs-mac/OSX keybindings
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-n") 'make-frame)
(global-set-key (kbd "s-l") 'evil-avy-goto-line)
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
(evil-define-key 'insert global-map
  "\C-o" 'evil-execute-in-normal-state
  (kbd "<s-backspace>") 'sooheon--delete-to-bol)
(global-set-key (kbd "s--") 'dec-face-height)
(global-set-key (kbd "s-=") 'inc-face-height)
(define-key universal-argument-map (kbd "s-u") 'universal-argument)

;; Evil surround textobjects
(itomap "$" 'evil-inner-$
        "*" 'evil-inner-*
        "/" 'evil-inner-/)
(otomap "$" 'evil-outer-$
        "*" 'evil-outer-*
        "/" 'evil-outer-/)

(with-eval-after-load 'evil-surround
  (push '(42 "*" . "*") evil-surround-pairs-alist)
  (push '(36 "$" . "$") evil-surround-pairs-alist)
  (push '(47 "/" . "/") evil-surround-pairs-alist))

