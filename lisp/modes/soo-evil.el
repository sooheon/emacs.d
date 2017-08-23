(use-package evil
  :ensure t
  :general
  (imap "C-w" 'evil-delete-backward-word
        "C-r" 'evil-paste-from-register)
  (nvmap "C-n" 'next-line
         "C-p" 'previous-line)
  :init
  (setq evil-want-C-u-scroll t
        evil-scroll-line-count 2
        evil-cross-lines t
        evil-symbol-word-search t
        evil-move-cursor-back t
        evil-want-C-i-jump t
        evil-disable-insert-state-bindings t
        evil-search-module 'evil-search
        evil-ex-search-persistent-highlight nil
        evil-want-Y-yank-to-eol t
        evil-ex-substitute-global t
        evil-want-C-w-delete t)
  :config
  (evil-mode)
  (setq evil-ex-search-highlight-all t)
  (general-define-key :keymaps 'evil-normal-state-map "M-." nil)
  ;; Let underscores be part of words
  (add-hook 'text-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w"))))

(use-package evil-commentary
  :ensure t
  :diminish evil-commentary-mode
  :general (nmap "gc" 'evil-commentary
                 "gy" 'evil-commentary-yank))

(use-package evil-exchange
  :ensure t
  :diminish evil-exchange
  :general
  (omap "x" 'evil-exchange/cx)
  (vmap "X" 'evil-exchange))

(use-package evil-matchit
  :ensure t
  :defer t
  :init
  (add-hook 'html-mode-hook 'turn-on-evil-matchit-mode)
  (add-hook 'ruby-mode-hook 'turn-on-evil-matchit-mode))

(use-package evil-textobj-anyblock
  :ensure t
  :general
  (itomap "b" 'evil-textobj-anyblock-inner-block)
  (otomap "b" 'evil-textobj-anyblock-a-block))

(use-package evil-visualstar
  :ensure t
  :general
  (vmap "*" 'evil-visualstar/begin-search-forward
        "#" 'evil-visualstar/begin-search-backward))

(use-package evil-snipe
  :ensure t
  :diminish evil-snipe-local-mode
  :init
  (setq evil-snipe-use-vim-sneak-bindings t
        evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'buffer
        evil-snipe-smart-case t
        evil-snipe-repeat-keys nil
        evil-snipe-char-fold t)
  :config
  (evil-snipe-mode)
  (evil-snipe-override-mode))

(use-package evil-surround
  :ensure t
  :general
  (itomap "$" 'evil-inner-$
          "*" 'evil-inner-*
          "/" 'evil-inner-/)
  (otomap "$" 'evil-outer-$
          "*" 'evil-outer-*
          "/" 'evil-outer-/)
  (vmap :keymaps 'evil-surround-mode-map
    "s" 'evil-surround-region
    "S" 'evil-surround-region)
  :init
  (global-evil-surround-mode 1)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))
  ;; Define new text objects
  (push '(42 "*" . "*") evil-surround-pairs-alist)
  (push '(36 "$" . "$") evil-surround-pairs-alist)
  (push '(47 "/" . "/") evil-surround-pairs-alist)
  (vmap :keymaps 'evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-text-object evil-inner-$ (count &optional beg end type)
    (evil-select-paren "\\$" "\\$" beg end type count nil))
  (evil-define-text-object evil-outer-$ (count &optional beg end type)
    (evil-select-paren "\\$" "\\$" beg end type count t))
  (evil-define-text-object evil-inner-* (count &optional beg end type)
    (evil-select-paren "\\*" "\\*" beg end type count nil))
  (evil-define-text-object evil-outer-* (count &optional beg end type)
    (evil-select-paren "\\*" "\\*" beg end type count t))
  (evil-define-text-object evil-inner-/ (count &optional beg end type)
    (evil-select-paren "\\/" "\\/" beg end type count nil))
  (evil-define-text-object evil-outer-/ (count &optional beg end type)
    (evil-select-paren "\\/" "\\/" beg end type count t)))

(use-package evil-numbers
  :general
  (nmap "C-S-a" 'evil-numbers/inc-at-pt
        "C-S-x" 'evil-numbers/dec-at-pt))

(use-package sentence-navigation
 :disabled t
 :defer t
 :general
 (mmap ")" 'sentence-nav-evil-forward
       "(" 'sentence-nav-evil-backward)
  (nmap "g)" 'sentence-nav-evil-forward-end
        "g(" 'sentence-nav-evil-backward-end)
  (otomap "s" 'sentence-nav-evil-a-sentence)
  (itomap "s" 'sentence-nav-evil-inner-sentence))

(use-package evil-multiedit
  :disabled t
  :general
  (nvmap "s-d" 'evil-multiedit-match-and-next
         "s-D" 'evil-multiedit-match-and-prev)
  (imap "s-d" 'evil-multiedit-toggle-marker-here)
  (:keymaps '(evil-multiedit-state-map evil-multiedit-insert-state-map)
   "<return>" 'evil-multiedit-toggle-or-restrict-region
   "C-n" 'evil-multiedit-next
   "C-p" 'evil-multiedit-prev)
  (mmap "<return>" 'evil-multiedit-toggle-or-restrict-region))

(use-package evil-mc
  :disabled t
  :init
  (global-evil-mc-mode 1)
  :general
  ("s-d" #'evil-mc-make-cursor-here))

(blink-cursor-mode -1)
(use-package frame
  :disabled t
  :ensure nil
  :init
  (setq blink-cursor-blinks 0
        blink-cursor-delay 0)
  (defun blink-cursor-mode-on () (blink-cursor-mode 1))
  (defun blink-cursor-mode-off () (blink-cursor-mode -1))
  (add-hook 'evil-insert-state-entry-hook 'blink-cursor-mode-on)
  (add-hook 'evil-insert-state-exit-hook 'blink-cursor-mode-off)
  (add-hook 'evil-normal-state-entry-hook 'blink-cursor-mode-off)
  (add-hook 'evil-visual-state-entry-hook 'blink-cursor-mode-off))

(provide 'soo-evil)
