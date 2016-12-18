(use-package evil
  :general
  (imap "C-w" 'evil-delete-backward-word)
  :init
  (setq-default evil-want-C-u-scroll t
                evil-cross-lines t
                evil-symbol-word-search t
                evil-move-cursor-back t
                evil-want-C-i-jump t
                evil-disable-insert-state-bindings t
                evil-search-module 'evil-search
                evil-ex-search-persistent-highlight t
                evil-want-Y-yank-to-eol t
                evil-ex-substitute-global t
                evil-want-C-w-delete t)
  :config
  (evil-mode)
  (setq evil-ex-search-highlight-all t)
  (defun blink-cursor-on () (blink-cursor-mode 1))
  (defun blink-cursor-off () (blink-cursor-mode -1))
  (add-hook 'evil-insert-state-entry-hook 'blink-cursor-on)
  (add-hook 'evil-insert-state-exit-hook 'blink-cursor-off)
  (add-hook 'evil-emacs-state-entry-hook 'blink-cursor-on)
  (add-hook 'evil-emacs-state-exit-hook 'blink-cursor-off)

  (progn ;; HACK: visual edits can be repeated with `.'
    ;; make v start recording
    (evil-set-command-property 'evil-visual-char :repeat t)
    (evil-set-command-property 'evil-visual-line :repeat t)
    (evil-set-command-property 'evil-visual-block :repeat t)

    ;; it would be better to advise these functions with :override
    (defun evil-repeat-motion (flag)
      "Repeation for motions. Motions are recorded by keystroke but only in insert state."
      ;; also record motions in visual state
      (when (memq evil-state '(insert replace visual))
        (evil-repeat-keystrokes flag)))

    (defun evil-repeat-start ()
      "Start recording a new repeat into `evil-repeat-info'."
      ;; don't stop recording in visual state
      (unless (evil-visual-state-p)
        (evil-repeat-reset t)
        (evil-repeat-record-buffer)))

    (defun evil-repeat-stop ()
      "Stop recording a repeat.
Update `evil-repeat-ring' with the accumulated changes
in `evil-repeat-info' and clear variables."
      ;; don't stop recording in visual state
      (unless (evil-visual-state-p)
        (unwind-protect
            (when (and (evil-repeat-recording-p))
              (setq evil-repeat-info
                    (evil-normalize-repeat-info evil-repeat-info))
              (when (and evil-repeat-info evil-repeat-ring)
                (ring-insert evil-repeat-ring evil-repeat-info)))
          (evil-repeat-reset nil))))))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :general (nmap "gc" 'evil-commentary
                 "gy" 'evil-commentary-yank))

(use-package evil-cleverparens
  :general
  (otomap "f" 'evil-cp-a-form
          "c" 'evil-cp-a-comment
          "d" 'evil-cp-a-defun)
  (itomap "f" 'evil-cp-inner-form
          "c" 'evil-cp-inner-comment
          "d" 'evil-cp-inner-defun))

(use-package evil-exchange
  :diminish evil-exchange
  :general
  (omap "x" 'evil-exchange/cx)
  (vmap "X" 'evil-exchange))

(use-package evil-matchit
  :defer t
  :init
  (add-hook 'html-mode-hook 'turn-on-evil-matchit-mode)
  (add-hook 'ruby-mode-hook 'turn-on-evil-matchit-mode))

(use-package evil-textobj-anyblock
  :general
  (itomap "b" 'evil-textobj-anyblock-inner-block)
  (otomap "b" 'evil-textobj-anyblock-a-block))

(use-package evil-visualstar
  :general
  (vmap "*" 'evil-visualstar/begin-search-forward
        "#" 'evil-visualstar/begin-search-backward))

(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :init
  (setq evil-snipe-use-vim-sneak-bindings t
        evil-snipe-scope 'visible
        evil-snipe-repeat-scope 'buffer
        evil-snipe-smart-case t
        evil-snipe-repeat-keys nil
        evil-snipe-char-fold t)
  :config
  (evil-snipe-override-mode 1))

(use-package evil-surround
  :general
  (itomap "$" 'evil-inner-$
          "*" 'evil-inner-*
          "/" 'evil-inner-/)
  (otomap "$" 'evil-outer-$
          "*" 'evil-outer-*
          "/" 'evil-outer-/)
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

(provide 'soo-evil)
