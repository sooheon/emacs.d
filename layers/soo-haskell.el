(use-package intero
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook #'intero-mode)
  :config
  (define-key intero-repl-mode-map [tab] 'dabbrev-expand)
  (add-hook 'intero-repl-mode-hook #'smartparens-mode))

(use-package hindent
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook 'hindent-mode)
  :config
  (setq hindent-style "chris-done"))

(use-package shm
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  :config
  (define-key shm-map "\C-j" 'shm/newline-indent))

;; Smartparens
(add-hook 'smartparens-mode-hook
          (lambda ()
            (add-to-list 'sp-no-reindent-after-kill-modes #'haskell-mode)
            (require 'smartparens-haskell)))
(add-hook 'haskell-mode-hook 'smartparens-mode)

(provide 'soo-haskell)
