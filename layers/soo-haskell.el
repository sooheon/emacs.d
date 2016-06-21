(use-package intero
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook #'intero-mode)
  :config
  (define-key intero-repl-mode-map [tab] 'dabbrev-expand))

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
  (add-hook 'haskell-mode-hook 'structured-haskell-mode))

;; Smartparens
(add-hook 'smartparens-mode-hook
          (lambda ()
            (add-to-list 'sp-no-reindent-after-kill-modes #'haskell-mode)
            (require 'smartparens-haskell)))

(provide 'soo-haskell)
