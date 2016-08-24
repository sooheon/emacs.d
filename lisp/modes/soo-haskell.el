(use-package haskell-mode
  :config
  (bind-key "C-c C-l" 'haskell-process-load-file haskell-mode-map)
  (bind-key "C-c C-z" 'haskell-interactive-switch haskell-mode-map)
  (setq haskell-ask-also-kill-buffers nil
        haskell-process-show-debug-tips nil
        haskell-interactive-popup-errors nil))

(use-package intero
  :diminish (intero-mode . "int")
  :defer t
  :config
  (define-key intero-repl-mode-map [tab] 'dabbrev-expand)
  (add-hook 'intero-repl-mode-hook #'smartparens-mode)
  (evil-define-key 'normal intero-mode-map "K" 'intero-info)
  (unbind-key "C-c C-l" intero-mode-map)
  (unbind-key "C-c C-z" intero-mode-map))

(use-package hindent
  :diminish hindent-mode
  :bind (:map hindent-mode-map
              ("C-c i" . hindent-reformat-buffer))
  :config
  (setq hindent-style "chris-done"))

(use-package shm
  :disabled t
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

;;;###autoload
(defun soo-haskell-hook ()
  (intero-mode)
  (hindent-mode))
