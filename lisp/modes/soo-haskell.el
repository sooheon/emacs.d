(use-package haskell-mode
  :defer t
  :config
  (defun haskell-load-file-and-switch-to-repl ()
    (interactive)
    (haskell-process-load-file)
    (haskell-interactive-switch)
    (evil-insert 1))
  (bind-key "C-c C-l" 'haskell-load-file-and-switch-to-repl haskell-mode-map)
  (bind-key "C-c C-z" 'haskell-interactive-switch haskell-mode-map)
  (setq haskell-ask-also-kill-buffers nil
        haskell-process-show-debug-tips nil
        haskell-interactive-popup-errors nil)
  (setq tab-width 4))

(use-package intero
  :defer t
  :diminish (intero-mode . "in")
  :config
  (define-key intero-repl-mode-map [tab] 'dabbrev-expand)
  (add-hook 'intero-repl-mode-hook #'smartparens-strict-mode)
  (evil-define-key 'normal intero-mode-map "K" 'intero-info)
  (unbind-key "C-c C-l" intero-mode-map)
  (unbind-key "C-c C-z" intero-mode-map)
  ;; (bind-key "C-c C-l" 'intero-repl-load intero-mode-map)
  ;; (bind-key "C-c C-z" 'intero-repl-buffer intero-mode-map)
  )

(use-package hindent
  :defer t
  :diminish hindent-mode
  :commands (hindent-reformat-buffer hindent-reformat-decl)
  :config
  (nvmap :prefix "SPC"
         :keymaps 'hindent-mode-map
         "=" 'hindent-reformat-buffer)
  (evil-define-key 'visual hindent-mode-map "=" 'hindent-reformat-region)
  (setq hindent-process-path "/Users/sooheon/.local/bin/hindent"))

(use-package shm
  :defer t
  :load-path "/Users/sooheon/.emacs.d/lib/structured-haskell-mode/elisp"
  :config
  (setq shm-program-name (expand-file-name "structured-haskell-mode/dist/build/structured-haskell-mode/structured-haskell-mode" lib-d))
  ;; (define-key shm-map (kbd ")") nil)
  (define-key shm-map (kbd "DEL") nil)
  (bind-key [remap yank] 'shm/yank shm-map))

;; Smartparens
(add-hook 'smartparens-mode-hook
          (lambda ()
            (add-to-list 'sp-no-reindent-after-kill-modes #'haskell-mode)
            (require 'smartparens-haskell)))

(add-hook 'haskell-interactive-mode-hook 'smartparens-strict-mode)
(add-hook 'haskell-interactive-mode-hook 'company-mode)
(evil-define-key 'normal haskell-interactive-mode-map
  [return] 'haskell-interactive-mode-return)
(evil-define-key 'normal haskell-interactive-mode-map
  (kbd "RET") 'haskell-interactive-mode-return)

;;;###autoload
(defun soo-haskell-hook ()
  (smartparens-strict-mode)
  (intero-mode)
  (hindent-mode)
  (structured-haskell-mode)
  (haskell-indentation-mode -1)
  (setq evil-auto-indent nil))

(add-hook 'haskell-mode-hook 'soo-haskell-hook)
