(use-package rust-mode
  :defer t
  :init
  (add-hook 'rust-mode-hook #'eldoc-mode)
  :config
  (evil-leader/set-key-for-mode 'rust-mode "=" 'rust-format-buffer))

(use-package racer
  :diminish racer-mode
  :defer t
  :init
  (add-hook 'rust-mode-hook 'racer-mode)
  :config
  (setq racer-rust-src-path "/usr/local/src/rust/rustc-1.11.0/src/"))

(use-package cargo
  :defer t
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode))

(with-eval-after-load 'smartparens
  (sp-local-pair 'rust-mode "'" nil :actions nil))

(provide 'soo-rust)
