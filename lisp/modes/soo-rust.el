(use-package rust-mode :ensure t :defer t)

(use-package racer
  :ensure t
  :diminish racer-mode
  :defer t
  :init
  (add-hook 'rust-mode-hook 'racer-mode)
  :config
  (setq racer-rust-src-path "/usr/local/src/rust/rustc-1.11.0/src/"))

(use-package cargo
  :ensure t
  :defer t
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode))

(with-eval-after-load 'smartparens
  (sp-local-pair 'rust-mode "'" nil :actions nil))

(provide 'soo-rust)
