(use-package rust-mode
  :defer t
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (setq-local company-tooltip-align-annotations t)))
  (add-hook 'rust-mode-hook 'eldoc-mode))

(use-package racer
  :disabled t
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
