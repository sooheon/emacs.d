(use-package rust-mode
  :defer t
  :init
  (defun soo-rust-hook ()
    (racer-mode)
    (eldoc-mode)
    (hungry-delete-mode))
  (add-hook 'rust-mode-hook #'soo-rust-hook)
  :config
  (nvmap :prefix "SPC"
         :keymaps 'rust-mode-map
         "=" 'rust-format-buffer))

(use-package racer
  :diminish racer-mode
  :defer t
  :init
  :config
  (nmap :keymaps 'racer-mode-map "K" 'racer-describe)
  (setq racer-rust-src-path "/usr/local/src/rust/rustc-1.11.0/src/")
  (evil-set-initial-state 'racer-help-mode 'emacs))

(use-package cargo
  :defer t
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package toml-mode
  :defer t
  :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'")

(with-eval-after-load 'smartparens
  (sp-local-pair 'rust-mode "'" nil :actions nil))
