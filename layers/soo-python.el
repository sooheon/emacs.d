(defvar no-pip
  (string-match "Command not found\\|no pip in"
                (shell-command-to-string "which pip")))
(unless no-pip
  (use-package jedi
    :defer t
    :init
    (setq python-environment-directory "~/.pyenv/versions")
    :config
    (define-key jedi-mode-map [C-tab] nil)
    (setq jedi:use-shortcuts nil)
    (setq jedi:complete-on-dot nil)
    (setq jedi:setup-function nil)
    (setq jedi:mode-function nil)
    ;; (define-key jedi-mode-map (kbd "M-.") 'jedi:goto-definition)
    ;; (define-key jedi-mode-map (kbd "K") 'jedi:show-doc)
    ))

(use-package function-args
  :defer t
  :config
  (fa-config-default)
  (set-default 'semantic-case-fold t))

(use-package python
  :defer t
  :config
  (define-key python-mode-map (kbd "C-j") 'newline-and-indent)
  (define-key python-mode-map (kbd "C-m") 'newline))

(require 'lpy)

(use-package lpy
  :defer t
  :init
  (add-hook 'python-mode-hook 'lpy-mode)
  :config
  (define-key lpy-mode-map (kbd "C-h") nil)
  (define-key lpy-mode-map (kbd "[") nil)
  (define-key lpy-mode-map (kbd "(") nil)
  (define-key lpy-mode-map (kbd "M-C-b") 'lispy-backward)
  (define-key lpy-mode-map (kbd "M-C-f") 'lispy-forward))

(use-package pyenv-mode
  :if (executable-find "pyenv")
  :commands (pyenv-mode-set pyenv-mode-unset)
  :init
  (require 'pyenv-mode-auto))

(use-package company-jedi
  :disabled t
  :init
  (add-hook 'python-mode-hook
            '(lambda () (add-to-list 'company-backends 'company-jedi))))

(use-package anaconda-mode
  :defer t
  :diminish anaconda-mode
  :init
  (setq anaconda-mode-installation-directory (expand-file-name "etc/anaconda-mode" emacs-d))
  (add-hook 'python-mode-hook 'anaconda-mode)
  :config
  (evil-define-key 'normal anaconda-mode-map "K" 'anaconda-mode-show-doc)
  (evilified-state-evilify anaconda-mode-view-mode anaconda-mode-view-mode-map
    (kbd "q") 'quit-window)
  (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
    (evil--jumps-push))
  (require 'company-anaconda)
  (add-to-list 'company-backends 'company-anaconda)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package py-yapf
  :commands py-yapf-buffer
  :init
  (evil-leader/set-key-for-mode 'python-mode "=" 'py-yapf-buffer))

(provide 'soo-python)
