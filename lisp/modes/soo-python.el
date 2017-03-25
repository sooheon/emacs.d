(use-package python
  :ensure nil
  :general
  (:keymaps 'python-mode-map
   "C-j" 'newline-and-indent
   "C-m" 'newline)
  :config
  (setq python-shell-completion-native-enable nil)
  (add-hook 'inferior-python-mode-hook 'company-mode)
  (add-hook 'inferior-python-mode-hook 'smartparens-mode)
  (nmap :keymaps 'inferior-python-mode-map "C-d" 'evil-scroll-down))

(use-package py-yapf
  :commands py-yapf-buffer
  :general
  (:keymaps 'python-mode-map "C-c C-c" 'py-yapf-and-send-buffer)
  :init
  (nvmap :prefix "SPC"
         :keymaps 'python-mode-map
         "=" 'py-yapf-buffer)
  :config
  (defun py-yapf-and-send-buffer ()
    (interactive)
    (py-yapf-buffer)
    (python-shell-send-buffer)))

(use-package jedi
  :defer t
  :config
  (setq python-environment-directory "~/.pyenv/versions")
  (define-key jedi-mode-map [C-tab] nil)
  (setq jedi:use-shortcuts nil)
  (setq jedi:complete-on-dot nil)
  (setq jedi:setup-function nil)
  (setq jedi:mode-function nil)
  ;; (define-key jedi-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key jedi-mode-map (kbd "K") 'jedi:show-doc))

(use-package lpy
  :disabled t
  :ensure nil
  :diminish (lpy-mode . "lpy")
  :after lispy
  :init
  :config
  (general-define-key :keymaps 'lpy-mode-map
    "C-h" nil
    "[" nil
    "(" nil
    "+" nil
    "-" nil
    "=" nil
    ">" nil
    "<" nil
    "," nil
    "\"" nil))

(use-package anaconda-mode
  :defer t
  :diminish anaconda-mode
  :config
  (evil-define-key 'normal anaconda-mode-map "K" 'anaconda-mode-show-doc)
  (evil-set-initial-state 'anaconda-mode-view-mode 'insert)
  (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
    (evil--jumps-push)))

(use-package company-anaconda
  :defer t
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package ein
  :defer t
  :config
  (setq ein:console-args "--simple-prompt"))

(use-package hy-mode
  :defer t
  :init
  (add-hook 'hy-mode-hook 'lispy-mode)
  :config
  (add-to-list 'sp-lisp-modes 'hy-mode))

;;;###autoload
(defun soo-python-hook ()
  (anaconda-mode 1)
  (anaconda-eldoc-mode 1))

(add-hook 'python-mode-hook 'soo-python-hook)

(provide 'soo-python)
