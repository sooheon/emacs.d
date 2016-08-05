(use-package clojure-mode
  :mode ("\\.boot\\'" . clojure-mode)
  :init
  (add-to-list 'magic-mode-alist '("#!.*boot\\s-*$" . clojure-mode))
  (add-to-list 'magic-mode-alist '("#!.*planck\\s-*$" . clojurescript-mode))
  :config
  ;; This is for clojure-semantic, the library file is clojure.el
  (load-library "clojure"))

;;;###autoload
(defun soo-clojure-hook ()
  (lispy-mode 1)
  (company-mode 1))

(use-package cider
  :init
  (add-hook 'clojure-mode-hook 'cider-mode)
  (setq cider-repl-pop-to-buffer-on-connect nil
        cider-prompt-save-file-on-load nil
        cider-repl-use-clojure-font-lock t
        cider-font-lock-dynamically t
        cider-mode-line '(:eval (format " [%s]" (cider--modeline-info)))
        cider-default-repl-command "boot")
  :config
  ;; (defadvice cider-jump-to-var (before add-evil-jump activate)
  ;;   (evil-set-jump))
  (evil-define-key 'normal cider-mode-map "K" 'cider-doc)
  (evil-set-initial-state 'cider-docview-mode 'insert)
  (evil-set-initial-state 'cider-stacktrace-mode 'insert)
  (add-hook 'cider-mode-hook 'eldoc-mode))

(use-package inf-clojure
  :config
  (setq inf-clojure-program "planck")
  (add-hook 'inf-clojure-mode-hook #'smartparens-mode))
