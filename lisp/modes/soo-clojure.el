(use-package clojure-mode
  :defer t
  :mode ("\\.boot\\'" . clojure-mode)
  :init
  (add-to-list 'magic-mode-alist '("#!.*boot\\s-*$" . clojure-mode))
  (add-to-list 'magic-mode-alist '("#!.*planck\\s-*$" . clojurescript-mode))
  :config
  ;; This is for clojure-semantic, the library file is clojure.el
  (load-library "clojure"))

(use-package clj-refactor
  :defer t
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  :config
  (setq cljr-favor-prefix-notation nil)
  (cljr-add-keybindings-with-prefix "s-,"))

;;;###autoload
(defun soo-clojure-hook ()
  (lispy-mode 1)
  (company-mode 1))

(use-package cider
  :defer t
  :config
  (setq cider-prompt-for-symbol nil
        nrepl-hide-special-buffers t
        cider-repl-pop-to-buffer-on-connect nil
        cider-prompt-save-file-on-load 'always-save
        cider-repl-use-clojure-font-lock t
        cider-font-lock-dynamically t
        cider-mode-line '(:eval (format " %s" (cider--modeline-info)))
        cider-default-repl-command "boot"
        cider-pprint-fn 'puget)
  (defun cider-figwheel-repl ()
    (interactive)
    (setq-local cider-cljs-lein-repl "(do (require 'figwheel-sidecar.repl-api)
                                          (figwheel-sidecar.repl-api/start-figwheel!)
                                          (figwheel-sidecar.repl-api/cljs-repl))")
    (cider-jack-in-clojurescript))
  (general-define-key :keymaps 'cider-mode-map
    "C-c C-M-j" 'cider-figwheel-repl)
  ;; (defadvice cider-jump-to-var (before add-evil-jump activate)
  ;;   (evil-set-jump))
  (add-hook 'cider-repl-mode-hook 'smartparens-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (evil-define-key 'normal cider-mode-map "K" 'cider-doc)
  (evil-set-initial-state 'cider-docview-mode 'insert)
  (evil-set-initial-state 'cider-stacktrace-mode 'insert)
  (evil-set-initial-state 'cider-macroexpansion-mode 'insert)
  (evil-set-initial-state 'cider-browse-ns-mode 'insert)
  (evil-set-initial-state 'cider-test-report-mode 'insert))

(use-package inf-clojure
  :defer t
  :config
  (setq inf-clojure-program "planck")
  (add-hook 'inf-clojure-mode-hook #'smartparens-mode)
  (general-define-key :keymaps 'clojurescript-mode-map
    "C-c C-k" 'inf-clojure-eval-buffer
    "C-c C-e" 'inf-clojure-eval-last-sexp
    "C-M-x" 'inf-clojure-eval-defun))

(provide 'soo-clojure)
