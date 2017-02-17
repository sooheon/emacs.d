(use-package clojure-mode
  :defer 20
  :mode ("\\.boot\\'" . clojure-mode)
  :init
  (add-to-list 'magic-mode-alist '("#!.*boot\\s-*$" . clojure-mode))
  (add-to-list 'magic-mode-alist '("#!.*planck\\s-*$" . clojurescript-mode))
  (with-eval-after-load 'smartparens
    (add-to-list 'sp-lisp-modes 'cider-clojure-interaction-mode))
  :config
  ;; This is for clojure-semantic, the library file is clojure.el
  (load-library "clojure")
  ;; Indentation
  (define-clojure-indent
    (match :defn)))

(use-package clj-refactor
  :diminish clj-refactor-mode
  :defer t
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  (add-hook 'clj-refactor-mode-hook 'yas-minor-mode-on)
  :config
  (cljr-add-keybindings-with-prefix "s-,")
  ;; (nmap :keymaps 'clj-refactor-map
  ;;   "c" (general-key-dispatch 'lispyville-change
  ;;         "r" (general-simulate-keys "s-,")
  ;;         "c" 'evil-change-whole-line))
  (setq cljr-favor-prefix-notation nil
        cljr-warn-on-eval nil)
  (setq cljr-magic-require-namespaces
        '(("io" . "clojure.java.io")
          ("set" . "clojure.set")
          ("str" . "clojure.string")
          ("walk" . "clojure.walk")
          ("zip" . "clojure.zip")
          ("r" . "clojure.core.reducers"))))

;;;###autoload
(defun soo-clojure-hook ()
  (lispy-mode 1)
  (company-mode 1))

(use-package cider
  :defer t
  :general
  (nmap :keymaps 'cider-mode-map "K"'cider-doc)
  :config
  (setq cider-prompt-for-symbol nil
        nrepl-hide-special-buffers t
        cider-repl-pop-to-buffer-on-connect nil
        cider-prompt-save-file-on-load 'always-save
        cider-repl-use-clojure-font-lock nil
        cider-font-lock-dynamically '(macro core deprecated)
        cider-mode-line '(:eval (format " %s" (cider--modeline-info)))
        cider-default-repl-command "boot"
        cider-pprint-fn 'fipp
        lispy-eval-display-style 'overlay)

  (iemap :keymaps 'cider-repl-mode-map
    "C-n" 'cider-repl-next-input
    "C-p" 'cider-repl-previous-input
    "M-n" nil
    "M-p" nil)

  (defun cider-figwheel-repl ()
    (interactive)
    (setq-local cider-cljs-lein-repl
                "(do (require 'figwheel-sidecar.repl-api)
                     (figwheel-sidecar.repl-api/start-figwheel!)
                     (figwheel-sidecar.repl-api/cljs-repl))")
    (cider-jack-in-clojurescript))
  (general-define-key :keymaps 'cider-mode-map
    "C-c C-M-j" 'cider-figwheel-repl)

<<<<<<< variant A
  ;; (defadvice cider-jump-to-var (before add-evil-jump activate)
  ;;   (evil-set-jump))
  (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
  (add-hook 'cider-clojure-interaction-mode 'smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)

  (evil-set-initial-state 'cider-docview-mode 'insert)
  (evil-set-initial-state 'cider-stacktrace-mode 'insert)
  (evil-set-initial-state 'cider-macroexpansion-mode 'insert)
  (evil-set-initial-state 'cider-browse-ns-mode 'insert)
  (evil-set-initial-state 'cider-test-report-mode 'insert)
>>>>>>> variant B
  (defun soo-cider-repl-hook ()
    (toggle-truncate-lines 1)
    (company-mode)
    (smartparens-mode 1))
  (add-hook 'cider-repl-mode-hook 'soo-cider-repl-hook)
  (evil-define-key 'normal cider-mode-map "K" 'cider-doc)
  (evil-set-initial-state 'cider-repl-mode 'emacs)
  (evil-set-initial-state 'cider-docview-mode 'emacs)
  (evil-set-initial-state 'cider-stacktrace-mode 'emacs)
  (evil-set-initial-state 'cider-macroexpansion-mode 'emacs)
  (evil-set-initial-state 'cider-browse-ns-mode 'emacs)
  (evil-set-initial-state 'cider-test-report-mode 'emacs)
======= end
  (advice-add 'cider-find-var :after #'recenter-top-bottom))

(use-package inf-clojure
  :defer t
  :config
  (setq inf-clojure-program "planck")
  (add-hook 'inf-clojure-mode-hook #'smartparens-mode)
  (general-define-key :keymaps 'clojurescript-mode-map
    "C-c C-k" 'inf-clojure-eval-buffer
    "C-c C-e" 'inf-clojure-eval-last-sexp
    "C-M-x" 'inf-clojure-eval-defun))
