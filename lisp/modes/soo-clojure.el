(use-package clojure-mode
  :ensure t
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
    (match :defn)
    (s/fdef :defn))
  (setq clojure-align-forms-automatically nil))

(use-package clj-refactor
  :ensure t
  :diminish clj-refactor-mode
  :defer t
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  (add-hook 'clj-refactor-mode-hook 'yas-minor-mode-on)
  :config
  (cljr-add-keybindings-with-prefix "C-,")
  ;; (nmap :keymaps 'clj-refactor-map
  ;;   "c" (general-key-dispatch 'lispyville-change
  ;;         "r" (general-simulate-keys "s-,")
  ;;         "c" 'evil-change-whole-line))
  (setq cljr-favor-prefix-notation nil
        cljr-warn-on-eval nil
        cljr-magic-require-namespaces '(("io" . "clojure.java.io")
                                        ("set" . "clojure.set")
                                        ("walk" . "clojure.walk")
                                        ("zip" . "clojure.zip")
                                        ("r" . "clojure.core.reducers"))))

;;;###autoload
(defun soo-clojure-hook ()
  (lispy-mode 1)
  (company-mode 1))

(use-package cider
  :ensure t
  :defer t
  :general
  (nmap :keymaps 'cider-mode-map "K" 'cider-doc)
  (nmap :keymaps 'cider-repl-mode-map "," 'cider-repl-handle-shortcut)
  (:keymaps 'cider-mode-map :prefix "C-c"
   "M-i" (lambda () (interactive) (progn (cider-inspect)
                                         (cider-inspector-next-inspectable-object 2))))
  :config
  (setq cider-prompt-for-symbol nil
        nrepl-hide-special-buffers t
        cider-repl-pop-to-buffer-on-connect nil
        cider-prompt-save-file-on-load 't
        cider-repl-use-clojure-font-lock t
        cider-font-lock-dynamically nil ;; '(macro core deprecated)
        cider-mode-line '(:eval (format " %s" (cider--modeline-info)))
        cider-default-repl-command "boot"
        cider-pprint-fn 'puget
        cider-repl-use-pretty-printing t)

  (nmap :keymaps '(cider-popup-buffer-mode-map
                   cider-docview-mode-map
                   cider-inspector-mode-map
                   cider-test-report-mode-map)
    "q" 'cider-popup-buffer-quit-function)
  (emap :keymaps 'cider-inspector-mode-map
    "j" 'cider-inspector-next-inspectable-object
    "k" 'cider-inspector-previous-inspectable-object
    "h" 'cider-inspector-pop
    "l" (lambda ()
          (interactive)
          (progn (cider-inspector-operate-on-point)
                 (cider-inspector-next-inspectable-object 2))))
  (defun soo-cider-repl-hook ()
    (toggle-truncate-lines 1)
    (company-mode)
    (lispy-mode)
    (cider-company-enable-fuzzy-completion))
  (add-hook 'cider-repl-mode-hook 'soo-cider-repl-hook)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

  (evil-define-key 'normal cider-mode-map "K" 'cider-doc)
  (evil-set-initial-state 'cider-repl-mode 'insert)
  (evil-set-initial-state 'cider-docview-mode 'normal)
  (evil-set-initial-state 'cider-stacktrace-mode 'insert)
  (evil-set-initial-state 'cider-macroexpansion-mode 'insert)
  (evil-set-initial-state 'cider-browse-ns-mode 'insert)
  (evil-set-initial-state 'cider-test-report-mode 'insert)
  (evil-set-initial-state 'cider-inspector-mode 'emacs))

(use-package inf-clojure
  :ensure t
  :defer t
  :config
  (add-hook 'inf-clojure-mode-hook #'smartparens-mode)
  (general-define-key :keymaps 'clojurescript-mode-map
    "C-c C-k" 'inf-clojure-eval-buffer
    "C-c C-e" 'inf-clojure-eval-last-sexp
    "C-M-x" 'inf-clojure-eval-defun))

(provide 'soo-clojure)
