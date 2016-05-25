;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds

(progn ;; startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(progn ;; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(progn ;; `use-package'
  (eval-when-compile
    (require 'use-package))
  (require 'diminish)
  (require 'bind-key)
  (setq use-package-verbose t))

(use-package auto-compile
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :config (or (server-running-p) (server-mode)))

(progn ; startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Long tail

(defvar sooheon--avy-keys '(?w ?e ?r ?s ?d ?x ?c ?u ?i ?o ?v ?n ?m ?l ?k ?j ?f))

(use-package avy
  :bind (("s-g" . evil-avy-goto-word-1))
  :config
  (setq avy-keys sooheon--avy-keys)
  (global-set-key [remap goto-line] 'evil-avy-goto-line))

(use-package company
  :diminish company-mode
  ;; :bind (:map company-active-map
  ;;             ("C-w" . nil)
  ;;             ("M-." . company-show-location)
  ;;             ("C-s" . company-filter-candidates)
  ;;             ("C-d" . nil)             ; company-show-doc-buffer
  ;;             ("C-/" . nil)             ; 'company-search-candidates
  ;;             ("C-M-/" . nil)           ; 'company-filter-candidates
  ;;             ("C-n" . nil)
  ;;             ("C-p" . nil)
  ;;             ("C-f" .  nil))
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2)
  (global-company-mode)
  (define-key company-active-map [escape] (lambda () (interactive)
					    (company-abort)
					    (evil-normal-state))))

(use-package dash
  :config (dash-enable-font-lock))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

(use-package eldoc
  :config (global-eldoc-mode))

(use-package evil
  :init
  (setq-default evil-want-C-u-scroll t
                evil-want-fine-undo nil
                evil-cross-lines t
                evil-symbol-word-search t
                evil-move-cursor-back nil
                evil-want-C-i-jump t
                evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map "\C-w" 'evil-delete-backward-word))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (defun spacemacs/alternate-buffer ()
    "Switch back and forth between current and last buffer in the
current window."
    (interactive)
    (if (evil-alternate-buffer)
	(switch-to-buffer (car (evil-alternate-buffer)))
      (switch-to-buffer (other-buffer (current-buffer) t))))
  (evil-leader/set-key
    "TAB" 'spacemacs/alternate-buffer))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(progn ;; `isearch'
  (setq isearch-allow-scroll t))

(use-package ivy
  :diminish ivy-mode
  :bind (("s-f" . swiper)
	 ;;:map ivy-minibuffer-map
	 ;;(([escape] . minibuffer-keyboard-quit)
	 )
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1)
  (evil-leader/set-key
    "f" 'counsel-find-file
    "r" 'ivy-recentf
    "b" 'ivy-switch-buffer
    "/" 'counsel-ag)
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (global-set-key "\C-ck" 'counsel-ag)
  (global-set-key "\M-x" 'counsel-M-x)
  (global-set-key "\C-hf" 'counsel-describe-function)
  (global-set-key "\C-hv" 'counsel-describe-variable)
  ;; (global-set-key "\C-hl" 'counsel-load-library)
  ;; (global-set-key "\C-hi" 'counsel-info-lookup-symbol)
  ;; (global-set-key "\C-hu" 'counsel-unicode-char)
  (global-set-key "\C-cg" 'counsel-git)
  (global-set-key "\C-cj" 'counsel-git-grep)
  (global-set-key "\C-xl" 'counsel-locate)
  (global-set-key "\C-c\C-r" 'ivy-resume))

(use-package lispy
  :diminish lispy-mode
  :init (setq lispy-compat '(edebug cider)
	      ;; Use the same keys as avy for ace jump
	      lispy-avy-keys sooheon--avy-keys
	      ;; Don't push around code I want to jump to!
	      lispy-avy-style-paren 'at-full
	      ;; lispy-eval-display-style 'overlay
	      lispy-delete-backward-recenter nil
	      lispy-safe-paste t)
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
  :config
  (lispy-set-key-theme '(special
			 c-digits
			 paredit
			 ;; parinfer
			 ))
  (dolist (map (list lispy-mode-map-paredit lispy-mode-map-parinfer))
    (define-key map (kbd "C-a") nil)
    (define-key map "\M-j" 'lispy-split)
    (define-key map "\M-k" 'lispy-kill-sentence)
    (define-key map [M-up] 'sp-splice-sexp-killing-backward)
    (define-key map [M-down] 'sp-splice-sexp-killing-forward)
    (define-key map (kbd "C-,") 'lispy-kill-at-point))
  (let ((map lispy-mode-map-paredit))
    (define-key map "\M-n" nil)		; lispy left
    (define-key map "\M-p" nil)
    (define-key map "\"" nil)		; lispy-quotes
    (define-key map (kbd "C-d") 'lispy-delete)
    (define-key map (kbd "M-)") nil)
    (evil-define-key 'insert map [backspace] 'lispy-delete-backward)
    (evil-define-key 'normal map [backspace] nil))
  (let ((map lispy-mode-map-parinfer))
    (define-key map (kbd "\"") nil)
    (define-key map (kbd "M-r") 'lispy-raise)
    (define-key map (kbd "#") nil)
    (define-key map (kbd ":") nil))

  ;; Unbind M-k and M-. in evil normal state and use lispy
  (define-key evil-normal-state-map "\M-k" nil)
  (define-key evil-normal-state-map "\M-." nil) ; evil-repeat-pop-next
  )

(use-package lispyville
  :diminish lispyville-mode
  :bind (:map lispyville-mode-map
              ("M-n" . lispyville-drag-forward)
              ("M-p" . lispyville-drag-backward))
  :init
  (add-hook 'lispy-mode-hook #'lispyville-mode)
  (setq lispyville-key-theme '(operators
                               (escape insert hybrid emacs)
                               slurp/barf-cp)
        lispyville-motions-put-into-special t
        lispyville-barf-stay-with-closing t))

(use-package magit
  :defer t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-upstream
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-pushremote
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpushed-to-upstream
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpushed-to-pushremote
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-submodules
                          'magit-insert-unpulled-from-upstream))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package paren
  :config (show-paren-mode))

(use-package prog-mode
  :config (global-prettify-symbols-mode))

(use-package recentf
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :config (save-place-mode))

(use-package smartparens
  :init (setq sp-cancel-autoskip-on-backward-movement nil)
  :config
  (progn
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1)
    (let ((m smartparens-mode-map))
      (define-key m [C-backspace] 'sp-backward-kill-sexp)
      (define-key m (kbd "C-)") 'sp-forward-slurp-sexp)
      (define-key m (kbd "C-(") 'sp-backward-slurp-sexp)
      (define-key m (kbd "C-{") 'sp-backward-barf-sexp)
      (define-key m (kbd "C-}") 'sp-forward-barf-sexp))
    (setq sp-show-pair-from-inside nil
	  sp-show-pair-delay 0)))

(use-package simple
  :config (column-number-mode))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(progn ;; startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(progn ;; personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
