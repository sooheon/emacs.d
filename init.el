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

(progn ;; themes
  (add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory)))

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
  :init
  (defun spacemacs/alternate-buffer ()
    "Switch back and forth between current and last buffer in the
current window."
    (interactive)
    (if (evil-alternate-buffer)
	(switch-to-buffer (car (evil-alternate-buffer)))
      (switch-to-buffer (other-buffer (current-buffer) t))))
  (evil-leader/set-key
    "TAB" 'spacemacs/alternate-buffer
    "u" 'universal-argument
    "wl" 'evil-window-right
    "wh" 'evil-window-left
    "wk" 'evil-window-top
    "wj" 'evil-window-bottom
    "wL" 'evil-window-move-far-right
    "wH" 'evil-window-move-far-left
    "wK" 'evil-window-move-very-top
    "wJ" 'evil-window-move-very-bottom
    "w=" 'balance-windows
    "wm" 'delete-other-windows
    "wo" 'other-window
    "ws" 'evil-window-split
    "wv" 'evil-window-vsplit)
  :config
  (global-evil-leader-mode))

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

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

(use-package ace-link
  :commands (ace-link-info ace-link-eww ace-link-help)
  :init
  (with-eval-after-load 'info (define-key Info-mode-map "o" 'ace-link-info))
  (with-eval-after-load 'help-mode (define-key help-mode-map "o" 'ace-link-help)))

(use-package autorevert
  :diminish auto-revert-mode
  :defer t)

(use-package company
  :diminish (company-mode . "co")
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
  (let ((m company-active-map))
    (define-key m [escape] (lambda () (interactive)
                             (company-abort)
                             (evil-normal-state)))))

(use-package dash
  :config (dash-enable-font-lock))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package dired
  :commands (dired-jump)
  :init
  (evil-leader/set-key "d" 'dired-jump)
  :config
  (setq dired-listing-switches "-alh"))

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode))
  (evil-define-key 'normal elisp-slime-nav-mode-map
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point))

(use-package evil-matchit
  :defer t)

(use-package help
  :config (temp-buffer-resize-mode))

(use-package ivy
  :diminish ivy-mode
  :bind (("s-f" . swiper)
         ("C-s" . swiper)
         ("s-b" . ivy-switch-buffer)
         ("C-c k" . counsel-ag)
         ("M-x" . counsel-M-x)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-x l" . counsel-locate)
         ("C-c C-r" . ivy-resume))
  :config
  (setq ivy-use-virtual-buffers t
        ivy-extra-directories '("./")
        ivy-count-format "%d "
        ivy-height 12)
  (ivy-mode 1)
  (evil-leader/set-key
    "f" 'counsel-find-file
    "r" 'ivy-recentf
    "b" 'ivy-switch-buffer
    "/" 'counsel-ag
    "Th" 'counsel-load-theme)
  (let ((m ivy-minibuffer-map))
    (define-key m [escape] 'minibuffer-keyboard-quit)
    (define-key m (kbd "<s-backspace>") (lambda () (interactive) (kill-line 0))))
  (require 'ivy-hydra)
  (define-key ivy-minibuffer-map "\C-o"
    (defhydra soo-ivy (:hint nil :color pink)
      "
 Move     ^^^^^^^^^^ | Call         ^^^^ | Cancel^^ | Options^^ | Action _w_/_s_/_a_: %s(ivy-action-name)
----------^^^^^^^^^^-+--------------^^^^-+-------^^-+--------^^-+---------------------------------
 _g_ ^ ^ _k_ ^ ^ _u_ | _f_orward _o_ccur | _i_nsert | _c_alling: %-7s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
 ^↨^ _h_ ^+^ _l_ ^↕^ | _RET_ done     ^^ | _q_uit   | _m_atcher: %-7s(ivy--matcher-desc) _t_runcate: %-11`truncate-lines
 _G_ ^ ^ _j_ ^ ^ _d_ | _TAB_ alt-done ^^ | ^ ^      | _<_/_>_: shrink/grow
"
      ;; arrows
      ("j" ivy-next-line)
      ("k" ivy-previous-line)
      ("l" ivy-alt-done)
      ("h" ivy-backward-delete-char)
      ("g" ivy-beginning-of-buffer)
      ("G" ivy-end-of-buffer)
      ("d" ivy-scroll-up-command)
      ("u" ivy-scroll-down-command)
      ("e" ivy-scroll-down-command)
      ;; actions
      ("q" keyboard-escape-quit :exit t)
      ("C-g" keyboard-escape-quit :exit t)
      ("<escape>" keyboard-escape-quit :exit t)
      ("C-o" nil)
      ("i" nil)
      ("TAB" ivy-alt-done :exit nil)
      ("C-j" ivy-alt-done :exit nil)
      ;; ("d" ivy-done :exit t)
      ("RET" ivy-done :exit t)
      ("C-m" ivy-done :exit t)
      ("f" ivy-call)
      ("c" ivy-toggle-calling)
      ("m" ivy-toggle-fuzzy)
      (">" ivy-minibuffer-grow)
      ("<" ivy-minibuffer-shrink)
      ("w" ivy-prev-action)
      ("s" ivy-next-action)
      ("a" ivy-read-action)
      ("t" (setq truncate-lines (not truncate-lines)))
      ("C" ivy-toggle-case-fold)
      ("o" ivy-occur :exit t))))

(use-package lispy
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
  :init
  (add-hook 'lispy-mode-hook #'lispyville-mode)
  (setq lispyville-key-theme '(operators
                               (escape insert hybrid emacs)
                               slurp/barf-cp)
        lispyville-motions-put-into-special t
        lispyville-barf-stay-with-closing t)
  :config
  (define-key lispyville-mode-map "\M-n" 'lispyville-drag-forward)
  (define-key lispyville-mode-map "\M-p" 'lispyville-drag-backward))

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

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

(use-package window-numbering
  :bind (("s-0" . select-window-0)
         ("s-1" . select-window-1)
         ("s-2" . select-window-2)
         ("s-3" . select-window-3)
         ("s-4" . select-window-4)
         ("s-5" . select-window-5)
         ("s-6" . select-window-6))
  :config
  (window-numbering-mode 1))

(use-package winner
  :init
  (evil-leader/set-key "wu" 'winner-undo)
  (winner-mode t)
  (setq winner-boring-buffers
        (append winner-boring-buffers '("*Compile-Log*"
                                        "*inferior-lisp*"
                                        "*Fuzzy Completions*"
                                        "*Apropos*"
                                        "*Help*"
                                        "*cvs*"
                                        "*Buffer List*"
                                        "*Ibuffer*"
                                        "*esh command on file*"))))

(progn ;; personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

(defun sooheon--config ()
  (blink-cursor-mode -1)
  (global-set-key (kbd "s-u") 'universal-argument)
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)
  ;; Fonts
  (add-to-list 'default-frame-alist
               '(font . "Input Mono Narrow-12"))
  (let ((f "fontset-default"))
    (set-fontset-font f 'hangul '("NanumGothic" . "unicode-bmp"))))
(sooheon--config)

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

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
