;;; init.el --- user-init-file

(setq gc-cons-threshold (* 200 1024 1024))

;;* loads
(defvar my-load-paths
  (mapcar (lambda (p) (concat user-emacs-directory p))
          '("lisp"
            ;; "lib/org-mode/contrib/lisp"
            ;; "lib/org-mode/lisp"
            "lisp/themes"
            "lisp/modes"
            "lib/emacs-libvterm"
            "lib/clojure-semantic"
            "lib/lpy"
            "lib/no-littering"
            "lib/org-mode"
            "lib/soap"
            "lib/structured-haskell-mode"
            "lib/company-simple-complete")))

(mapc (apply-partially 'add-to-list 'load-path) my-load-paths)

(load "loaddefs.el" nil 'nomessage)
(load "auto.el" 'noerror 'nomessage)

;;* customize
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(load custom-file 'noerror)
;;** font
(set-face-attribute 'default nil :family "Input Mono Narrow")
(set-face-attribute 'default nil :height 140)
(set-fontset-font t 'hangul (font-spec :name "NanumGothic"))
;;** decorations
(setq tool-bar-mode nil)
(setq menu-bar-mode nil)
(setq scroll-bar-mode nil)
(setq line-spacing 1)
(setq inhibit-startup-screen t
      inhibit-message nil
      initial-scratch-message nil
      create-lockfiles nil
      window-combination-resize t)
(eval '(setq inhibit-startup-echo-area-message "sooheon"))
(setq frame-title-format '("%b"
                           (:eval
                            (when (bound-and-true-p projectile-mode)
                              (when (projectile-project-name)
                                (list " [" (projectile-project-name) "]"))))))
(setq fringe-indicator-alist
      '((continuation nil right-curly-arrow)
        (truncation left-arrow right-arrow)
        (continuation left-curly-arrow right-curly-arrow)
        (overlay-arrow . right-triangle)
        (up . up-arrow)
        (down . down-arrow)
        (top top-left-angle top-right-angle)
        (empty-line . empty-line)
        (unknown . question-mark)))
;;** minibuffer interaction
(setq enable-recursive-minibuffers t
      minibuffer-message-timeout 1)
(minibuffer-depth-indicate-mode 1)
;;** editor behavior
(setq tab-always-indent t)
(put 'scroll-left 'disabled nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil)
(setq scroll-margin 2
      scroll-preserve-screen-position 'always
      scroll-conservatively 100)
(add-to-list 'default-frame-alist '(width . 90))
(show-paren-mode 1)
(progn ;; Deal with large files
  (setq jit-lock-defer-time 0)
  (setq-default bidi-display-reordering nil) ; http://tinyurl.com/jc9corx
  (add-hook 'find-file-hook #'my-find-huge-file-literally-hook))
;; Don't indent lists starting with keywords
(setq lisp-indent-function 'Fuco1/lisp-indent-function)
(setq load-prefer-newer t
      vc-follow-symlinks t
      find-file-suppress-same-file-warnings t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)
(prefer-coding-system 'utf-8)
(electric-indent-mode -1)
(setq truncate-lines nil)
(setq default-input-method "korean-hangul")
(setq indent-tabs-mode nil)
(setq backup-inhibited t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions nil)
(setq recenter-positions '(top middle bottom))
(setq eval-expression-print-length nil
      eval-expression-print-level nil
      resize-mini-windows t)
(setq sentence-end-double-space nil)
(setq search-default-mode 'char-fold-to-regexp)
(add-hook 'server-switch-hook 'raise-frame)
(put 'narrow-to-region 'disabled nil)

;;** shell
;; (when (executable-find "fish")
;;   (setq shell-file-name (executable-find "fish")))

;;* Bootstrap
;;** Package init
(setenv "LANG" "en_US.UTF-8")
(setq no-littering-etc-directory (expand-file-name ".etc/" user-emacs-directory)
      no-littering-var-directory (expand-file-name ".var/" user-emacs-directory))
(require 'no-littering)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;;** Set up environment
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :commands (shell-command eval-expression)
  :defer 5
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;;** OSX
(use-package osx-trash
  :ensure t
  :if (eq system-type 'darwin)
  :init
  (setq delete-by-moving-to-trash t)
  :config
  (osx-trash-setup))

;;** themes
(add-to-list 'custom-theme-load-path (expand-file-name "lisp/themes" user-emacs-directory))
(require 'soo-themes)

;;** keybinds
(use-package general
  :ensure t
  :demand t
  :config
  (general-evil-setup t t)
  (load "keybinds.el" nil t))

;;** Mode Requires
(require 'soo-evil)
(require 'soo-ivy)
;; (require 'soo-rust)
(require 'soo-c)
(require 'soo-clojure)
(require 'soo-ess)
(require 'soo-python)
;; (add-hook 'haskell-mode-hook 'soo-haskell-hook)
(add-hook 'org-mode-hook 'soo-org-hook)
(run-with-idle-timer 10 nil (lambda () (require 'soo-org)))

;;** general modes config
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t
        auto-compile-source-recreate-deletes-dest t
        auto-compile-toggle-deletes-nonlib-dest t
        auto-compile-update-autoloads t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

(defvar sooheon-avy-keys '(?w ?e ?r ?s ?d ?x ?c ?u ?i ?o ?v ?n ?m ?l ?k ?j ?f))

(use-package avy
  :ensure t
  :commands spacemacs/avy-open-url
  :general
  ([remap goto-line] 'evil-avy-goto-line)
  (nmap :prefix "SPC" "xo" 'spacemacs/avy-open-url)
  :config
  (setq avy-keys sooheon-avy-keys)
  (defun spacemacs/avy-goto-url ()
    "Use avy to go to an URL in the buffer."
    (interactive)
    (avy--generic-jump "https?://" nil 'pre))
  (defun spacemacs/avy-open-url ()
    "Use avy to select an URL in the buffer and open it."
    (interactive)
    (save-excursion
      (spacemacs/avy-goto-url)
      (browse-url-at-point))))

(use-package ace-link
  :ensure t
  :commands (ace-link-info ace-link-woman ace-link-help ace-link-custom)
  :general
  (:keymaps 'help-mode-map "o" 'ace-link-help)
  :config
  (ace-link-setup-default))

(use-package link-hint
  :ensure t
  :disabled t
  :defer 6
  :general
  (nmap :prefix "SPC" "o" 'link-hint-open-link
    (:prefix "C-c l" "o" 'link-hint-open-link "c" 'link-hint-copy-link)
    (:keymaps 'help-mode-map "o" 'link-hint-open-link)))

(use-package ace-window
  :ensure t
  :general
  ("C-x o" 'ace-window)
  :config
  (setq aw-keys sooheon-avy-keys))

(use-package autorevert
  :ensure t
  :defer 2
  :diminish auto-revert-mode
  :config
  (setq global-auto-revert-non-file-buffers t ; revert Dired buffers too
        auto-revert-use-notify nil            ; OSX doesn't have file-notify
        auto-revert-verbose nil))

(use-package compile
  :ensure t
  :defer t
  :init
  (define-key prog-mode-map [f9] #'compile)
  :config
  (setq compilation-ask-about-save nil
        compilation-scroll-output 'next-error
        compilation-skip-threshold 2))

(use-package conf-mode
  :ensure t
  :mode ("/\\.[^/]*rc" . conf-mode))

(use-package cc-mode
  :ensure t
  :defer t
  :mode ("\\.h\\'" . c-mode)
  :config
  (c-toggle-auto-hungry-state 1))

(use-package function-args
  :ensure t
  :defer t
  :config
  (fa-config-default))

(use-package dired
  :commands dired-jump
  :general
  (nmap "-" 'dired-jump)
  :config
  (nmap :keymaps 'dired-mode-map
    "-" '(lambda () (interactive) (find-file ".."))
    "gg" '(lambda () (interactive) (beginning-of-buffer) (dired-next-line 1))
    "got" 'soo-terminal-pop
    "gof" 'reveal-in-osx-finder
    "G" '(lambda () (interactive) (end-of-buffer) (dired-next-line -1))
    "=" 'vinegar/dired-diff
    "I" 'vinegar/dotfiles-toggle
    "~" '(lambda () (interactive) (find-file "~/"))
    "<return>" 'dired-find-file
    "f" 'counsel-find-file
    "J" 'dired-goto-file
    "C-f" nil                           ; 'find-name-dired
    "H" 'diredp-dired-recent-dirs
    "T" 'dired-tree-down
    "K" 'dired-do-kill-lines
    "r" 'revert-buffer
    "C-r" 'dired-do-redisplay
    "RET" 'dired-find-file
    "e" 'ora-ediff-files
    "Y" 'ora-dired-rsync)
  (setq dired-listing-switches "-alGh1v --group-directories-first")
  (defvar dired-dotfiles-show-p)
  (defun soo--dired-setup ()
    ;; (setq dired-omit-verbose nil)
    (setq dired-hide-details-hide-symlink-targets nil)
    (dired-hide-details-mode -1))
  (add-hook 'dired-mode-hook 'soo--dired-setup))

(use-package ediff
  :ensure t
  :defer t
  :commands (ediff-buffers ediff)
  :config
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally
                ediff-merge-split-window-function 'split-window-horizontally)
  (add-hook 'ediff-quit-hook #'winner-undo))

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :general
  (nmap :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point))

(use-package circe
  :ensure t
  :defer t
  :general
  (nmap :prefix "SPC" "i" 'soo--counsel-circe)
  (:prefix "C-c" "i" 'soo--counsel-circe)
  :config
  (defun soo--counsel-circe ()
    "Switch to Circe buffers using completing-read, or start
Circe if no buffers open."
    (interactive)
    (let ((candidates (list)))
      (dolist (buf (buffer-list) candidates)
        (if (memq (with-current-buffer buf major-mode)
                  '(circe-channel-mode circe-server-mode circe-query-mode))
            (setq candidates (append (list (buffer-name buf)) candidates))))
      (if candidates
          (switch-to-buffer (completing-read "cIRCe buffer: " candidates))
        (circe "Freenode"))))
  (defun counsel-tracking ()
    (interactive)
    (switch-to-buffer (completing-read "Tracked buffer: " tracking-buffers)))
  (setq circe-reduce-lurker-spam t
        tracking-position 'end)
  (enable-circe-color-nicks)
  (enable-lui-track-bar)
  (require 'lui-autopaste)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
  (add-hook 'circe-chat-mode-hook 'my-circe-prompt)
  (defun my-circe-prompt ()
    (lui-set-prompt
     (concat (propertize (concat (buffer-name) ">") 'face 'circe-prompt-face)
             " ")))
  (load "lui-logging" nil t)
  (enable-lui-logging-globally)

  (setq lui-time-stamp-position 'right-margin
        lui-fill-type nil
        lui-time-stamp-format "%H:%M")
  (add-hook 'lui-mode-hook 'my-lui-setup)
  (defun my-lui-setup ()
    (setq fringes-outside-margins t
          right-margin-width 5
          word-wrap t
          wrap-prefix "    ")
    (setf (cdr (assoc 'continuation fringe-indicator-alist)) nil)))

(use-package circe-notifications
  :disabled t
  :defer t
  :init
  (add-hook 'circe-server-connected-hook 'enable-circe-notifications)
  :config
  (setq circe-notifications-alert-style 'osx-notifier))

(use-package dabbrev
  :ensure t
  :defer t
  :config
  (defun soo--dabbrev-friend-buffer (other-buffer)
    "If OTHER-BUFFER is not remote and is in the same project as
the current buffer, consider it a friend. Otherwise consider it a
friend if it has the same major mode."
    (if (and (not
              (file-remote-p
               (buffer-file-name other-buffer)))
             (projectile-project-p))
        (string= (projectile-project-name)
                 (with-current-buffer other-buffer
                   (projectile-project-name)))
      (eq major-mode
          (with-current-buffer other-buffer
            major-mode))))
  (setq dabbrev-friend-buffer-function #'soo--dabbrev-friend-buffer))

(use-package flycheck
  :ensure t
  :defer t
  :config
  (evil-set-initial-state 'flycheck-error-list-mode 'insert)
  (setq flycheck-indication-mode nil
        flycheck-mode-line-prefix "fc"))

(use-package gist :defer t)

(use-package hydra
  :ensure t
  :general
  (:keymaps 'hydra-base-map
   "C-u" nil
   "0" nil))

(use-package ispell
  :disabled t
  :init
  (setenv "DICTIONARY" "en_US")
  (setq ispell-program-name (executable-find "hunspell")))

(use-package flyspell
  :disabled t
  :defer t
  :diminish flyspell-mode
  :general ("C-;" 'flyspell-auto-correct-previous-word)
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package flyspell-correct
  :disabled t
  :after flyspell
  :general (:keymaps 'flyspell-mode-map "C-;" 'flyspell-correct-word-generic)
  :config
  (setq flyspell-correct-interface 'flyspell-correct-ivy))

(use-package speck
  :disabled t
  :commands speck-mode
  :general (nmap :prefix "SPC" "ts" 'speck-mode)
  :init
  (setq
   speck-hunspell-minimum-word-length 3
   speck-auto-correct-case 'two
   speck-hunspell-coding-system "utf-8"
   speck-hunspell-library-directory (expand-file-name "~/Library/Spelling/")
   speck-hunspell-dictionary-alist '(("en" . "en_US"))
   speck-hunspell-default-dictionary-name "en"
   speck-hunspell-extra-arguments
   (list "-p" (concat speck-hunspell-library-directory "LocalDictionary")))
  (defun soo--speck-prog-hook ()
    (set (make-local-variable 'speck-syntactic) t)
    (set (make-local-variable 'speck-face-inhibit-list)
         '(font-lock-constant-face)))
  (add-hook 'prog-mode-hook 'soo--speck-prog-hook)
  (defun soo--speck-org-hook ()
    (set (make-local-variable 'speck-face-inhibit-list)
         '(org-tag org-latex-and-related org-meta-line org-table))
    (speck-mode))
  (add-hook 'org-mode-hook 'soo--speck-org-hook)
  (add-hook 'text-mode-hook 'speck-mode))

(use-package help
  :init
  (setq help-window-select t)
  :config
  (add-hook 'help-mode-hook (lambda () (toggle-truncate-lines -1))))

(use-package hl-todo
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook 'hl-todo-mode))

(use-package highlight-escape-sequences
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook 'hes-mode))

(use-package info
  :ensure t
  :general
  (nmap :prefix "SPC" "hi" 'info)
  :config
  (evil-set-initial-state 'Info-mode 'emacs))

(use-package golden-ratio
  :ensure t
  :general
  (nmap :prefix "SPC" "tg" 'golden-ratio))

(use-package restclient :defer t
  :ensure t
  :config
  (use-package company-restclient
    :config
    (add-to-list 'company-backends 'company-restclient)))

(with-eval-after-load 'warnings (add-to-list 'warning-suppress-types '(undo discard-info)))

;;** Parens and lisp
(use-package smartparens
  :ensure t
  :disabled t
  :diminish (smartparens-mode . "sp")
  :defer t
  :general
  (:keymaps 'smartparens-mode-map
   ;; "DEL" 'sp-backward-delete-char
   ;; "C-d" 'sp-delete-char
   "C-M-a" 'sp-beginning-of-sexp "C-M-e" 'sp-end-of-sexp
   "M-r" 'sp-raise-sexp
   [C-backspace] 'sp-backward-kill-sexp
   "C-)" 'sp-forward-slurp-sexp "C-(" 'sp-backward-slurp-sexp
   "C-{" 'sp-backward-barf-sexp "C-}" 'sp-forward-barf-sexp
   "M-S" 'sp-splice-sexp)
  :init
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'smartparens-strict-mode-hook 'show-smartparens-mode)
  (add-hook 'smartparens-mode-hook 'show-smartparens-mode)
  (defun conditionally-enable-sp ()
    "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
    (if (eq this-command 'eval-expression)
        (smartparens-strict-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-sp)
  :config
  (require 'smartparens-config)
  (setq sp-cancel-autoskip-on-backward-movement nil
        sp-autoskip-closing-pair 'always-end
        sp-autoinsert-pair t
        sp-autodelete-wrap nil
        sp-show-pair-from-inside nil
        sp-highlight-pair-overlay nil
        sp-escape-quotes-after-insert nil)
  (turn-off-show-smartparens-mode)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil))

(use-package lispy
  :ensure t
  :diminish (lispy-mode . "ly")
  :general
  (:keymaps 'lispy-mode-map-c-digits
   "C-8" 'lispy-out-forward-newline
   "C-9" 'lispy-parens-down)
  (:keymaps 'lispy-mode-map-special "+" nil)
  :init
  (defun conditionally-enable-lispy ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
  (add-hook 'clojure-mode-hook 'lispy-mode)
  (add-hook 'clojurescript-mode-hook 'lispy-mode)
  (add-hook 'cider-repl-mode-hook 'lispy-mode)
  :config
  (setq iedit-toggle-key-default nil    ; Don't want to use iedit
        lispy-delete-atom-from-within nil)
  (with-eval-after-load 'evil
    (nmap :keymaps 'lispy-mode-map "gd" 'lispy-goto-symbol))
  (lispy-set-key-theme '(special c-digits))
  (setq lispy-compat '(edebug cider)
        lispy-avy-keys sooheon-avy-keys
        lispy-avy-style-paren 'at-full
        lispy-avy-style-symbol 'at-full
        lispy-delete-backward-recenter nil
        lispy-comment-use-single-semicolon t)
  (lispy-define-key lispy-mode-map-special ">" 'lispy-slurp-or-barf-right)
  (lispy-define-key lispy-mode-map-special "<" 'lispy-slurp-or-barf-left)
  (general-define-key :keymaps 'lispy-mode-map
    "DEL" 'lispy-delete-backward
    "C-d" 'lispy-delete
    "M-d" 'lispy-kill-word
    "M-DEL" 'lispy-backward-kill-word
    [C-backspace] 'lispy-delete-backward
    "(" 'lispy-parens
    "[" 'lispy-brackets
    "{" 'lispy-braces
    ")" 'lispy-right-nostring
    "]" 'lispy-right-nostring
    "}" 'lispy-right-nostring
    "\"" 'lispy-quotes
    "C-a" nil
    "C-M-b" 'lispy-backward
    "C-M-f" 'lispy-forward
    "M-(" 'lispy-wrap-round
    "M-[" 'lispy-wrap-brackets
    "M-{" 'lispy-wrap-braces
    "M-j" 'lispy-split
    "M-J" 'lispy-join
    "M-i" 'lispy-iedit
    "M-k" 'lispy-kill-sentence
    "C-k" 'lispy-kill
    ";" 'lispy-comment
    "M-RET" 'soo-lispy-meta-return
    "M-r" 'lispy-raise-sexp)
  (defun soo-lispy-meta-return ()
    (interactive)
    (call-interactively 'lispy-meta-return)
    (end-of-line)
    (evil-insert-state 1)))

(use-package parinfer
  :disabled t
  :ensure t
  :general
  (:keymaps 'parinfer-mode-map
            "C-," 'parinfer-toggle-mode
            [tab] 'parinfer-smart-tab:dwim-right-or-complete
            [backtab] 'parinfer-smart-tab:dwim-left
            "M-j" 'lispy-split
            "M-i" 'lispy-iedit
            "M-k" 'lispy-kill-sentence
            "C-k" 'lispy-kill
            [?\r] 'electric-newline-and-maybe-indent
            "M-(" 'lispy-wrap-round
            "M-[" 'lispy-wrap-brackets
            "M-{" 'lispy-wrap-braces
            "C-a" nil)
  :init
  (setq parinfer-extensions
        '(defaults pretty-parens evil lispy smart-tab paredit))
  (add-hook 'clojure-mode-hook #'parinfer-mode)
  (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
  (add-hook 'scheme-mode-hook #'parinfer-mode)
  (add-hook 'lisp-mode-hook #'parinfer-mode))

(use-package lispyville
  :ensure t
  :diminish lispyville-mode
  :general
  ([remap evil-normal-state] 'lispyville-normal-state)
  (:keymaps 'lispyville-mode-map
   [M-down] 'lispyville-drag-forward
   [M-up] 'lispyville-drag-backward)
  :init
  (add-hook 'lispy-mode-hook 'lispyville-mode)
  (setq lispyville-key-theme '(operators escape slurp/barf-cp)
        lispyville-barf-stay-with-closing t))

;;*** Emacs lisp
(use-package suggest
  :ensure t
  :commands suggest
  :config
  (sp-local-pair 'suggest-mode "'" nil :actions nil))

(use-package elisp-mode
  :general
  (:keymaps 'emacs-lisp-mode-map "C-c C-k" 'eval-buffer))

;;** Git and version control
(use-package magit
  :ensure t
  :general
  ("s-9" 'magit-status)
  (nvmap :prefix "SPC" "g" 'magit-status)
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-traditional
        magit-popup-show-common-commands nil))

(use-package evil-magit
  :ensure t
  :after magit
  :init
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  (setq evil-magit-want-horizontal-movement nil))

(use-package diff-hl
  :ensure t
  :defer 2
  :init
  (setq diff-hl-draw-borders nil)
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (evil-set-initial-state 'diff-mode 'emacs))

(use-package markdown-mode
  :ensure t
  :mode ("\\.m[k]d" . markdown-mode)
  :general
  (nmap :keymaps 'markdown-mode-map
    "gj" 'outline-forward-same-level
    "gk" 'outline-backward-same-level
    "gh" 'outline-up-heading
    ;; next visible heading is not exactly what we want but close enough
    "gl" 'outline-next-visible-heading))

(use-package super-save
  :ensure t
  :diminish super-save-mode
  :init
  (super-save-mode 1))

(use-package woman
  :ensure t
  :defer t
  :config (evil-set-initial-state 'woman-mode 'emacs)
  (bind-key "s-w" 'Man-quit woman-mode-map))

(use-package paradox
  :ensure t
  :defer t
  :config
  (setq paradox-execute-asynchronously t)
  (with-eval-after-load 'evil (evil-set-initial-state 'paradox-menu-mode 'emacs)))

(use-package async
  :ensure t
  :after paradox
  :config
  (async-bytecomp-package-mode t))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :general
  ("C-c k" 'soo--projectile-rg
   "s-O" 'projectile-find-file)
  (nmap "gs" 'soo--projectile-rg)
  ;; https://github.com/jwiegley/use-package/issues/121#issuecomment-237624152
  (nvmap :prefix "SPC" "p" '(:keymap projectile-command-map))
  (:keymaps 'projectile-command-map
   "e" 'projectile-replace
   "r" 'projectile-recentf)
  :init
  (add-hook 'text-mode-hook 'projectile-mode)
  (add-hook 'prog-mode-hook 'projectile-mode)
  :config
  ;; Starting projectile-mode in these hooks is necessary so that tramp does not
  ;; hang on ssh.
  ;; https://github.com/bbatsov/prelude/issues/594#issuecomment-220951394
  (setq projectile-enable-caching t
        projectile-sort-order 'recentf
        projectile-create-missing-test-files t
        projectile-completion-system 'ivy)
  (defun soo--projectile-rg (&optional initial-input)
    "Grep for a string in the current directory or project using rg.
INITIAL-INPUT can be given as the initial minibuffer input."
    (interactive)
    (counsel-rg initial-input (or (ignore-errors (projectile-project-root))
                                  default-directory))))

(use-package rainbow-mode
  :ensure t
  :general
  ("C-c t r" 'rainbow-mode)
  (nmap :prefix "SPC" "tr" #'rainbow-mode)
  :diminish rainbow-mode
  :init
  (add-hook 'css-mode-hook #'rainbow-mode))

(use-package recentf
  :ensure t
  :commands (counsel-recentf)
  :config
  (recentf-mode)
  (setq recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
                          ".*png$" ".*cache$" "^/\\(?:ssh\\|su\\|sudo\\)?:"
                          ".*el.gz$" "/\\.get/.*\\'" "/elpa/\\.*"
                          ".emacs.d/.var/\\.*"))
  (setq recentf-max-saved-items 200
        recentf-auto-cleanup 300))

(use-package reveal-in-osx-finder
  :ensure t
  :if (eq system-type 'darwin)
  :general (nmap "gof" 'reveal-in-osx-finder))

(use-package savehist
  :ensure t
  :defer 1
  :config (savehist-mode))

(use-package saveplace
  :ensure t
  :defer 2
  :if (version< "25" emacs-version)
  :config (save-place-mode))

(use-package semantic
  :ensure t
  :defer t
  :init
  ;; Set semantic to parse only file, local, and project scope.
  (setq semanticdb-find-default-throttle '(file local project))
  (add-hook 'semantic-mode-hook
            (lambda ()
              (dolist (x (default-value 'completion-at-point-functions))
                (when (string-prefix-p "semantic-" (symbol-name x))
                  (remove-hook 'completion-at-point-functions x))))))

(use-package smart-mode-line
  :ensure t
  :disabled t
  :defer t
  :init
  (sml/setup)
  (setq sml/name-width '(29 . 38)
        ;; 29 is just enough to display two eyebrowse numbers at 80 width
        sml/mode-width 'full))

(use-package swift-mode :defer t)

(use-package company-sourcekit :defer t)

(use-package term
  :ensure t
  :general
  (:keymaps 'term-raw-map
   "s-v" 'term-paste
   "M-x" 'counsel-M-x)
  :config
  (setq term-suppress-hard-newline nil
        term-scroll-to-bottom-on-output t
        term-scroll-show-maximum-output t)
  (add-hook 'term-mode-hook (lambda () (toggle-truncate-lines 1)))
  (add-hook 'term-mode-hook (lambda ()
                              (set (make-local-variable 'scroll-margin) 0)))
  (evil-set-initial-state 'term-mode 'emacs))

(use-package vterm
  :commands vterm-create
  :config
  (add-to-list 'vterm-keymap-exceptions "s-1")
  (add-to-list 'vterm-keymap-exceptions "s-j")
  (add-to-list 'vterm-keymap-exceptions "s-k")
  (add-to-list 'vterm-keymap-exceptions "s-h")
  (add-to-list 'vterm-keymap-exceptions "s-l")
  (add-to-list 'vterm-keymap-exceptions "s-2")
  (add-to-list 'vterm-keymap-exceptions "s-3"))

(use-package terminal-here
  :ensure t
  :defer t
  :general
  (nmap "got" #'terminal-here-launch)
  :init
  (setq terminal-here-terminal-command
        (lambda (dir) (list "open" "-a" "iTerm.app" dir))))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :general
  ("s-Z" 'undo-tree-redo
   "s-z" 'undo-tree-undo)
  (nmap "U" 'undo-tree-redo)
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-auto-save-history nil))

(use-package wgrep :defer t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.5)
  (which-key-mode 1))

(use-package window-numbering
  :ensure t
  :general
  (:keymaps 'window-numbering-keymap
   "M-0" nil "M-1" nil "M-2" nil "M-3" nil "M-4" nil
   "M-5" nil "M-6" nil "M-7" nil "M-8" nil "M-9" nil)
  ("s-1" 'select-window-1 "s-2" 'select-window-2
   "s-3" 'select-window-3 "s-4" 'select-window-4 "s-5" 'select-window-5
   "s-6" 'select-window-6)
  :config
  (window-numbering-mode 1))

(use-package winner
  :ensure t
  :general
  (nmap :prefix "SPC" "wu" 'winner-undo)
  :init
  (winner-mode t)
  (setq winner-boring-buffers
        (append winner-boring-buffers '("*Compile-Log*" "*inferior-lisp*"
                                        "*Apropos*" "*cvs*" "*Buffer List*"
                                        "*Ibuffer*"))))

(use-package shackle
  :ensure t
  :init
  (defun shackle-smart-align ()
    (if (< (window-width) 160) 'below 'right))
  (setq shackle-select-reused-windows t
        shackle-rules '((compilation-mode :noselect t)
                        (help-mode :align shackle-smart-align :size 0.4)
                        (undo-tree-visualizer-mode :align t :size 0.3)
                        (woman-mode :popup t)
                        (flycheck-error-list-mode :select t)
                        (cargo-process-mode :align t :size 0.3)
                        ("*cider-result*" :align t :size 0.4)
                        (cider-repl-mode :align t :size 0.4)))
  (shackle-mode 1))

(use-package vlf
  :ensure t
  :defer 3
  :config (require 'vlf-setup))

(use-package yaml-mode :defer t)

;;** Basic Editing
(setq-default fill-column 90)
(add-hook 'text-mode-hook 'visual-line-mode)
(diminish 'auto-fill-function)          ; auto-fill-mode is called this

(use-package simple
  :diminish visual-line-mode
  :general
  (mmap :keymaps 'visual-line-mode
    "k" 'evil-previous-visual-line
    "j" 'evil-next-visual-line)
  (nmap :keymaps '(special-mode-map messages-buffer-mode-map)
    "q" 'quit-window)
  :init
  (add-hook 'visual-line-mode-hook 'evil-normalize-keymaps)
  (column-number-mode))

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :init
  (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package expand-region
  :ensure t
  :general
  ("M-2" 'soo-er-and-insert)
  (nmap :prefix "SPC"
    "v" 'er/expand-region)
  :config
  (defun soo-er-and-insert (arg)
    (interactive "p")
    (progn (evil-insert 1)
           (er/expand-region arg)))
  (setq expand-region-contract-fast-key "1"))

(use-package multiple-cursors
  :ensure t
  :preface
  (define-prefix-command 'endless/mc-map)
  :general
  (vmap "m" 'mc/mark-all-like-this-dwim)
  ;; Malabarba keybinds http://endlessparentheses.com/multiple-cursors-keybinds.html
  (:keymaps 'ctl-x-map
   "m" 'endless/mc-map                  ; C-x m is usually `compose-mail'.
   "C-m" #'mc/mark-all-dwim
   "<return>" mule-keymap)
  ("M-3" #'mc/mark-next-like-this
   "M-4" #'mc/mark-previous-like-this
   "M-#" #'mc/unmark-next-like-this
   "M-$" #'mc/unmark-previous-like-this)
  (:keymaps 'endless/mc-map
   ;; Really really nice!
   "i" #'mc/insert-numbers
   "h" #'mc-hide-unmatched-lines-mode
   "a" #'mc/mark-all-like-this
   ;; Occasionally useful
   "d" #'mc/mark-all-symbols-like-this-in-defun
   "r" #'mc/reverse-regions
   "s" #'mc/sort-regions
   "l" #'mc/edit-lines
   "C-a" #'mc/edit-beginnings-of-lines
   "C-e" #'mc/edit-ends-of-lines))

(use-package hungry-delete :ensure t :defer t)

(use-package eldoc
  :diminish (eldoc-mode . " d"))

;;** Window mgmt
(use-package eyebrowse
  :ensure t
  :init
  (eyebrowse-setup-evil-keys)
  (eyebrowse-mode t)
  :general
  (nmap :keymaps 'eyebrowse-mode-map
        :prefix "g"
    "1" 'eyebrowse-switch-to-window-config-1
    "2" 'eyebrowse-switch-to-window-config-2
    "3" 'eyebrowse-switch-to-window-config-3
    "4" 'eyebrowse-switch-to-window-config-4
    "x" 'eyebrowse-close-window-config)
  (:keymaps 'eyebrowse-mode-map
   "s-1" 'eyebrowse-switch-to-window-config-1
   "s-2" 'eyebrowse-switch-to-window-config-2
   "s-3" 'eyebrowse-switch-to-window-config-3
   "s-4" 'eyebrowse-switch-to-window-config-4
   [C-tab] 'eyebrowse-next-window-config
   [C-S-tab] 'eyebrowse-prev-window-config)
  :config
  (setq eyebrowse-wrap-around t
        eyebrowse-switch-back-and-forth nil))

(use-package window-purpose
  :disabled t
  :defer t
  :config
  (purpose-mode 1)
  (setq purpose-use-default-configuration nil)
  (purpose-compile-user-configuration))

(use-package persp-mode
  :disabled t
  :defer t
  :general
  ("s-p" '(:keymap persp-key-map))
  :init (persp-mode 1)
  :config (setq persp-autokill-buffer-on-remove 'kill-weak))

;;** Completion and expansion
(use-package hippie-exp
  :ensure t
  :general ([remap dabbrev-expand] 'hippie-expand))

(use-package company
  :ensure t
  :diminish (company-mode . "co")
  :general
  (:keymaps 'company-active-map
   "C-n" 'company-select-next
   "C-p" 'company-select-previous
   "C-w" nil
   [tab] 'company-complete-common
   "<escape>" 'soo-company-esc)
  (imap [tab] 'company-indent-or-complete-common)
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-idle-delay 0.3
        company-minimum-prefix-length 2
        company-elisp-detect-function-context nil
        company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-if-just-one-frontend)
        company-backends '(company-elisp
                           company-capf
                           (company-dabbrev-code
                            company-gtags
                            company-keywords)
                           company-files
                           company-dabbrev))
  (defun soo-company-esc () (interactive) (company-abort) (evil-normal-state)))

(use-package company-statistics
  :ensure t
  :defer t
  :after company
  :config (company-statistics-mode))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode))

(require 'server)
(or (server-running-p) (server-start))

(put 'scroll-left 'disabled nil)

;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)

(setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
