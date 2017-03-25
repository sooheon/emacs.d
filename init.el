;;; init.el --- user-init-file

;;* loads
(defvar my-load-paths
  (mapcar (lambda (p) (concat user-emacs-directory p))
          '("lisp"
            "lib/org-mode/contrib/lisp"
            "lib/org-mode/lisp"
            "lisp/themes"
            "lisp/modes"
            "lib/clojure-semantic"
            "lib/lpy"
            "lib/no-littering"
            "lib/org-mode"
            "lib/soap"
            "lib/structured-haskell-mode")))

(mapc (apply-partially 'add-to-list 'load-path) my-load-paths)

(load "loaddefs.el" nil 'nomessage)
(load "auto.el" 'noerror 'nomessage)

;;* customize
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(load custom-file 'noerror)
;;** font
(set-face-attribute 'default nil :family "Input Mono Narrow")
(set-fontset-font "fontset-default" 'hangul '("NanumGothic" . "unicode-bmp"))
;;** decorations
(setq tool-bar-mode nil)
(setq menu-bar-mode nil)
(setq scroll-bar-mode nil)
(setq line-spacing 0.1)
(setq inhibit-startup-screen t
      initial-scratch-message ";; You have power over your mind - not outside events. Realize this, and you \n;; will find strength.\n\n"
      create-lockfiles nil
      window-combination-resize t)
(eval '(setq inhibit-startup-echo-area-message "sooheon"))
(blink-cursor-mode -1)
(setq blink-cursor-blinks 0)
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
(put 'scroll-left 'disabled nil)
(setq scroll-margin 2
      scroll-preserve-screen-position t
      scroll-conservatively 101)
(progn ;; Deal with large files
  ;; (setq jit-lock-defer-time 0)
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

;;** internals
(setq gc-cons-threshold (* 12 1024 1024))

;;** shell
(when (executable-find "fish")
  (setq shell-file-name "/usr/local/bin/fish"
        explicit-shell-file-name "/usr/local/bin/fish"))
(setenv "LANG" "en_US.UTF-8")

;;* Bootstrap
;;** Package init
(setq no-littering-etc-directory (expand-file-name ".etc/" user-emacs-directory)
      no-littering-var-directory (expand-file-name ".var/" user-emacs-directory))
(require 'no-littering)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ;; ("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("org" . "http://orgmode.org/elpa/")
                         ))
(package-initialize)
(with-eval-after-load 'evil
  (evil-set-initial-state 'package-menu-mode 'emacs))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

(use-package async
  :config
  (async-bytecomp-package-mode t))

;;** Set up environment
(use-package exec-path-from-shell
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;;** OSX
(use-package osx-trash
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
  :demand t
  :config
  (general-evil-setup t t)
  (load "keybinds.el" nil t))

;;** Mode Requires
(require 'soo-evil)
(require 'soo-ivy)
;; (require 'soo-rust)
(require 'soo-clojure)
;; (require 'soo-ess)
(require 'soo-python)
;; (add-hook 'haskell-mode-hook 'soo-haskell-hook)
(add-hook 'org-mode-hook 'soo-org-hook)
(run-with-idle-timer 10 nil (lambda () (require 'soo-org)))

;;** general modes config
(use-package auto-compile
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
  :commands spacemacs/avy-open-url
  :general
  ("s-g" 'evil-avy-goto-word-1
   [remap goto-line] 'evil-avy-goto-line)
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
  :commands (ace-link-info ace-link-woman ace-link-help ace-link-custom)
  :general
  (:keymaps 'help-mode-map "o" 'ace-link-help)
  :config
  (ace-link-setup-default))

(use-package ace-window
  :general
  ("C-x o" 'ace-window)
  :config
  (setq aw-keys sooheon-avy-keys))

(use-package autorevert
  :defer 2
  :diminish auto-revert-mode
  :config
  (setq global-auto-revert-non-file-buffers t ; revert Dired buffers too
        auto-revert-use-notify nil            ; OSX doesn't have file-notify
        auto-revert-verbose nil))

(use-package compile
  :defer t
  :init
  (define-key prog-mode-map [f9] #'compile)
  :config
  (setq compilation-ask-about-save nil
        compilation-scroll-output 'next-error
        compilation-skip-threshold 2))

(use-package conf-mode
  :ensure nil
  :mode ("/\\.[^/]*rc" . conf-mode))

(use-package cc-mode
  :ensure nil
  :defer t
  :mode ("\\.h\\'" . c-mode)
  :config
  (c-toggle-auto-hungry-state 1))

(use-package function-args
  :defer t
  :config
  (fa-config-default))

(use-package irony
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  :config
  (defun soo-irony-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async)
    (irony-cdb-autosetup-compile-options))
  (add-hook 'irony-mode-hook #'soo-irony-hook))

(use-package ggtags
  :defer t
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))

(use-package dired
  :ensure nil
  :commands dired-jump
  :general
  (nmap "-" 'dired-jump)
  :config
  (nmap :keymaps 'dired-mode-map
    "-" 'dired-jump
    "gg" '(lambda () (interactive) (beginning-of-buffer) (dired-next-line 1))
    "got" 'soo-terminal-pop
    "gof" 'reveal-in-osx-finder
    "G" '(lambda () (interactive) (end-of-buffer) (dired-next-line -1))
    "=" 'vinegar/dired-diff
    "I" 'vinegar/dotfiles-toggle
    "~" '(lambda () (interactive) (find-alternate-file "~/"))
    "<return>" 'dired-find-file
    "f" 'counsel-find-file
    "J" 'dired-goto-file
    "C-f" nil                           ; 'find-name-dired
    "H" 'diredp-dired-recent-dirs
    "T" 'dired-tree-down
    "K" 'dired-do-kill-lines
    "r" 'revert-buffer
    "C-r" 'dired-do-redisplay)
  (setq dired-listing-switches "-alGh1v --group-directories-first")
  (defvar dired-dotfiles-show-p)
  (defun soo--dired-setup ()
    ;; (setq dired-omit-verbose nil)
    (setq dired-hide-details-hide-symlink-targets nil)
    (dired-hide-details-mode -1))
  (add-hook 'dired-mode-hook 'soo--dired-setup))

(use-package ediff
  :defer t
  :commands (ediff-buffers ediff)
  :config
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally
                ediff-merge-split-window-function 'split-window-horizontally)
  (add-hook 'ediff-quit-hook #'winner-undo))

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :general
  (nmap :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point))

(use-package circe
  :defer t
  :general
  (nmap :prefix "SPC" "i" 'sooheon--switch-to-circe)
  (:prefix "C-c" "i" 'sooheon--switch-to-circe)
  :init
  (defun sooheon--switch-to-circe ()
    "Switch to CIRCE buffers using completing-read, or start
CIRCE if no buffers open."
    (interactive)
    (let ((candidates (list)))
      (dolist (buf (buffer-list) candidates)
        (if (memq (with-current-buffer buf major-mode)
                  '(circe-channel-mode circe-server-mode circe-query-mode))
            (setq candidates (append (list (buffer-name buf)) candidates))))
      (if candidates
          (switch-to-buffer (completing-read "IRC buffer: " candidates))
        ;; (circe "EsperNet")
        (circe "Freenode"))))
  :config
  (setq circe-reduce-lurker-spam t
        tracking-position 'end)
  (enable-circe-color-nicks)
  (enable-lui-track-bar)
  (require 'lui-autopaste)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste))

(use-package circe-notifications
  :disabled t
  :defer t
  :init
  (add-hook 'circe-server-connected-hook 'enable-circe-notifications)
  :config
  (setq circe-notifications-alert-style 'osx-notifier))

(use-package dabbrev
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
  :defer t
  :config
  (evil-set-initial-state 'flycheck-error-list-mode 'insert)
  (setq flycheck-indication-mode nil
        flycheck-mode-line-prefix "fc"))

(use-package gist :defer t)

(use-package hydra
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
  :ensure nil
  :init
  (setq help-window-select t)
  :config
  (evil-set-initial-state 'help-mode 'emacs)
  (add-hook 'help-mode-hook (lambda () (toggle-truncate-lines -1))))

(use-package hl-todo
  :defer t
  :init (add-hook 'prog-mode-hook 'hl-todo-mode))

(use-package highlight-escape-sequences
  :defer t
  :init (add-hook 'prog-mode-hook 'hes-mode))

(use-package info
  :ensure nil
  :general
  (nmap :prefix "SPC" "hi" 'info)
  :config
  (evil-set-initial-state 'Info-mode 'emacs))

(use-package golden-ratio
  :general
  (nmap :prefix "SPC" "tg" 'golden-ratio))

;;** Parens and lisp
(use-package smartparens
  :diminish (smartparens-mode . "sp")
  :defer t
  :general
  (:keymaps 'smartparens-mode-map
   "C-M-a" 'sp-beginning-of-sexp "C-M-e" 'sp-end-of-sexp
   "M-r" 'sp-raise-sexp
   [C-backspace] 'sp-backward-kill-sexp
   "C-)" 'sp-forward-slurp-sexp "C-(" 'sp-backward-slurp-sexp
   "C-{" 'sp-backward-barf-sexp "C-}" 'sp-forward-barf-sexp
   "M-S" 'sp-splice-sexp-killing-backward
   [M-up] 'sp-splice-sexp-killing-backward
   [M-down] 'sp-splice-sexp-killing-forward)
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
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil))

(use-package lispy
  :diminish (lispy-mode . "ly")
  :general
  (:keymaps 'lispy-mode-map-c-digits
   "C-8" 'lispy-out-forward-newline
   "C-9" 'lispy-parens-down)
  (:keymaps 'lispy-mode-map-special "+" nil)
  ;; Unbind M-k and M-. in normal state, pass through to lispy
  (:keymaps 'evil-normal-state-map
   "M-." nil                            ; evil-repeat-pop-next
   "M-k" nil
   "gd" 'lispy-goto-symbol)
  :init
  (defun enable-lispy-for-lisps ()
    (when (or (member major-mode sp-lisp-modes)
              (eq this-command 'eval-expression))
      (lispy-mode 1)))
  (add-hook 'smartparens-enabled-hook 'enable-lispy-for-lisps)
  (add-hook 'smartparens-disabled-hook (lambda () (lispy-mode -1)))
  (setq iedit-toggle-key-default nil)   ; Don't want to use iedit
  :config
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
    "(" 'lispy-parens
    "[" 'lispy-brackets
    "{" 'lispy-braces
    ;; "(" nil
    ;; "[" nil
    ;; "{" nil
    "\"" nil
    "C-a" nil
    "]" nil
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
    ;; "<return>" 'lispy-alt-line
    "M-RET" 'lispy-meta-return))

(use-package lispyville
  :diminish lispyville-mode
  :general
  ([remap evil-normal-state] 'lispyville-normal-state)
  (:keymaps 'lispyville-mode-map
   "M-n" 'lispyville-drag-forward
   "M-p" 'lispyville-drag-backward)
  :init
  (defun conditionally-enable-lispyville ()
    "Only turn on lispyville outside of REPLs.
Keep M-n and M-p reserved for history."
    (unless (or (memq major-mode '(cider-repl-mode))
                (eq this-command 'eval-expression))
      (lispyville-mode 1)))
  ;; (add-hook 'lispy-mode-hook #'conditionally-enable-lispyville)
  (add-hook 'lispy-mode-hook 'lispyville-mode)
  (setq lispyville-key-theme '(operators
                               escape
                               slurp/barf-cp)
        lispyville-barf-stay-with-closing t))

;;*** Emacs lisp
(use-package suggest
  :commands suggest
  :config
  (sp-local-pair 'suggest-mode "'" nil :actions nil))

;;** Git and version control
(use-package magit
  :general
  (nvmap :prefix "SPC"
    "g" 'magit-status)
  (:keymaps 'magit-mode-map
   "SPC" 'scroll-up
   "DEL" 'scroll-down)
  :config
  (evil-set-initial-state 'magit-submodule-list-mode 'insert)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
        magit-popup-show-common-commands nil)
  (defvar hmb--count)
  (defhydra hydra-magit-blame
    (:color pink
     :body-pre (progn (setq hmb--count 1) ; var to track recursive blames
                      (call-interactively 'magit-blame)))
    "Magit blame"
    ("b" (progn (call-interactively 'magit-blame)
                (setq hmb--count (1+ hmb--count))) "backwards")
    ("f" (progn (magit-blame-quit)
                ;; once magit-blame-mode is no-longer active, quit hydra
                (unless (bound-and-true-p magit-blame-mode)
                  (setq hydra-deactivate t))) "forwards")
    ("q" (dotimes (i hmb--count)
           (call-interactively 'magit-blame-quit)) "quit"
           :color blue)))

(use-package diff-hl
  :defer 2
  :init
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (evil-set-initial-state 'diff-mode 'emacs))

(use-package markdown-mode
  :mode ("\\.m[k]d" . markdown-mode)
  :general
  (nmap :keymaps 'markdown-mode-map
    "gj" 'outline-forward-same-level
    "gk" 'outline-backward-same-level
    "gh" 'outline-up-heading
    ;; next visible heading is not exactly what we want but close enough
    "gl" 'outline-next-visible-heading))

(use-package super-save
  :diminish super-save-mode
  :init
  (super-save-mode 1))

(use-package woman
  :ensure nil
  :defer t
  :config (evil-set-initial-state 'woman-mode 'emacs)
  (bind-key "s-w" 'Man-quit woman-mode-map))

(use-package projectile
  :commands (projectile-switch-project projectile-find-file projectile-find-dir)
  :diminish projectile-mode
  :general
  ("C-c k" 'soo--projectile-rg)
  ;; https://github.com/jwiegley/use-package/issues/121#issuecomment-237624152
  (nvmap :prefix "SPC" "p" '(:keymap projectile-command-map))
  (:keymaps 'projectile-command-map
   "e" 'projectile-replace
   "r" 'projectile-recentf)
  :config
  ;; Starting projectile-mode in these hooks is necessary so that tramp does not
  ;; hang on ssh.
  ;; https://github.com/bbatsov/prelude/issues/594#issuecomment-220951394
  (add-hook 'text-mode-hook 'projectile-mode)
  (add-hook 'prog-mode-hook 'projectile-mode)
  (setq projectile-enable-caching t
        projectile-sort-order 'recentf
        projectile-create-missing-test-files t
        projectile-completion-system 'ivy)
  (defun soo--projectile-rg (&optional initial-input)
    "Grep for a string in the current directory or project using ag.
INITIAL-INPUT can be given as the initial minibuffer input."
    (interactive)
    (counsel-rg initial-input (or (ignore-errors (projectile-project-root))
                                  default-directory))))

(use-package rainbow-mode
  :general
  ("C-c t r" 'rainbow-mode)
  (nmap :prefix "SPC" "tr" #'rainbow-mode)
  :diminish rainbow-mode
  :init
  (add-hook 'css-mode-hook #'rainbow-mode))

(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  (setq recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
                          ".*png$" ".*cache$" "^/\\(?:ssh\\|su\\|sudo\\)?:"
                          ".*el.gz$" "/\\.get/.*\\'" "/elpa/\\.*"
                          ".emacs.d/.var/\\.*"))
  (setq recentf-max-saved-items 200
        recentf-auto-cleanup 300))

(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
  :general (nmap "gof" 'reveal-in-osx-finder))

(use-package savehist
  :ensure nil
  :defer 1
  :config (savehist-mode))

(use-package saveplace
  :ensure nil
  :if (version< "25" emacs-version)
  :config (save-place-mode))

(use-package semantic
  :defer t
  :init
  ;; Set semantic to parse only file, local, and project scope.
  (setq semanticdb-find-default-throttle '(file local project))
  (add-hook 'semantic-mode-hook
            (lambda ()
              (dolist (x (default-value 'completion-at-point-functions))
                (when (string-prefix-p "semantic-" (symbol-name x))
                  (remove-hook 'completion-at-point-functions x))))))

(use-package term
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

(use-package undo-tree
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
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.5)
  (which-key-mode 1))

(use-package window-numbering
  :general
  (:keymaps 'window-numbering-keymap
   "M-0" nil "M-1" nil "M-2" nil "M-3" nil "M-4" nil
   "M-5" nil "M-6" nil "M-7" nil "M-8" nil "M-9" nil)
  ("s-0" 'select-window-0 "s-1" 'select-window-1 "s-2" 'select-window-2
   "s-3" 'select-window-3 "s-4" 'select-window-4 "s-5" 'select-window-5
   "s-6" 'select-window-6)
  :config
  (window-numbering-mode 1))

(use-package winner
  :general
  (nmap :prefix "SPC" "wu" 'winner-undo)
  :init
  (winner-mode t)
  (setq winner-boring-buffers
        (append winner-boring-buffers '("*Compile-Log*" "*inferior-lisp*"
                                        "*Apropos*" "*cvs*" "*Buffer List*"
                                        "*Ibuffer*"))))

(use-package shackle
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
  :defer t
  :config (require 'vlf-setup))

(use-package yaml-mode :defer t)

(use-package eldoc
  :defer t
  :config
  (global-eldoc-mode -1))

;;** Basic Editing
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'visual-line-mode)
(diminish 'auto-fill-function)          ; auto-fill-mode is called this

(use-package simple
  :ensure nil
  :diminish visual-line-mode
  :general
  (mmap :keymaps 'visual-line-mode
    "k" 'evil-previous-visual-line
    "j" 'evil-next-visual-line)
  :init
  (add-hook 'visual-line-mode-hook 'evil-normalize-keymaps)
  (evil-set-initial-state 'messages-buffer-mode 'insert)
  (evil-set-initial-state 'special-mode 'insert)
  :config
  (column-number-mode 1))

(use-package ws-butler
  :diminish ws-butler-mode
  :init
  (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package expand-region
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
   "C-e" #'mc/edit-ends-of-lines)
  :config
  (defalias 'mc/cursor-is-bar (lambda () ())))

(use-package hungry-delete :defer t)

;;** Completion and expansion
(use-package hippie-exp
  :ensure nil
  :general ([remap dabbrev-expand] 'hippie-expand))

(use-package company
  :diminish (company-mode . "co")
  :general
  (:keymaps 'company-active-map
   "C-n" 'company-select-next
   "C-p" 'company-select-previous
   "C-w" nil
   [tab] 'company-complete-common
   "<escape>" 'soo-company-esc)
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-idle-delay 0.4
        company-minimum-prefix-length 3
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
  :after company
  :config (company-statistics-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode))

(require 'server)
(or (server-running-p) (server-start))

(put 'scroll-left 'disabled nil)

;; (load "my-easypg")

;;; init.el ends here
