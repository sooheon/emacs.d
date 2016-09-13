;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

;;* Base directory
(defvar emacs-d (directory-file-name "~/.emacs.d/")
  "The giant turtle on which the world rests.")
(setq package-user-dir (expand-file-name "elpa" emacs-d))
(defvar lisp-d (expand-file-name "lisp" emacs-d))
(defvar lib-d (expand-file-name "lib/" emacs-d))
(mapc (lambda (x)
        (add-to-list 'load-path (expand-file-name x lib-d)))
      (delete ".." (directory-files lib-d)))
;;** load some packages manually
(add-to-list 'load-path (expand-file-name "lisp" emacs-d))
(add-to-list 'load-path (expand-file-name "lib/org-mode/contrib/lisp" emacs-d))
(add-to-list 'load-path (expand-file-name "lib/org-mode/lisp" emacs-d))
(add-to-list 'load-path (expand-file-name "lisp/modes" emacs-d))
(add-to-list 'load-path (expand-file-name "lisp/themes" emacs-d))

;;** font
(add-to-list 'default-frame-alist '(font . "Input Mono Narrow"))
;;* customize
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))
(csetq custom-file (expand-file-name "custom.el" lisp-d))
(when (file-exists-p custom-file) (load custom-file))
;;** decorations
(csetq tool-bar-mode nil)
(csetq menu-bar-mode nil)
(csetq scroll-bar-mode nil)
(csetq inhibit-startup-screen t)
(csetq initial-scratch-message ";; You have power over your mind - not outside events. Realize this, and you \n;; will find strength.\n\n")
(csetq create-lockfiles nil)
(global-hl-line-mode 1)
(csetq line-spacing 0.1)
(blink-cursor-mode -1)
(csetq blink-cursor-blinks 0)
(eval '(setq inhibit-startup-echo-area-message "sooheon"))
(csetq frame-title-format '((:eval (if buffer-file-name
                                       (abbreviate-file-name buffer-file-name)
                                     "%b"))))
(csetq window-combination-resize t)
(csetq fringe-indicator-alist '((continuation nil right-curly-arrow) (truncation left-arrow right-arrow) (continuation left-curly-arrow right-curly-arrow) (overlay-arrow . right-triangle) (up . up-arrow) (down . down-arrow) (top top-left-angle top-right-angle) (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle) (top-bottom left-bracket right-bracket top-right-angle top-left-angle) (empty-line . empty-line) (unknown . question-mark)))
;;** minibuffer interaction
(csetq enable-recursive-minibuffers t)
(setq minibuffer-message-timeout 1)
(minibuffer-depth-indicate-mode 1)
;;** editor behavior
(setq scroll-preserve-screen-position t)
;; (setq scroll-margin 3)
;; (setq scroll-conservatively 101)
(setq lisp-indent-function 'Fuco1/lisp-indent-function)
(csetq vc-follow-symlinks t)
(csetq find-file-suppress-same-file-warnings t)
(csetq read-file-name-completion-ignore-case t)
(csetq read-buffer-completion-ignore-case t)
(prefer-coding-system 'utf-8)
(electric-indent-mode -1)
(csetq truncate-lines nil)
(csetq default-input-method "korean-hangul")
(csetq indent-tabs-mode nil)
(setq ring-bell-function 'ignore)
(csetq highlight-nonselected-windows t)
(csetq backup-inhibited t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions nil)
(csetq load-prefer-newer t)
(csetq recenter-positions '(top middle bottom))
(csetq mac-pass-command-to-system nil) ; https://github.com/railwaycat/emacs-mac-port/issues/78
(add-hook 'server-switch-hook 'raise-frame)
(csetq eval-expression-print-length nil)
(csetq eval-expression-print-level nil)
(csetq sentence-end-double-space nil)
(csetq search-default-mode 'char-fold-to-regexp)
(csetq resize-mini-windows t)
;;** internals
(csetq gc-cons-threshold (* 10 1024 1024))
(csetq ad-redefinition-action 'accept)
;;** shell
(csetq shell-file-name "/usr/local/bin/fish")
(csetq explicit-shell-file-name "/usr/local/bin/fish")

;; Use right command as control
(setq mac-right-command-modifier 'control)

;;* Bootstrap
;;** Package init
(require 'no-littering)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
(with-eval-after-load 'evil
  (evil-set-initial-state 'package-menu-mode 'insert))
(eval-and-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;;** Set up environment
(use-package exec-path-from-shell
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (csetq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;;** OSX
(use-package osx-trash
  :if (eq system-type 'darwin)
  :init
  (csetq delete-by-moving-to-trash t)
  (osx-trash-setup))

;;** Themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes" lisp-d))
(require 'soo-themes)
(load-theme 'eclipse2 t)

;;** Evil
(setq evil-want-C-u-scroll t
      evil-cross-lines t
      evil-symbol-word-search t
      evil-move-cursor-back nil
      evil-want-C-i-jump t
      evil-disable-insert-state-bindings t
      evil-search-module 'evil-search
      evil-ex-search-persistent-highlight nil
      evil-want-Y-yank-to-eol t
      evil-ex-substitute-global t)
(require 'evil)
(evil-mode 1)
(evil-define-key 'normal global-map "U" 'undo-tree-redo)
(define-key evil-insert-state-map "\C-w" 'evil-delete-backward-word)

(use-package general
  :config
  (general-evil-setup t)
  (setq general-vim-definer-default 'states))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :general (nmap "gc" 'evil-commentary
                 "gy" 'evil-commentary-yank))

(use-package evil-cleverparens
  :general
  (otomap "f" 'evil-cp-a-form
          "c" 'evil-cp-a-comment
          "d" 'evil-cp-a-defun)
  (itomap "f" 'evil-cp-inner-form
          "c" 'evil-cp-inner-comment
          "d" 'evil-cp-inner-defun))

(use-package evil-exchange
  :diminish evil-exchange
  :general
  (omap "x" 'evil-exchange/cx)
  (vmap "X" 'evil-exchange))

(use-package evil-matchit
  :defer t
  :init (add-hook 'html-mode-hook 'turn-on-evil-matchit-mode))

(use-package evil-textobj-anyblock
  :general
  (itomap "b" 'evil-textobj-anyblock-inner-block)
  (otomap "b" 'evil-textobj-anyblock-a-block))

(use-package evil-visualstar
  :general
  (vmap "*" 'evil-visualstar/begin-search-forward
        "#" 'evil-visualstar/begin-search-backward))

(use-package evil-surround
  :general
  (vmap :keymaps 'evil-surround-mode-map
    "s" 'evil-surround-region
    "gs" 'evil-Surround-region
    "S" 'evil-substitute)
  :config
  (global-evil-surround-mode 1)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '(?` . ("`" . "'")) evil-surround-pairs-alist))))

(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :init
  (setq evil-snipe-use-vim-sneak-bindings t
        evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'buffer
        evil-snipe-smart-case t
        evil-snipe-repeat-keys nil)
  (evil-snipe-override-mode 1))

(use-package evil-numbers
  :general
  (nmap "C-S-a" 'evil-numbers/inc-at-pt
        "C-S-x" 'evil-numbers/dec-at-pt))

;;** mode hooks
(add-hook 'python-mode-hook 'soo-python-hook)
(add-hook 'clojure-mode-hook 'soo-clojure-hook)
(add-hook 'org-mode-hook 'soo-org-hook)
(run-with-idle-timer 5 nil (lambda () (require 'soo-org)))
(add-hook 'haskell-mode-hook 'soo-haskell-hook)
(require 'soo-ivy)
(require 'soo-rust)

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

(defvar sooheon--avy-keys '(?w ?e ?r ?s ?d ?x ?c ?u ?i ?o ?v ?n ?m ?l ?k ?j ?f))

(use-package avy
  :commands spacemacs/avy-open-url
  :general
  ("s-g" 'evil-avy-goto-word-1
   [remap goto-line] 'evil-avy-goto-line)
  (nmap :prefix "SPC" "xo" 'spacemacs/avy-open-url)
  :config
  (setq avy-keys sooheon--avy-keys)
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
  (nmap "s-o" 'ace-link-addr)
  :config
  (ace-link-setup-default))

(use-package autorevert
  :diminish auto-revert-mode
  :init (global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t ; revert Dired buffers too
        auto-revert-use-notify nil            ; OSX doesn't have file-notify
        nil auto-revert-verbose))

(use-package artbollocks-mode
  :diminish (artbollocks-mode . "ab")
  :general (nmap :prefix "SPC" "ta" 'artbollocks-mode))

(use-package compile
  :defer t
  :init
  (define-key prog-mode-map [f9] #'compile)
  :config
  (setq compilation-ask-about-save nil
        compilation-scroll-output 'next-error
        compilation-skip-threshold 2))

(use-package cc-mode
  :ensure nil
  :defer t
  :mode ("\\.h\\'" . c-mode)
  :config
  (c-toggle-auto-hungry-state 1))

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
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'soo-irony-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

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
    "C-f" 'find-name-dired
    "H" 'diredp-dired-recent-dirs
    "T" 'dired-tree-down
    "K" 'dired-do-kill-lines
    "r" 'revert-buffer
    "C-r" 'dired-do-redisplay)
  :init
  (add-hook 'dired-mode-hook #'soo--dired-setup)
  :config
  (setq dired-listing-switches "-laGh1v --group-directories-first")
  (defvar dired-dotfiles-show-p)
  (defun soo--dired-setup ()
    ;; (setq dired-omit-verbose nil)
    (setq dired-hide-details-hide-symlink-targets nil)
    (dired-hide-details-mode t)))

(use-package ediff
  :defer t
  :commands (ediff-buffers ediff)
  :config
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally
                ediff-merge-split-window-function 'split-window-horizontally)
  (add-hook 'ediff-quit-hook #'winner-undo))

(use-package eldoc
  :diminish eldoc-mode
  :config
  (global-eldoc-mode -1))

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :general
  (nmap :keymaps 'emacs-lisp-mode-map
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point)
  (nmap :keymaps 'lisp-interaction-mode-map
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point))

(use-package circe
  :defer t
  :general
  (nmap :prefix "SPC" "i" 'sooheon--switch-to-circe)
  :init
  (setq circe-network-options
        '(("Freenode"
           :nick "sooheon"
           :channels ("#emacs" "#clojure" "#haskell" "##crawl"
                      ;; "#lesswrong"
                      )
           :nickserv-password "qwefasdf")))
  (defun sooheon--switch-to-circe ()
    "Switch to CIRCE buffers using completing-read, or start
CIRCE if no buffers open."
    (interactive)
    (let ((candidates (list)))
      (dolist (buf (buffer-list) candidates)
        (if (memq (with-current-buffer buf major-mode)
                  '(circe-channel-mode circe-server-mode))
            (setq candidates (append (list (buffer-name buf)) candidates))))
      (if candidates
          (switch-to-buffer (completing-read "IRC buffer: " candidates))
        (circe "Freenode"))))
  :config
  (setq circe-reduce-lurker-spam t
        tracking-position 'end)
  (enable-circe-color-nicks)
  (require 'lui-autopaste)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste))

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
  :init
  (setq speck-hunspell-coding-system 'utf-8
        speck-hunspell-dictionary-alist '(("en" . "en_US"))
        speck-hunspell-default-dictionary-name "en"
        speck-hunspell-library-directory "/Library/Spelling/"
        speck-hunspell-minimum-word-length 3
        speck-auto-correct-case 'two)
  ;; (defun soo--speck-prog-hook ()
  ;;   (set (make-local-variable 'speck-syntactic) t)
  ;;   (set (make-local-variable 'speck-face-inhibit-list)
  ;;        '(font-lock-constant-face))
  ;;   (speck-mode))
  ;; (add-hook 'prog-mode-hook 'soo--speck-prog-hook)
  (add-hook 'text-mode-hook 'speck-mode))

(use-package help
  :ensure nil
  :init
  (csetq help-window-select t)
  :config
  (evil-set-initial-state 'help-mode 'insert)
  (add-hook 'help-mode-hook (lambda () (toggle-truncate-lines -1))))

(use-package highlight-escape-sequences
  :defer
  :init (add-hook 'prog-mode-hook 'hes-mode))

(use-package info
  :ensure nil
  :general
  (nmap :prefix "SPC" "hi" 'info)
  :config
  (evil-set-initial-state 'Info-mode 'insert))

;;** Parens and lisp
(use-package smartparens
  :diminish (smartparens-mode . "sp")
  :defer t
  :general
  (:keymaps 'smartparens-mode-map
   "M-a" 'sp-beginning-of-sexp
   "M-e" 'soo-end-of-sexp-or-next
   [C-backspace] 'sp-backward-kill-sexp
   "C-)" 'sp-forward-slurp-sexp
   "C-(" 'sp-backward-slurp-sexp
   "C-{" 'sp-backward-barf-sexp
   "C-}" 'sp-forward-barf-sexp)
  (nmap :keymaps 'smartparens-mode-map
    "M-i" 'soo-insert-at-bos
    "M-a" 'soo-insert-at-eos)
  :init
  (add-hook 'prog-mode-hook 'smartparens-strict-mode)
  (add-hook 'smartparens-strict-mode-hook 'show-smartparens-mode)
  (add-hook 'smartparens-mode-hook 'show-smartparens-mode)
  :config
  (require 'smartparens-config)
  (setq sp-cancel-autoskip-on-backward-movement nil
        sp-autoskip-closing-pair 'always
        sp-show-pair-from-inside nil
        sp-show-pair-delay 0
        sp-highlight-pair-overlay nil)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (defun soo-end-of-sexp-or-next ()
    (interactive)
    (if (or (looking-at "[])}]") (eolp))
        (sp-end-of-next-sexp)
      (sp-end-of-sexp)))
  (defun soo-insert-at-bos ()
    (interactive)
    (progn (sp-beginning-of-sexp) (evil-insert-state)))
  (defun soo-insert-at-eos ()
    (interactive)
    (progn (sp-end-of-sexp) (evil-insert-state))))

(use-package lispy
  :diminish lispy-mode
  :defer t
  :general
  (:keymaps 'lispy-mode-map-paredit
   "C-a" nil
   "M-j" 'lispy-split
   "M-k" 'lispy-kill-sentence
   [M-up] 'sp-splice-sexp-killing-backward
   [M-down] 'sp-splice-sexp-killing-forward
   ;; (define-key map (kbd "C-,") 'lispy-kill-at-point)
   "M-n" nil                            ; lispy left
   "M-p" nil
   "\"" 'lispy-doublequote
   "C-d" 'lispy-delete
   "M-S" 'sp-splice-sexp-killing-backward
   "M-)" nil
   "DEL" 'lispy-delete-backward
   "C-)" nil
   "C-(" nil
   "C-}" nil
   "C-{" nil)
  ;; Unbind M-k and M-. in evil normal state and use lispy
  (:keymaps 'evil-normal-state-map
   "M-." nil                            ; evil-repeat-pop-next
   "M-k" nil)
  :init
  (defun conditionally-enable-lispy ()
    "Enable `lispy-mode' in the minibuffer, during `eval-expression'."
    (if (eq this-command 'eval-expression)
        (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)
  (defun toggle-lispy-for-lisps (arg)
    (lambda () (when (member major-mode sp-lisp-modes) (lispy-mode arg))))
  (add-hook 'smartparens-enabled-hook (toggle-lispy-for-lisps 1))
  (add-hook 'smartparens-disabled-hook (toggle-lispy-for-lisps -1))
  (csetq iedit-toggle-key-default nil) ; Don't want to use iedit
  :config
  (lispy-set-key-theme '(special c-digits paredit))
  (setq lispy-compat '(edebug cider)
        lispy-avy-keys sooheon--avy-keys
        lispy-avy-style-paren 'at-full
        lispy-avy-style-symbol 'at-full
        lispy-delete-backward-recenter nil
        lispy-safe-paste t
        lispy-safe-copy t
        lispy-safe-delete t
        lispy-comment-use-single-semicolon t)
  (add-to-list 'lispy-parens-preceding-syntax-alist
               '(clojurescript-mode . ("[`'~@]+" "\\|" "#" "\\|" "#\\?@?")))
  (lispy-define-key lispy-mode-map-special ">" 'lispy-slurp-or-barf-right)
  (lispy-define-key lispy-mode-map-special "<" 'lispy-slurp-or-barf-left))

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
  (add-hook 'lispy-mode-hook #'conditionally-enable-lispyville)
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
  (nmap :prefix "SPC"
        "g" 'magit-status
        "G" 'magit-dispatch-popup)
  :config
  (evil-set-initial-state 'magit-submodule-list-mode 'insert)
  (evil-set-initial-state 'magit-status-mode 'insert)
  (setq magit-display-buffer-function
        'magit-display-buffer-fullframe-status-v1))

(use-package diff-hl
  :after (projectile magit)
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package markdown-mode
  :mode ("\\.m[k]d" . markdown-mode)
  :general
  (nmap :keymaps 'markdown-mode-map
    "gj" 'outline-forward-same-level
    "gk" 'outline-backward-same-level
    "gh" 'outline-up-heading
    ;; next visible heading is not exactly what we want but close enough
    "gl" 'outline-next-visible-heading)
  :config
  (add-hook 'markdown-mode-hook (lambda () (auto-fill-mode 1))))

(use-package super-save
  :diminish super-save-mode
  :init
  (csetq auto-save-default nil)
  :config
  (super-save-mode 1))

(use-package pdf-tools
  :disabled t
  :defer t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :init
  (setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")
  :config
  (pdf-tools-install))

(use-package woman
  :ensure nil
  :defer t
  :config (evil-set-initial-state 'woman-mode 'insert)
  (bind-key "s-w" 'Man-quit woman-mode-map))

(use-package projectile
  :diminish projectile-mode
  :commands (projectile-switch-project
             projectile-find-file
             projectile-find-dir
             projectile-dired
             projectile-recentf
             counsel-projectile-ag)
  :general
  ("C-c k" 'counsel-projectile-ag)
  (nmap :prefix "SPC"
    "pd" 'projectile-find-dir
    "pD" 'projectile-dired
    "p!" 'projectile-run-shell-command-in-root
    "p&" 'projectile-run-async-shell-command-in-root
    "p%" 'projectile-replace-regexp
    "pk" 'projectile-kill-buffers
    "pa" 'projectile-find-other-file
    "pt" 'projectile-toggle-between-implementation-and-test
    "po" 'projectile-multi-occur
    "pR" 'projectile-replace
    "pT" 'projectile-find-test-file
    "pP" 'projectile-test-project
    "pm" 'projectile-commander
    "/" 'counsel-projectile-ag)
  :config
  (projectile-global-mode)
  (defun counsel-projectile-ag (&optional initial-input)
    "Grep for a string in the current directory or project using ag.
INITIAL-INPUT can be given as the initial minibuffer input."
    (interactive)
    (counsel-ag initial-input (or (ignore-errors (projectile-project-root))
                                  default-directory)))
  (setq projectile-enable-caching t
        projectile-sort-order 'recentf
        projectile-create-missing-test-files t
        projectile-completion-system 'ivy))

(use-package counsel-projectile
  :after projectile
  :init
  (nmap :prefix "SPC"
    "pp" 'counsel-projectile
    "pb" 'counsel-projectile-switch-to-buffer
    "pd" 'counsel-projectile-find-dir
    "pf" 'counsel-projectile-find-file
    "pr" 'projectile-recentf
    "ps" 'counsel-projectile)
  :config
  (setq projectile-switch-project-action 'counsel-projectile-find-file)
  (ivy-set-actions
   'counsel-projectile
   '(("d" (lambda (dir)
            (let ((projectile-switch-project-action 'projectile-dired))
              (projectile-switch-project-by-name dir arg)))
      "find directory")
     ("b" (lambda (dir)
            (let ((projectile-switch-project-action 'counsel-projectile-switch-to-buffer))
              (projectile-switch-project-by-name dir arg)))
      "switch to buffer")
     ("s" (lambda (dir)
            (let ((projectile-switch-project-action 'projectile-save-project-buffers))
              (projectile-switch-project-by-name dir arg)))
      "save all buffers")
     ("k" (lambda (dir)
            (let ((projectile-switch-project-action 'projectile-kill-buffers))
              (projectile-switch-project-by-name dir arg)))
      "kill all buffers")
     ("r" (lambda (dir)
            (let ((projectile-switch-project-action
                   'projectile-remove-current-project-from-known-projects))
              (projectile-switch-project-by-name dir arg)))
      "remove from known projects")
     ("g" (lambda (dir)
            (let ((projectile-switch-project-action 'projectile-vc))
              (projectile-switch-project-by-name dir arg)))
      "open in magit")
     ("t" (lambda (dir)
            (let ((projectile-switch-project-action (lambda () (projectile-run-term "/usr/local/bin/fish"))))
              (projectile-switch-project-by-name dir arg)))
      "start term for project")
     ("a" (lambda (dir)
            (let ((projectile-switch-project-action (lambda () (counsel-projectile-ag))))
              (projectile-switch-project-by-name dir arg)))
      "run ag in project"))))

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
                          ".*el.gz$" "/\\.get/.*\\'" "/elpa/.*\\"))
  (setq recentf-max-saved-items 200
        recentf-auto-cleanup 300))

(use-package restclient :defer t)

(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
  :general (nmap "gof" 'reveal-in-osx-finder))

(use-package savehist :ensure nil :config (savehist-mode))

(use-package saveplace
  :ensure nil
  :if (version< "25" emacs-version)
  :config (save-place-mode))

(use-package sentence-navigation
  :defer t
  :general
  (mmap ")" 'sentence-nav-evil-forward
        "(" 'sentence-nav-evil-backward)
  (nmap "g)" 'sentence-nav-evil-forward-end
        "g(" 'sentence-nav-evil-backward-end)
  (otomap "s" 'sentence-nav-evil-a-sentence)
  (itomap "s" 'sentence-nav-evil-inner-sentence))

(use-package semantic
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (semantic-mode -1)))
  (add-hook 'lisp-interaction-mode-hook (lambda () (semantic-mode -1))))

(use-package shell-pop
  :general ("s-`" 'shell-pop)
  :init
  (setq shell-pop-window-position 'bottom
        shell-pop-window-height 30
        shell-pop-full-span t
        shell-pop-shell-type '("terminal" "*terminal*"
                               (lambda () (term explicit-shell-file-name)))))

(use-package term
  :general
  (:keymaps 'term-raw-map "s-v" 'term-paste)
  (nmap :keymaps 'term-raw-map "p" 'term-paste)
  :config
  (setq term-suppress-hard-newline nil
        term-scroll-to-bottom-on-output t)
  (add-hook 'term-mode-hook (lambda () (toggle-truncate-lines 1))))

(use-package typo
  :disabled t
  :init
  (typo-global-mode 1)
  (setq typo-language 'English)
  (add-hook 'text-mode-hook 'typo-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :general ("s-Z" 'undo-tree-redo
            "s-z" 'undo-tree-undo)
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t
        ;; undo-tree-visualizer-diff t
        undo-tree-auto-save-history t))

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
  (nmap "C-w u" 'winner-undo)
  (nmap :prefix "SPC" "wu" 'winner-undo)
  :init
  (winner-mode t)
  (setq winner-boring-buffers
        (append winner-boring-buffers '("*Compile-Log*" "*inferior-lisp*"
                                        "*Apropos*" "*cvs*" "*Buffer List*"
                                        "*Ibuffer*"))))

(use-package shackle
  :config
  (defun shackle-smart-align ()
    (if (< (window-width) 160)
        'below
      'right))
  (setq shackle-rules '((compilation-mode :noselect t)
                        (help-mode :align shackle-smart-align :size 0.42)
                        (undo-tree-visualizer-mode :align t :size 0.3)
                        (woman-mode :popup t)
                        (flycheck-error-list-mode :select t)
                        (cargo-process-mode :align t :size 0.3)
                        (ivy-occur-mode :select t))
        shackle-select-reused-windows t)
  (shackle-mode 1))

;;** Basic Editing
(csetq fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
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
  :config
  (evil-set-initial-state 'messages-buffer-mode 'insert)
  (column-number-mode 1))

(use-package ws-butler
  :diminish ws-butler-mode
  :init (ws-butler-global-mode))

(bind-key [remap just-one-space] #'cycle-spacing)

(use-package expand-region
  :general
  ("M-2" 'soo-er-and-insert)
  (nmap :prefix "SPC"
    "v" 'er/expand-region)
  :config
  (defun soo-er-and-insert (arg)
    (interactive "p")
    (progn (evil-insert 1)
           (er/expand-region arg))))

(use-package multiple-cursors
  :commands (mc/mark-next-like-this mc/mark-all-dwim)
  :init
  (evil-define-key 'visual global-map "m" 'mc/mark-all-like-this-dwim)
  ;; Malabarba keybinds http://endlessparentheses.com/multiple-cursors-keybinds.html
  (define-key ctl-x-map "\C-m" #'mc/mark-all-dwim)
  (define-key ctl-x-map (kbd "<return>") mule-keymap)
  (global-set-key (kbd "M-3") #'mc/mark-next-like-this)
  (global-set-key (kbd "M-4") #'mc/mark-previous-like-this)
  (global-set-key (kbd "M-#") #'mc/unmark-next-like-this)
  (global-set-key (kbd "M-$") #'mc/unmark-previous-like-this)
  (define-prefix-command 'endless/mc-map)
  ;; C-x m is usually `compose-mail'. Bind it to something
  ;; else if you use this command.
  (define-key ctl-x-map "m" 'endless/mc-map)
  ;; Really really nice!
  (define-key endless/mc-map "i" #'mc/insert-numbers)
  (define-key endless/mc-map "h" #'mc-hide-unmatched-lines-mode)
  (define-key endless/mc-map "a" #'mc/mark-all-like-this)
  ;; Occasionally useful
  (define-key endless/mc-map "d" #'mc/mark-all-symbols-like-this-in-defun)
  (define-key endless/mc-map "r" #'mc/reverse-regions)
  (define-key endless/mc-map "s" #'mc/sort-regions)
  (define-key endless/mc-map "l" #'mc/edit-lines)
  (define-key endless/mc-map "\C-a" #'mc/edit-beginnings-of-lines)
  (define-key endless/mc-map "\C-e" #'mc/edit-ends-of-lines))

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
   [tab] 'company-complete-common)
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil
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

;;** autoloads
(load (expand-file-name "auto.el" lisp-d) nil t)
(load (expand-file-name "loaddefs.el" lisp-d) nil t)

;;** keybinds
(load (expand-file-name "keybinds.el" lisp-d) nil t)

(require 'server)
(or (server-running-p) (server-start))

;;; init.el ends here
