;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

(defvar before-user-init-time (current-time)
  "Value of `current-time' when Emacs begins loading `user-init-file'.")
(message "Loading Emacs...done (%.3fs)"
         (float-time (time-subtract before-user-init-time before-init-time)))
(setq user-init-file (or load-file-name buffer-file-name))
(setq emacs-d (file-name-directory user-init-file))
(setq package-user-dir (expand-file-name "elpa" emacs-d))
(package-initialize)

(message "Loading %s..." user-init-file)

;; Theme
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'zenburn t)
;; Font
(ignore-errors (set-frame-font "Input Mono Narrow"))
;; Customize
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))
(csetq tool-bar-mode nil)
(csetq scroll-bar-mode nil)
(csetq menu-bar-mode nil)
(csetq inhibit-startup-screen t)
(csetq initial-scratch-message "")
(csetq load-prefer-newer t)
(csetq create-lockfiles nil) ; Don't create #foo.file#
(csetq fill-column 80)
(eval '(setq inhibit-startup-echo-area-message "sooheon"))
;; Navigation within buffer
(csetq recenter-positions '(top middle bottom))
;; Finding files
(csetq vc-follow-symlinks t)
(csetq find-file-suppress-same-file-warnings t)
(csetq read-buffer-completion-ignore-case t)
;; Editor behavior
(csetq default-input-method "korean-hangul")
(csetq indent-tabs-mode nil)
(csetq ring-bell-function 'ignore)
(csetq highlight-nonselected-windows t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions nil)
(add-hook 'server-switch-hook 'raise-frame)
(csetq eval-expression-print-length nil)
(csetq eval-expression-print-level nil)
;; Shell
(csetq shell-file-name "/usr/local/bin/bash")
(csetq explicit-shell-file-name "/usr/local/bin/fish")
;; Internals
(csetq gc-cons-threshold (* 10 1024 1024))
(csetq ad-redefinition-action 'accept)
;; Set PATHs--see: http://tinyurl.com/ctf9h3a
(setenv "MANPATH" "/usr/local/opt/coreutils/libexec/gnuman:/usr/local/opt/findutils/libexec/gnuman:/usr/local/share/man")
(setenv "PATH" "/usr/local/Cellar/pyenv-virtualenv/20160315/shims:/Users/sooheon/.pyenv/shims:/usr/local/opt/coreutils/libexec/gnubin:/usr/local/opt/findutils/libexec/gnubin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/Library/TeX/texbin")
(setq exec-path '("/usr/local/Cellar/pyenv-virtualenv/20160315/shims" "/Users/sooheon/.pyenv/shims" "/usr/local/opt/coreutils/libexec/gnubin" "/usr/local/opt/findutils/libexec/gnubin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/opt/X11/bin" "/Library/TeX/texbin" "/usr/local/Cellar/emacs/HEAD/libexec/emacs/25.1.50/x86_64-apple-darwin15.5.0"))

;; borg
(add-to-list 'load-path (expand-file-name "lib/borg" emacs-d))
(require 'borg)
(borg-initialize)

;; use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(setq use-package-verbose t)

(use-package no-littering :demand t)

(use-package evil
  :init
  (setq-default evil-want-C-u-scroll t
                evil-want-fine-undo nil
                evil-cross-lines t
                evil-symbol-word-search t
                ;; evil-move-cursor-back nil
                evil-want-C-i-jump t
                evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map "\C-w" 'evil-delete-backward-word))

(use-package evil-leader
  :init
  (evil-leader/set-key
    "TAB" 'evil-buffer
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
    "wv" 'evil-window-vsplit
    "wr" 'evil-window-rotate-downwards
    "wR" 'evil-window-rotate-upwards)
  :config
  (global-evil-leader-mode))

(use-package evil-evilified-state
  :load-path "~/.emacs.d/lib/evil-evilified-state"
  :config
  (use-package bind-map :defer t)
  (define-key evil-evilified-state-map " " evil-leader--default-map))

(use-package auto-compile
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest t)
  (setq auto-compile-update-autoloads t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

(use-package epkg
  :defer t
  :commands epkg-describe-package
  :init (setq epkg-repository (expand-file-name "var/epkgs/" emacs-d))
  (evil-leader/set-key "aep" 'epkg-describe-package))

(use-package package
  :config
  (setq package-archives '(("melpa" . "http://melpa.org/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")))
  (evil-leader/set-key "ak" 'package-list-packages))

(use-package custom
  :config
  (setq custom-file (expand-file-name "custom.el" emacs-d))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server :config (or (server-running-p) (server-mode)))

(progn ; startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Long tail

(defvar sooheon--avy-keys '(?w ?e ?r ?s ?d ?x ?c ?u ?i ?o ?v ?n ?m ?l ?k ?j ?f))

(use-package avy
  :defer t
  :commands spacemacs/avy-open-url
  :bind (("s-g" . evil-avy-goto-word-1)
         ([remap goto-line] . evil-avy-goto-line))
  :init
  (evil-leader/set-key
    "xo" 'spacemacs/avy-open-url)
  :config
  (setq avy-keys sooheon--avy-keys)
  (progn
    (defun spacemacs/avy-goto-url ()
      "Use avy to go to an URL in the buffer."
      (interactive)
      (avy--generic-jump "https?://" nil 'pre))
    (defun spacemacs/avy-open-url ()
      "Use avy to select an URL in the buffer and open it."
      (interactive)
      (save-excursion
        (spacemacs/avy-goto-url)
        (browse-url-at-point)))))

(use-package ace-link
  :commands (ace-link-info ace-link-eww ace-link-help)
  :init
  (ace-link-setup-default)
  (when (boundp 'org-mode-map)
    (define-key org-mode-map "\M-o" 'ace-link-org)))

(use-package autorevert
  :diminish auto-revert-mode
  :init (global-auto-revert-mode 1))

(use-package company
  :defer 2
  :diminish (company-mode . "co")
  :config
  (setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-if-just-one-frontend))
  (setq company-backends '(company-elisp
                           company-css
                           ;; company-semantic
                           company-capf
                           (company-dabbrev-code
                            company-gtags
                            company-etags
                            company-keywords)
                           company-files
                           company-dabbrev))
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2)
  (global-company-mode)
  (let ((m company-active-map))
    (define-key m [escape] (lambda () (interactive)
                             (company-abort)
                             (evil-normal-state)))
    (define-key m "\C-n" 'company-select-next)
    (define-key m "\C-p" 'company-select-previous)
    (define-key m [tab] 'company-complete-common)))

(use-package cider
  :defer t
  :init
  (add-hook 'clojure-mode-hook 'cider-mode)
  :config
  (setq cider-repl-pop-to-buffer-on-connect nil
        cider-prompt-save-file-on-load nil
        cider-repl-use-clojure-font-lock t
        cider-mode-line '(:eval (format " [%s]" (cider--modeline-info))))
  ;; (defadvice cider-jump-to-var (before add-evil-jump activate)
  ;;   (evil-set-jump))
  (add-hook 'cider-mode-hook 'eldoc-mode))

(use-package clojure-mode
  :defer t
  :mode ("\\.boot\\'" . clojure-mode)
  :interpreter ("!.*boot\\s-*" . clojure-mode))

(use-package dash :config (dash-enable-font-lock))

(use-package diff-hl
  :defer 5
  :after magit
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package dired
  :commands dired-jump
  :init
  (delete ".elc" completion-ignored-extensions)
  (define-key evil-normal-state-map "-" 'dired-jump)
  (defun soo--dired-setup ()
    (setq dired-omit-verbose nil)
    (setq dired-hide-details-hide-symlink-targets nil)
    (dired-hide-details-mode t)
    (dired-omit-mode t))
  (add-hook 'dired-mode-hook 'soo--dired-setup)
  :config
  (setq dired-listing-switches "-alh")
  (defvar dired-dotfiles-show-p)
  (defun vinegar/dotfiles-toggle ()
    "Show/hide dot-files"
    (interactive)
    (when (equal major-mode 'dired-mode)
      (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
          (progn
            (set (make-local-variable 'dired-dotfiles-show-p) nil)
            (message "h")
            (dired-mark-files-regexp "^\\\.")
            (dired-do-kill-lines))
        (progn (revert-buffer)          ; otherwise just revert to re-show
               (set (make-local-variable 'dired-dotfiles-show-p) t)))))
  (defun vinegar/dired-diff ()
    "Ediff marked files in dired or selected files in separate window"
    (interactive)
    (let* ((marked-files (dired-get-marked-files nil nil))
           (other-win (get-window-with-predicate
                       (lambda (window)
                         (with-current-buffer (window-buffer window)
                           (and (not (eq window (selected-window)))
                                (eq major-mode 'dired-mode))))))
           (other-marked-files (and other-win
                                    (with-current-buffer (window-buffer other-win)
                                      (dired-get-marked-files nil)))))
      (cond ((= (length marked-files) 2)
             (ediff-files (nth 0 marked-files)
                          (nth 1 marked-files)))
            ((= (length marked-files) 3)
             (ediff-files3 (nth 0 marked-files)
                           (nth 1 marked-files)
                           (nth 2 marked-files)))
            ((and (= (length marked-files) 1)
                  (= (length other-marked-files) 1))
             (ediff-files (nth 0 marked-files)
                          (nth 0 other-marked-files)))
            ((= (length marked-files) 1)
             (call-interactively 'dired-diff))
            (t (error "Mark exactly 2 files, at least 1 locally")))))
  (evilified-state-evilify dired-mode dired-mode-map
    "j" 'dired-next-line
    "k" 'dired-previous-line
    "-" 'dired-jump
    "0" 'dired-back-to-start-of-files
    "=" 'vinegar/dired-diff
    (kbd "C-j") 'dired-next-subdir
    (kbd "C-k") 'dired-prev-subdir
    "I" 'vinegar/dotfiles-toggle
    (kbd "~") '(lambda () (interactive) (find-alternate-file "~/"))
    (kbd "RET") 'dired-find-file
    "f" 'counsel-find-file
    "J" 'dired-goto-file
    (kbd "C-f") 'find-name-dired
    "H" 'diredp-dired-recent-dirs
    "T" 'dired-tree-down
    "K" 'dired-do-kill-lines
    "r" 'revert-buffer
    (kbd "C-r") 'dired-do-redisplay
    "gg" '(lambda () (interactive) (beginning-of-buffer) (dired-next-line 1))
    "gs" 'magit-status
    "G" '(lambda () (interactive) (end-of-buffer) (dired-next-line -1)))
  ;; Use rsync in dired: http://oremacs.com/2016/02/24/dired-rsync/
  (defun ora-dired-rsync (dest)
    (interactive (list (expand-file-name
                        (read-file-name
                         "Rsync to:"
                         (dired-dwim-target-directory)))))
    (let ((files (dired-get-marked-files nil current-prefix-arg))
          (tmtxt/rsync-command "rsync -arvz --progress "))
      ;; Add all selected file names as arguments to the rsync command
      (dolist (file files)
        (setq tmtxt/rsync-command
              (concat tmtxt/rsync-command
                      (shell-quote-argument file)
                      " ")))
      ;; Append the destination
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument dest)))
      ;; Run the async shell command
      (async-shell-command tmtxt/rsync-command "*rsync*")
      ;; Finally, switch to that window
      (other-window 1)))
  (define-key dired-mode-map "Y" 'ora-dired-rsync))

(use-package dired+ :after dired :diminish dired-omit-mode)

(use-package ediff
  :defer t
  :commands (ediff-buffers ediff)
  :config
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally
                ediff-merge-split-window-function 'split-window-horizontally)
  (add-hook 'ediff-quit-hook #'winner-undo))

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :config
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :commands elisp-slime-nav-describe-elisp-thing-at-point
  :init
  (evil-define-key 'normal emacs-lisp-mode-map
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point)
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :commands (evil-commentary
             evil-commentary-yank
             evil-commentary-line)
  :init
  (evil-define-key 'normal global-map "gc" 'evil-commentary)
  (evil-define-key 'normal global-map "gy" 'evil-commentary-yank)
  (define-key global-map (kbd "s-/") 'evil-commentary-line)
  :config
  (evil-commentary-mode))

(use-package evil-matchit
  :commands (evilmi-jump-items)
  :init
  (evil-define-key 'normal global-map "%" 'evilmi-jump-items)
  (evil-define-key 'visual global-map "%" 'evilmi-jump-items)
  :config
  (global-evil-matchit-mode))

(use-package evil-multiedit
  :commands (evil-multiedit-match-symbol-and-next
             evil-multiedit-match-symbol-and-prev
             evil-multiedit-match-and-prev
             evil-multiedit-match-and-next
             evil-multiedit-prev
             evil-multiedit-next)
  :init
  (let ((map evil-normal-state-map))
    (define-key map (kbd "s-d") 'evil-multiedit-match-symbol-and-next)
    (define-key map (kbd "s-D") 'evil-multiedit-match-symbol-and-prev))
  (let ((map evil-visual-state-map))
    (define-key map (kbd "s-d") 'evil-multiedit-match-and-next)
    (define-key map (kbd "s-D") 'evil-multiedit-match-and-prev)
    (define-key map (kbd "C-s-D") 'evil-multiedit-restore)
    (define-key map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
    (define-key map "R" 'evil-multiedit-match-all))
  :config
  (let ((map evil-multiedit-state-map))
    (define-key map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
    (define-key map (kbd "C-n") 'evil-multiedit-next)
    (define-key map (kbd "C-p") 'evil-multiedit-prev))
  (let ((map evil-multiedit-insert-state-map))
    (define-key map (kbd "C-n") 'evil-multiedit-next)
    (define-key map (kbd "C-p") 'evil-multiedit-prev)))

(use-package evil-textobj-anyblock
  :config
  (define-key evil-inner-text-objects-map
    "b" 'evil-textobj-anyblock-inner-block)
  (define-key evil-outer-text-objects-map
    "b" 'evil-textobj-anyblock-a-block))

(use-package evil-visualstar
  :commands (evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (define-key evil-visual-state-map "*" 'evil-visualstar/begin-search-forward)
  (define-key evil-visual-state-map "#" 'evil-visualstar/begin-search-backward))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "gs" 'evil-Surround-region)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute))

(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :config
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'buffer
        evil-snipe-show-prompt nil
        evil-snipe-smart-case t)
  (evil-snipe-override-mode 1))

(use-package evil-numbers
  :bind (:map evil-normal-state-map
              ("C-S-a" . evil-numbers/inc-at-pt)
              ("C-S-x" . evil-numbers/dec-at-pt)))

(use-package flycheck
  :defer t
  :config
  (setq flycheck-standard-error-navigation nil
        flycheck-global-modes nil))

(use-package flyspell
  :disabled t
  :defer t
  :diminish flyspell-mode
  :init
  (add-hook 'text-mode-hook 'flyspell-mode))

(use-package flyspell-correct
  :disabled t
  :defer t
  :bind (("C-;" . flyspell-correct-word-generic))
  :config
  (setq flyspell-correct-interface 'flyspell-correct-ivy))

(use-package flx :after ivy)

(use-package help :config (setq help-window-select t))

(use-package highlight-escape-sequences
  :defer
  :init
  (add-hook 'prog-mode-hook 'hes-mode))

(use-package info :config (evil-leader/set-key "hi" 'info))

(use-package counsel
  :bind (([remap execute-extended-command] . counsel-M-x)
         ([remap describe-function] . counsel-describe-function)
         ([remap describe-variable] . counsel-describe-variable)
         ([remap describe-bindings] . counsel-descbinds)
         ([remap find-file] . counsel-find-file)
         ([remap imenu] . counsel-imenu)
         ([remap load-library] . counsel-load-library)
         ([remap yank-pop] . counsel-yank-pop)
         ([remap info-lookup-symbol] . counsel-info-lookup-symbol)
         ([remap menu-bar-open] . counsel-tmm)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-x l" . counsel-locate))
  :init
  (evil-leader/set-key
    "hv" 'counsel-describe-variable
    "hf" 'counsel-describe-function
    "f" 'counsel-find-file
    "Th" 'counsel-load-theme
    "ap" 'counsel-list-processes)
  (define-key evil-normal-state-map "\M-y" 'counsel-yank-pop))

(use-package swiper
  :bind (([remap isearch-forward] . counsel-grep-or-swiper)))

(use-package ivy
  :diminish ivy-mode
  :commands (magit-status epkg-describe-package)
  :bind (("s-b" . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume))
  :init
  (evil-leader/set-key
    "r" 'ivy-recentf
    "b" 'ivy-switch-buffer)
  :config
  (with-eval-after-load 'recentf (setq ivy-use-virtual-buffers t))
  (setq ivy-extra-directories '("./")
        ivy-count-format "%d "
        ivy-height 12
        ;; ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ;; ivy-initial-inputs-alist nil
        ivy-action-wrap t)
  (ivy-mode 1)
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
  :defer t
  :init
  (add-hook 'smartparens-enabled-hook
            (lambda () (when (member major-mode sp-lisp-modes) (lispy-mode))))
  (add-hook 'smartparens-disabled-hook
            (lambda () (when (member major-mode sp-lisp-modes) (lispy-mode -1))))
  :config
  (setq lispy-compat '(edebug cider)
        lispy-avy-keys sooheon--avy-keys
        lispy-avy-style-paren 'at-full
        lispy-avy-style-symbol 'at-full
        lispy-delete-backward-recenter nil
        lispy-safe-paste t)
  (lispy-set-key-theme '(special
                         c-digits
                         paredit))
  (dolist (map (list lispy-mode-map-paredit lispy-mode-map-parinfer))
    (define-key map (kbd "C-a") nil)
    (define-key map "\M-j" 'lispy-split)
    (define-key map "\M-k" 'lispy-kill-sentence)
    (define-key map [M-up] 'sp-splice-sexp-killing-backward)
    (define-key map [M-down] 'sp-splice-sexp-killing-forward)
    (define-key map (kbd "C-,") 'lispy-kill-at-point))
  (let ((map lispy-mode-map-paredit))
    (define-key map "\M-n" nil)         ; lispy left
    (define-key map "\M-p" nil)
    ;; (define-key map "\"" nil)           ; lispy-quotes
    (define-key map "\C-d" 'lispy-delete)
    (define-key map (kbd "M-)") nil)
    (define-key map (kbd "DEL") 'lispy-delete-backward))
  ;; (let ((map lispy-mode-map-parinfer))
  ;;   (define-key map (kbd "\"") nil)
  ;;   (define-key map (kbd "M-r") 'lispy-raise)
  ;;   (define-key map (kbd "#") nil)
  ;;   (define-key map (kbd ":") nil))
  (lispy-define-key lispy-mode-map-special ">" 'lispy-slurp-or-barf-right)
  (lispy-define-key lispy-mode-map-special "<" 'lispy-slurp-or-barf-left)

  ;; Unbind M-k and M-. in evil normal state and use lispy
  (define-key evil-normal-state-map "\M-." nil) ; evil-repeat-pop-next
  (define-key evil-normal-state-map "\M-k" nil))

(use-package lispyville
  :diminish lispyville-mode
  :commands (lispyville-delete
             lispyville-delete-char-or-splice
             lispyville-drag-forward
             lispyville-drag-backward)
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

(use-package lpy
  :disabled t
  :defer t
  :init
  (use-package soap :defer t)
  (add-hook 'python-mode-hook 'lpy-mode))

(use-package anaconda-mode
  :defer t
  :diminish anaconda-mode
  :init
  (setq anaconda-mode-installation-directory (expand-file-name "etc/anaconda-mode" emacs-d))
  (add-hook 'python-mode-hook 'anaconda-mode)
  :config
  (evil-define-key 'normal anaconda-mode-map "K" 'anaconda-mode-show-doc)
  (evilified-state-evilify anaconda-mode-view-mode anaconda-mode-view-mode-map
    (kbd "q") 'quit-window)
  (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
    (evil--jumps-push))
  (require 'company-anaconda)
  (add-to-list 'company-backends 'company-anaconda))

(use-package py-yapf
  :commands py-yapf-buffer
  :init
  (evil-leader/set-key-for-mode 'python-mode "=" 'py-yapf-buffer))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :init
  (define-key evil-normal-state-map "gs" 'magit-status)
  (define-key evil-normal-state-map "gp" 'magit-dispatch-popup)
  (evil-leader/set-key "g" 'magit-status "G" 'magit-dispatch-popup)
  :config
  (setq magit-refresh-verbose t
        magit-refresh-status-buffer nil)
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
                          'magit-insert-unpulled-from-upstream)

  (use-package evil-magit
    :config
    (evil-define-key 'normal magit-status-mode-map
      "n" 'magit-section-forward
      "p" 'magit-section-backward
      "\C-n" 'next-line
      "\C-p" 'previous-line)))

(use-package markdown-mode
    :mode ("\\.m[k]d" . markdown-mode)
    :defer t
    :config
    ;; Insert key for org-mode and markdown a la C-h k
    ;; from SE endless http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode/2208#2208
    (defun spacemacs/insert-keybinding-markdown (key)
      "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
      (interactive "kType key sequence: ")
      (let* ((tag "~%s~"))
        (if (null (equal key "\r"))
            (insert
             (format tag (help-key-description key nil)))
          (insert (format tag ""))
          (forward-char -6))))

    ;; Header navigation in normal state movements
    (evil-define-key 'normal markdown-mode-map
      "gj" 'outline-forward-same-level
      "gk" 'outline-backward-same-level
      "gh" 'outline-up-heading
      ;; next visible heading is not exactly what we want but close enough
      "gl" 'outline-next-visible-heading)

    ;; Promotion, Demotion
    ;; (define-key markdown-mode-map "\M-h" 'markdown-promote)
    ;; (define-key markdown-mode-map "\M-j" 'markdown-move-down)
    ;; (define-key markdown-mode-map "\M-k" 'markdown-move-up)
    ;; (define-key markdown-mode-map "\M-l" 'markdown-demote)
    )

(use-package org
  :defer 10
  :config
  (setq org-export-backends '(html latex))
  (add-to-list 'load-path (expand-file-name "lib/org/contrib/lisp/" emacs-d))
  (require 'org-download)
  (org-download-enable)
  (require 'org-bullets)
  (org-bullets-mode)
  (require 'ox)
  (setq org-src-fontify-natively t
        org-startup-indented t
        org-adapt-indentation nil
        org-preview-latex-default-process 'dvipng
        org-inhibit-startup-visibility-stuff nil)
  (fset 'latexify-line
        (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([95 105 36 escape 65 36 escape] 0 "%d")) arg)))
  ;; Keybinds
  (evil-define-key 'normal org-mode-map
    [C-return] (lambda () (interactive) (org-insert-heading-respect-content) (evil-append 1))
    [M-return] (lambda () (interactive) (org-meta-return) (evil-append 1))
    [return] 'org-open-at-point
    "t" 'org-todo
    "$" 'org-end-of-line
    "^" 'org-beginning-of-line
    ;; "-" 'org-ctrl-c-minus
    "<" 'org-metaleft
    ">" 'org-metaright
    "\M-n" 'org-metadown
    "\M-p" 'org-metaup)
  (evil-define-key 'insert org-mode-map "\C-j" 'org-return)
  ;; Org Babel
  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        geiser-default-implementation 'guile
        org-babel-load-languages '((python . t)
                                   (clojure . t))
        org-babel-default-header-args '((:session . "none")
                                        (:results . "replace")
                                        (:exports . "code")
                                        (:cache . "no")
                                        (:noweb . "no")
                                        (:hlines . "no")
                                        (:tangle . "yes")))
  ;; Hydra
  (defhydra hydra-org-template (:color blue :hint nil)
    "
_C_enter  _q_uote    _c_lojure     _L_aTeX:
_l_atex   _e_xample  _s_cheme      _i_ndex:
_a_scii   _v_erse    _E_macs-lisp  _I_NCLUDE:
s_r_c     ^ ^        _p_ython      _H_TML:
_h_tml    ^ ^        ^ ^           _A_SCII:
"
    ("c" (hot-expand-and-edit "clojure"))
    ("s" (hot-expand-and-edit "scheme"))
    ("E" (hot-expand-and-edit "emacs-lisp"))
    ("p" (hot-expand-and-edit "python"))
    ("r" (hot-expand "<s"))
    ("e" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("C" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("o" nil "quit"))
  (defun hot-expand (str)
    "Expand org template."
    (insert str)
    (org-try-structure-completion))
  (defun hot-expand-and-edit (str)
    "Expand src template for given languange and enter org-edit-special."
    (hot-expand "<s")
    (insert str)
    (forward-line)
    (evil-normal-state)
    (org-edit-special)
    (evil-insert-state))
  (define-key org-mode-map "<" (lambda () (interactive)
                                 (if (bolp)
                                     (hydra-org-template/body)
                                   (self-insert-command 1)))))

(use-package worf
  :diminish worf-mode
  :after org
  :init
  (add-hook 'org-mode-hook 'worf-mode)
  :config
  (evil-define-key 'insert worf-mode-map
    "\C-j" nil
    "\[" nil
    "\]" nil))

(use-package shackle
  :config
  (shackle-mode 1)
  (setq shackle-rules '((compilation-mode :noselect t)
                        (help-mode :noselect t)
                        ("*undo-tree*" :size 0.3)
                        (woman-mode :popup t))))

(use-package popwin
  :disabled t
  :config
  (popwin-mode 1)
  (setq popwin:special-display-config nil)
  (evil-leader/set-key
    "wpm" 'popwin:messages
    "wpp" 'popwin:close-popup-window)
  (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect t :height 0.4) popwin:special-display-config)
  (push '("*compilation*"          :dedicated t :position bottom :stick t :noselect t :height 0.4) popwin:special-display-config)
  (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil) popwin:special-display-config)
  (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil) popwin:special-display-config)
  (push '(" *undo-tree*"           :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
  (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil) popwin:special-display-config)
  (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil) popwin:special-display-config)
  (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil) popwin:special-display-config)
  (push '("^\*WoMan.+\*$"          :regexp t :position bottom) popwin:special-display-config))

(use-package projectile
  :diminish projectile-mode
  :defer 6
  :commands (projectile-switch-project
             projectile-find-file
             projectile-find-dir
             projectile-dired
             projectile-recentf
             counsel-projectile-ag)
  :bind (("C-c k" . counsel-projectile-ag))
  :init
  (evil-leader/set-key
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
  (defun counsel-projectile-ag ()
    (interactive)
    (counsel-ag nil (or (ignore-errors (projectile-project-root))
                        default-directory)))
  (setq projectile-enable-caching t
        projectile-sort-order 'recentf
        projectile-create-missing-test-files t
        projectile-completion-system 'ivy))

(use-package counsel-projectile
  :after projectile
  :init
  (evil-leader/set-key
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

(use-package rainbow-delimiters
  :init
  (add-hook 'smartparens-mode-hook #'rainbow-delimiters-mode))

(use-package recentf
  :demand t
  :init
  (setq recentf-max-saved-items 1000
        recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1)
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
  :commands reveal-in-osx-finder
  :init (define-key evil-normal-state-map "gof" 'reveal-in-osx-finder))

(use-package savehist :config (savehist-mode))

(use-package saveplace :config (save-place-mode))

(use-package shell-pop
  :bind (("s-`" . shell-pop))
  :init
  (define-key evil-normal-state-map "got" 'shell-pop)
  (setq shell-pop-window-position 'bottom
        shell-pop-window-height 30
        shell-pop-full-span t
        shell-pop-shell-type '("terminal" "*terminal*"
                               (lambda () (term explicit-shell-file-name))))
  :config
  (define-key term-raw-map (kbd "s-v") 'term-paste)
  (evil-define-key 'normal term-raw-map "p" 'term-paste))

(use-package smartparens
  :defer t
  :init
  (setq sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-from-inside nil
        sp-show-pair-delay 0
        sp-autoskip-closing-pair 't)
  (defun conditionally-enable-smartparens-mode ()
    "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
    (if (eq this-command 'eval-expression)
        (smartparens-mode)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode 1)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (let ((m smartparens-mode-map))
    (define-key m (kbd "M-a") 'sp-beginning-of-sexp)
    (define-key m (kbd "M-e") 'sp-end-of-sexp)
    (define-key m [C-backspace] 'sp-backward-kill-sexp)
    (define-key m (kbd "C-)") 'sp-forward-slurp-sexp)
    (define-key m (kbd "C-(") 'sp-backward-slurp-sexp)
    (define-key m (kbd "C-{") 'sp-backward-barf-sexp)
    (define-key m (kbd "C-}") 'sp-forward-barf-sexp)))

(use-package smex :defer t :config (setq smex-history-length 32))

(use-package simple :config (column-number-mode))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(use-package undo-tree
  :diminish undo-tree-mode
  :bind (("s-Z" . undo-tree-redo)
         ("s-z" . undo-tree-undo))
  :commands (undo-tree-undo)
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t))

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package window-numbering
  :bind (("s-0" . select-window-0)
         ("s-1" . select-window-1)
         ("s-2" . select-window-2)
         ("s-3" . select-window-3)
         ("s-4" . select-window-4)
         ("s-5" . select-window-5)
         ("s-6" . select-window-6)
         ("M-0" . nil)
         ("M-1" . nil)
         ("M-2" . nil)
         ("M-3" . nil)
         ("M-4" . nil)
         ("M-5" . nil)
         ("M-6" . nil)
         ("M-7" . nil)
         ("M-8" . nil)
         ("M-9" . nil))
  :config
  (window-numbering-mode 1))

(use-package winner
  :init
  (evil-leader/set-key "wu" 'winner-undo)
  (winner-mode t)
  (setq winner-boring-buffers
        (append winner-boring-buffers '("*Compile-Log*" "*inferior-lisp*"
                                        "*Apropos*" "*cvs*" "*Buffer List*"
                                        "*Ibuffer*"))))

(use-package ws-butler
  :diminish ws-butler-mode
  :config (ws-butler-global-mode))

(progn ;; Personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                emacs-d)))
    (when (file-exists-p file)
      (load file))))

(progn ;; Startup
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
