;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds

(progn ;; Startup
  (setq gc-cons-threshold most-positive-fixnum)
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil
        inhibit-startup-buffer-menu t
        inhibit-startup-screen t
        initial-buffer-choice t
        initial-major-mode 'fundamental-mode
        initial-scratch-message ""
        load-prefer-newer t
        create-lockfiles nil       ; Don't create #foo.file#
        vc-handled-backends nil    ; vc may slow down opening of files
        fill-column 81)
  (eval '(setq inhibit-startup-echo-area-message "sooheon"))
  (fset 'yes-or-no-p 'y-or-n-p)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  ;; Set paths--see: http://tinyurl.com/ctf9h3a
  (setenv "MANPATH" "/usr/local/opt/coreutils/libexec/gnuman:/usr/local/opt/findutils/libexec/gnuman:/usr/local/share/man")
  (setenv "PATH" "/usr/local/Cellar/pyenv-virtualenv/20160315/shims:/Users/sooheon/.pyenv/shims:/usr/local/opt/coreutils/libexec/gnubin:/usr/local/opt/findutils/libexec/gnubin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/Library/TeX/texbin")
  (setq exec-path '("/usr/local/Cellar/pyenv-virtualenv/20160315/shims" "/Users/sooheon/.pyenv/shims" "/usr/local/opt/coreutils/libexec/gnubin" "/usr/local/opt/findutils/libexec/gnubin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/opt/X11/bin" "/Library/TeX/texbin" "/usr/local/Cellar/emacs/HEAD/libexec/emacs/25.1.50/x86_64-apple-darwin15.5.0")))

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

(use-package no-littering :demand t)

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
  :defer 4
  :init
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
  (defun spacemacs/alternate-buffer ()
    "Switch back and forth between current and last buffer in the
current window."
    (interactive)
    (if (evil-alternate-buffer)
        (switch-to-buffer (car (evil-alternate-buffer)))
      (switch-to-buffer (other-buffer (current-buffer) t))))
  (global-evil-leader-mode))

(use-package bind-map :defer t)

(use-package evil-evilified-state
  :load-path "~/.emacs.d/lib/evil-evilified-state"
  :config (define-key evil-evilified-state-map " " spacemacs-default-map))

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
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory))
  (evil-leader/set-key "ak" 'epkg-describe-package))

(use-package custom
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
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
  :defer
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
  (with-eval-after-load 'org
    (define-key org-mode-map "\M-o" 'ace-link-org)))

(use-package autorevert :diminish auto-revert-mode :defer t)

(use-package company
  :diminish (company-mode . "co")
  :defer 3
  :config
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

(use-package dash :config (dash-enable-font-lock))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package dired
  :commands dired-jump
  :diminish dired-omit-mode
  :init
  (define-key evil-normal-state-map "-" 'dired-jump)
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-omit-mode t)
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
        (progn (revert-buffer)      ; otherwise just revert to re-show
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
             (dired-diff))
            (t (error "mark exactly 2 files, at least 1 locally")))))
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
    "gg" '(lambda () (interactive) (beginning-of-buffer) (dired-next-line 2))
    "G" '(lambda () (interactive) (end-of-buffer) (dired-next-line -1))))

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
  :init (evil-commentary-mode))

(use-package evil-matchit :config (global-evil-matchit-mode))

(use-package evil-visualstar :config (global-evil-visualstar-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "gs" 'evil-Surround-region)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute))

(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :config
  (setq evil-snipe-scope 'whole-buffer
        evil-snipe-smart-case t)
  (evil-snipe-override-mode 1))

(use-package evil-numbers
  :bind (:map evil-normal-state-map
              ("C-S-a" . evil-numbers/inc-at-pt)
              ("C-S-x" . evil-numbers/dec-at-pt)))

(use-package flx :defer 5)

(use-package help :config (setq help-window-select t))

(use-package info :config (evil-leader/set-key "hi" 'info))

(use-package ivy
  :diminish ivy-mode
  :commands magit-status
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
  :init
  (evil-leader/set-key
    "hv" 'counsel-describe-variable
    "hf" 'counsel-describe-function
    "f" 'counsel-find-file
    "r" 'ivy-recentf
    "b" 'ivy-switch-buffer
    "Th" 'counsel-load-theme)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-extra-directories '("./")
        ivy-count-format "%d "
        ivy-height 12
        ;; ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-initial-inputs-alist nil
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
  :defer 3
  :init
  (add-hook 'smartparens-enabled-hook
            (lambda () (when (member major-mode sp-lisp-modes) (lispy-mode))))
  (add-hook 'smartparens-disabled-hook
            (lambda () (when (member major-mode sp-lisp-modes) (lispy-mode -1))))
  :config
  (setq lispy-compat '(edebug cider)
        lispy-avy-keys sooheon--avy-keys
        lispy-avy-style-paren 'at-full
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
    (define-key map "\"" nil)           ; lispy-quotes
    (define-key map "\C-d" 'lispy-delete)
    (define-key map (kbd "M-)") nil)
    (define-key map (kbd "DEL") 'lispy-delete-backward))
  ;; (let ((map lispy-mode-map-parinfer))
  ;;   (define-key map (kbd "\"") nil)
  ;;   (define-key map (kbd "M-r") 'lispy-raise)
  ;;   (define-key map (kbd "#") nil)
  ;;   (define-key map (kbd ":") nil))

  ;; Unbind M-k and M-. in evil normal state and use lispy
  (define-key evil-normal-state-map "\M-." nil) ; evil-repeat-pop-next
  (define-key evil-normal-state-map "\M-k" nil))

(use-package lispyville
  :diminish lispyville-mode
  :init (add-hook 'lispy-mode-hook #'lispyville-mode)
  :commands (lispyville-delete
             lispyville-delete-char-or-splice
             lispyville-drag-forward
             lispyville-drag-backward)
  :config
  (setq lispyville-key-theme '(operators
                               (escape insert hybrid emacs)
                               slurp/barf-cp)
        lispyville-motions-put-into-special t
        lispyville-barf-stay-with-closing t)
  (define-key lispyville-mode-map "\M-n" 'lispyville-drag-forward)
  (define-key lispyville-mode-map "\M-p" 'lispyville-drag-backward))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :init
  (define-key evil-normal-state-map "gs" 'magit-status)
  (evil-leader/set-key "g" 'magit-status "G" 'magit-dispatch-popup)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-refresh-verbose t)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-upstream
                          'magit-insert-unpulled-from-upstream)
  ;; (magit-add-section-hook 'magit-status-sections-hook
  ;;                         'magit-insert-modules-unpulled-from-pushremote
  ;;                         'magit-insert-unpulled-from-upstream)
  ;; (magit-add-section-hook 'magit-status-sections-hook
  ;;                         'magit-insert-modules-unpushed-to-upstream
  ;;                         'magit-insert-unpulled-from-upstream)
  ;; (magit-add-section-hook 'magit-status-sections-hook
  ;;                         'magit-insert-modules-unpushed-to-pushremote
  ;;                         'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-submodules
                          'magit-insert-unpulled-from-upstream))

(use-package morlock
  :commands global-morlock-mode
  :init (global-morlock-mode))

(use-package org
  :defer 10
  :config
  (setq org-src-fontify-natively t
        org-startup-indented t
        org-adapt-indentation nil
        org-preview-latex-default-process 'dvisvgm
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
        org-babel-clojure-backend 'cider
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
  :defer t
  :init
  (add-hook 'org-mode-hook 'worf-mode)
  :config
  (evil-define-key 'insert worf-mode-map
        "\C-j" nil
        "\[" nil
        "\]" nil))

(use-package paren
  :config (show-paren-mode))

(use-package popwin
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

(use-package prog-mode
  :config (global-prettify-symbols-mode))

(use-package projectile
  :diminish projectile-mode
  :commands (counsel-projectile-ag projectile-global-mode)
  :defer 5
  :init
  (evil-leader/set-key
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
  (defun counsel-projectile-ag ()
    (interactive)
    (counsel-ag nil (projectile-project-root)))
  (setq projectile-enable-caching t
        projectile-sort-order 'recentf
        projectile-create-missing-test-files t)
  (projectile-global-mode))

(use-package counsel-projectile
  :defer 5
  :init
  (evil-leader/set-key
    "pb" 'counsel-projectile-switch-to-buffer
    "pd" 'counsel-projectile-find-dir
    "pp" 'counsel-projectile
    "pf" 'counsel-projectile-find-file
    "pr" 'projectile-recentf)
  :config
  (ivy-set-actions
   'counsel-projectile
   '(("d" (lambda (dir)
            (let ((projectile-switch-project-action 'counsel-projectile-find-dir))
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
      "open in vc-dir / magit / monky")
     ("t" (lambda (dir)
            (let ((projectile-switch-project-action (lambda () (projectile-run-term "/usr/local/bin/fish"))))
              (projectile-switch-project-by-name dir arg)))
      "start term for project"))))

(use-package recentf
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

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
                               (lambda () (term shell-pop-term-shell)))))

(use-package smartparens
  :init
  (setq sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-from-inside nil
        sp-show-pair-delay 0)
  (defun conditionally-enable-smartparens-mode ()
    "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
    (if (eq this-command 'eval-expression)
        (smartparens-mode)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)
  :config
  (smartparens-global-mode 1)
  (require 'smartparens-config)
  (show-smartparens-global-mode 1)
  (let ((m smartparens-mode-map))
    (define-key m (kbd "M-a") 'sp-beginning-of-sexp)
    (define-key m (kbd "M-e") 'sp-end-of-sexp)
    (define-key m [C-backspace] 'sp-backward-kill-sexp)
    (define-key m (kbd "C-)") 'sp-forward-slurp-sexp)
    (define-key m (kbd "C-(") 'sp-backward-slurp-sexp)
    (define-key m (kbd "C-{") 'sp-backward-barf-sexp)
    (define-key m (kbd "C-}") 'sp-forward-barf-sexp)))

(use-package smex :defer t)

(use-package simple
  :config
  (define-key messages-buffer-mode-map (kbd "s-k") 'bury-buffer)
  (column-number-mode))

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
  :config (global-ws-butler-mode))

(progn ;; Personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
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

(setq gc-cons-threshold 1000000)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
