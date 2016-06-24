;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

(defvar before-user-init-time (current-time)
  "Value of `current-time' when Emacs begins loading `user-init-file'.")
(message "Loading Emacs...done (%.3fs)"
         (float-time (time-subtract before-user-init-time before-init-time)))
(setq user-init-file (or load-file-name buffer-file-name))
(setq emacs-d (file-name-directory user-init-file))
(add-to-list 'load-path (concat emacs-d "layers/"))
(setq package-user-dir (expand-file-name "elpa" emacs-d))
(package-initialize)

(message "Loading %s..." user-init-file)

;; Theme
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'eclipse2 t)
;; Font
(ignore-errors (set-frame-font "Input Mono Narrow"))
;; Customize
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(hl-line-mode 1)
(setq-default inhibit-startup-screen t
              initial-scratch-message ""
              initial-major-mode 'emacs-lisp-mode
              load-prefer-newer t
              create-lockfiles nil      ; Don't create #foo.file#
              fill-column 80)
(eval '(setq inhibit-startup-echo-area-message "sooheon"))
(setq frame-title-format '((:eval (if buffer-file-name
                                      (abbreviate-file-name buffer-file-name)
                                    "%b"))
                           (:eval (if (buffer-modified-p) " •"))
                           " - Emacs"))
;; Navigation within buffer
(setq recenter-positions '(top middle bottom))
;; Finding files
(setq vc-follow-symlinks t)
(setq find-file-suppress-same-file-warnings t)
(setq read-buffer-completion-ignore-case t)
;; Editor behavior
(setq default-input-method "korean-hangul"
      indent-tabs-mode nil
      ring-bell-function 'ignore
      highlight-nonselected-windows t
      backup-inhibited t
      kill-buffer-query-functions nil
      enable-recursive-minibuffers t
      ;; https://github.com/railwaycat/emacs-mac-port/issues/78
      mac-pass-command-to-system nil)
(minibuffer-depth-indicate-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'server-switch-hook 'raise-frame)
(setq eval-expression-print-length nil
      eval-expression-print-level nil
      sentence-end-double-space nil
      search-default-mode 'character-fold-to-regexp
      replace-character-fold t)
;; Shell
(setq shell-file-name "/usr/local/bin/bash")
(setq explicit-shell-file-name "/usr/local/bin/fish")
;; Internals
(setq gc-cons-threshold (* 10 1024 1024)
      ad-redefinition-action 'accept)
;; Set PATHs--see: http://tinyurl.com/ctf9h3a
(setenv "PATH" "/Users/sooheon/.local/bin:/usr/local/Cellar/pyenv-virtualenv/20160315/shims:/Users/sooheon/.pyenv/shims:/usr/local/opt/coreutils/libexec/gnubin:/usr/local/opt/findutils/libexec/gnubin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/Library/TeX/texbin")
(setenv "MANPATH" "/usr/local/opt/coreutils/libexec/gnuman:/usr/local/opt/findutils/libexec/gnuman:/usr/local/share/man")
(setq exec-path '("/Users/sooheon/.local/bin" "/usr/local/Cellar/pyenv-virtualenv/20160315/shims" "/Users/sooheon/.pyenv/shims" "/usr/local/opt/coreutils/libexec/gnubin" "/usr/local/opt/findutils/libexec/gnubin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/opt/X11/bin" "/Library/TeX/texbin" "/usr/local/Cellar/emacs/HEAD/libexec/emacs/25.1.50/x86_64-apple-darwin15.5.0"))

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
                evil-cross-lines t
                evil-symbol-word-search t
                evil-move-cursor-back nil
                evil-want-C-i-jump t
                evil-disable-insert-state-bindings t
                evil-search-module 'evil-search
                evil-ex-search-persistent-highlight nil)
  (defmacro spacemacs|define-text-object (key name start end)
    (let ((inner-name (make-symbol (concat "evil-inner-" name)))
          (outer-name (make-symbol (concat "evil-outer-" name)))
          (start-regex (regexp-opt (list start)))
          (end-regex (regexp-opt (list end))))
      `(progn
         (evil-define-text-object ,inner-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex beg end type count nil))
         (evil-define-text-object ,outer-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex beg end type count t))
         (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
         (define-key evil-outer-text-objects-map ,key (quote ,outer-name))
         (with-eval-after-load 'evil-surround
           (push (cons (string-to-char ,key)
                       (if ,end
                           (cons ,start ,end)
                         ,start))
                 evil-surround-pairs-alist)))))
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map "\C-w" 'evil-delete-backward-word)
  ;; define text objects
  (spacemacs|define-text-object "$" "dollar" "$" "$")
  (spacemacs|define-text-object "*" "star" "*" "*")
  ;; (spacemacs|define-text-object "8" "block-star" "/*" "*/")
  ;; (spacemacs|define-text-object "|" "bar" "|" "|")
  (spacemacs|define-text-object "%" "percent" "%" "%")
  (spacemacs|define-text-object "/" "slash" "/" "/")
  (spacemacs|define-text-object "_" "underscore" "_" "_")
  (spacemacs|define-text-object "-" "hyphen" "-" "-")
  (spacemacs|define-text-object "~" "tilde" "~" "~")
  (spacemacs|define-text-object "=" "equal" "=" "=")
  (evil-define-text-object evil-inner-buffer (count)
    (list (point-min) (point-max)))
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer))

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
    "wo" 'delete-other-windows
    "ww" 'evil-window-next
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
  (evil-leader/set-key "ap" 'package-list-packages))

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

(use-package compile
  :defer t
  :init
  (define-key prog-mode-map [C-f9] #'compile)
  (define-key prog-mode-map [f9] #'endless/compile-please)
  :config
  (setq compilation-ask-about-save nil
        compilation-scroll-output 'next-error
        compilation-skip-threshold 2)
  (defun endless/compile-please (comint)
    "Compile without confirmation.
With a prefix argument, use comint-mode."
    (interactive "P")
    ;; Do the command without a prompt.
    (save-window-excursion
      (compile (eval compile-command) (and comint t)))
    (pop-to-buffer (get-buffer "*compilation*"))))

(use-package cider
  :defer t
  :init
  (add-hook 'clojure-mode-hook 'cider-mode)
  (setq cider-repl-pop-to-buffer-on-connect nil
        cider-prompt-save-file-on-load nil
        cider-repl-use-clojure-font-lock t
        cider-font-lock-dynamically t
        cider-mode-line '(:eval (format " [%s]" (cider--modeline-info)))
        cider-default-repl-command "boot")
  :config
  ;; (defadvice cider-jump-to-var (before add-evil-jump activate)
  ;;   (evil-set-jump))
  (evil-define-key 'normal cider-mode-map "K" 'cider-doc)
  (evil-set-initial-state 'cider-docview-mode 'insert)
  (evil-set-initial-state 'cider-stacktrace-mode 'insert)
  (add-hook 'cider-mode-hook 'eldoc-mode))

(use-package clojure-mode
  :defer t
  :mode ("\\.boot\\'" . clojure-mode)
  :init
  (add-to-list 'magic-mode-alist '("#!.*boot\\s-*$" . clojure-mode))
  (add-to-list 'magic-mode-alist '("#!.*planck\\s-*$" . clojurescript-mode))
  :config
  ;; This is for clojure-semantic, the library file is clojure.el
  (load-library "clojure"))

(use-package diff-hl
  :defer 7
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
  (setq dired-listing-switches "-laGh1v --group-directories-first")
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
    "gp" 'magit-dispatch-popup
    "got" 'soo--terminal-pop
    "gof" 'reveal-in-osx-finder
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

(use-package circe
  :disabled t
  :defer t
  :init
  (setq circe-network-options '(("Freenode"
                                 :nick "sooheon"
                                 :channels ("#emacs" "#clojure" "#haskell"
                                            "##crawl"
                                            ;; "#lesswrong"
                                            )
                                 :nickserv-password "qwefasdf")))
  (defun sooheon--switch-to-circe ()
    "Switch to CIRCE buffers using completing-read, or start
CIRCE if no buffers open."
    (interactive)
    (let (candidates (list))
      (dolist (buf (buffer-list) candidates)
        (if (or (equal 'circe-channel-mode (with-current-buffer buf major-mode))
                (equal 'circe-server-mode (with-current-buffer buf major-mode)))
            (setq candidates (append (list (buffer-name buf)) candidates))))
      (if candidates
          (switch-to-buffer (completing-read "IRC buffer: " candidates))
        (circe "Freenode"))))
  (evil-leader/set-key "ai" 'sooheon--switch-to-circe)
  :config
  (setq circe-reduce-lurker-spam t
        tracking-position 'end
        tracking-most-recent-first t)
  (enable-circe-color-nicks)
  (enable-circe-highlight-all-nicks)
  (require 'lui-autopaste)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste))
(use-package circe
  :disabled t
  :defer t
  :init
  (setq circe-network-options '(("Freenode"
                                 :nick "sooheon"
                                 :channels ("#emacs" "#clojure" "#haskell"
                                            "##crawl"
                                            ;; "#lesswrong"
                                            )
                                 :nickserv-password "qwefasdf")))
  (defun sooheon--switch-to-circe ()
    "Switch to CIRCE buffers using completing-read, or start
CIRCE if no buffers open."
    (interactive)
    (let (candidates (list))
      (dolist (buf (buffer-list) candidates)
        (if (or (equal 'circe-channel-mode (with-current-buffer buf major-mode))
                (equal 'circe-server-mode (with-current-buffer buf major-mode)))
            (setq candidates (append (list (buffer-name buf)) candidates))))
      (if candidates
          (switch-to-buffer (completing-read "IRC buffer: " candidates))
        (circe "Freenode"))))
  (evil-leader/set-key "ai" 'sooheon--switch-to-circe)
  :config
  (setq circe-reduce-lurker-spam t
        tracking-position 'end
        tracking-most-recent-first t)
  (enable-circe-color-nicks)
  (enable-circe-highlight-all-nicks)
  (require 'lui-autopaste)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste))

(use-package erc
  :disabled t
  :defer t
  :init
  (defun sooheon--erc ()
    (interactive)
    (erc :server "irc.freenode.net"
         :port 6667
         :nick "sooheon"))
  (defun sooheon--switch-to-erc ()
    "Switch to ERC buffers using completing-read, or start ERC
if no buffers open."
    (interactive)
    (let (candidates (list))
      (dolist (buf (buffer-list) candidates)
        (if (equal 'erc-mode (with-current-buffer buf major-mode))
            (setq candidates (append (list (buffer-name buf)) candidates))))
      (if candidates
          (switch-to-buffer (completing-read "IRC buffer: " candidates))
        (call-interactively 'sooheon--erc))))
  (evil-leader/set-key "ai" 'sooheon--switch-to-erc)
  :config
  (setq erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE"
                        "324" "329" "332" "333" "353" "477")
        erc-track-shorten-aggressively 'max
        erc-track-position-in-mode-line t
        erc-prompt-for-password nil
        erc-autojoin-channels-alist
        '(("freenode.net" "#clojure" "#lesswrong" "##crawl"))
        erc-join-buffer 'bury
        erc-nickserv-passwords '((freenode (("sooheon" . "qwefasdf"))))))

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

(use-package evil-cleverparens-text-objects
  :config
  (define-key evil-outer-text-objects-map "f" #'evil-cp-a-form)
  (define-key evil-inner-text-objects-map "f" #'evil-cp-inner-form)
  (define-key evil-outer-text-objects-map "c" #'evil-cp-a-comment)
  (define-key evil-inner-text-objects-map "c" #'evil-cp-inner-comment)
  (define-key evil-outer-text-objects-map "d" #'evil-cp-a-defun)
  (define-key evil-inner-text-objects-map "d" #'evil-cp-inner-defun))

(use-package evil-matchit
  :commands (evilmi-jump-items)
  :init
  (evil-define-key 'normal global-map "%" 'evilmi-jump-items)
  (evil-define-key 'visual global-map "%" 'evilmi-jump-items)
  :config
  (global-evil-matchit-mode))

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
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (push '(?` . ("`" . "'")) evil-surround-pairs-alist))))

(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :config
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'buffer
        evil-snipe-show-prompt nil
        evil-snipe-smart-case t
        evil-snipe-repeat-keys nil)
  (evil-snipe-override-mode 1))

(use-package evil-numbers
  :bind (:map evil-normal-state-map
              ("C-S-a" . evil-numbers/inc-at-pt)
              ("C-S-x" . evil-numbers/dec-at-pt)))

(use-package flycheck
  :defer t
  :config
  (setq flycheck-global-modes nil))

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
  :bind (("C-;" . flyspell-auto-correct-previous-word))
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package flyspell-correct
  :disabled t
  :after flyspell
  :config
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-word-generic)
  (setq flyspell-correct-interface 'flyspell-correct-ivy))

(use-package speck
  :defer t
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

(use-package flx :after ivy)

(use-package help
  :config
  (setq help-window-select t)
  (evil-set-initial-state 'help-mode 'insert))

(use-package highlight-escape-sequences
  :defer
  :init
  (add-hook 'prog-mode-hook 'hes-mode))

(use-package iedit :defer t :init (setq iedit-toggle-key-default nil))

(use-package info
  :config
  (evil-leader/set-key "hi" 'info)
  (evil-set-initial-state 'Info-mode 'insert))

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
    "aP" 'counsel-list-processes)
  (define-key evil-normal-state-map "\M-y" 'counsel-yank-pop))

(use-package swiper
  :bind (([remap isearch-forward] . counsel-grep-or-swiper)
         ("s-f" . counsel-grep-or-swiper)))

(use-package inf-clojure
  :defer t
  :config
  (setq inf-clojure-program "planck")
  (add-hook 'inf-clojure-mode-hook #'smartparens-mode))

(use-package ivy
  :diminish ivy-mode
  :commands (magit-status epkg-describe-package)
  :bind (("s-b" . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         ("<f2> j" . counsel-set-variable))
  :init
  (evil-leader/set-key
    "r" 'ivy-recentf
    "b" 'ivy-switch-buffer)
  :config
  (with-eval-after-load 'recentf (setq ivy-use-virtual-buffers t))
  (setq ivy-extra-directories '("./")
        ivy-count-format "%d "
        ivy-height 12
        ivy-re-builders-alist '(;; (counsel-M-x . ivy--regex-fuzzy)
                                (t . ivy--regex-plus))
        ivy-initial-inputs-alist '((org-refile . "^")
                                   (org-agenda-refile . "^")
                                   (org-capture-refile . "^")
                                   (man . "^")
                                   (woman . "^"))
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
        lispy-safe-paste t
        lispy-safe-copy t
        lispy-safe-delete t
        lispy-comment-use-single-semicolon t)
  (lispy-set-key-theme '(special
                         c-digits
                         paredit))
  (dolist (map (list lispy-mode-map-paredit lispy-mode-map-parinfer))
    (define-key map (kbd "C-a") nil)
    (define-key map "\M-j" 'lispy-split)
    (define-key map "\M-k" 'lispy-kill-sentence)
    (define-key map [M-up] 'sp-splice-sexp-killing-backward)
    (define-key map [M-down] 'sp-splice-sexp-killing-forward))
  (let ((map lispy-mode-map-paredit))
    (define-key map "\M-n" nil)         ; lispy left
    (define-key map "\M-p" nil)
    (define-key map "\"" nil)           ; lispy-doublequote
    (define-key map "\C-d" 'lispy-delete)
    (define-key map (kbd "M-)") nil)
    (define-key map (kbd "DEL") 'lispy-delete-backward)
    (define-key map (kbd "C-)") nil)
    (define-key map (kbd "C-(") nil)
    (define-key map (kbd "C-}") nil)
    (define-key map (kbd "C-{") nil))
  (lispy-define-key lispy-mode-map-special ">" 'lispy-slurp-or-barf-right)
  (lispy-define-key lispy-mode-map-special "<" 'lispy-slurp-or-barf-left)

  ;; Unbind M-k and M-. in evil normal state and use lispy
  (define-key evil-normal-state-map "\M-." nil) ; evil-repeat-pop-next
  (define-key evil-normal-state-map "\M-k" nil))

(use-package lispyville
  :after lispy
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
  :commands (magit-status magit-dispatch-popup)
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

(use-package multiple-cursors
  :defer t
  :init
  (evil-define-key 'visual global-map "m" 'mc/mark-all-like-this-dwim)
  ;; Malabarba keybinds
  ;; http://endlessparentheses.com/multiple-cursors-keybinds.html
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

(use-package super-save
  :ensure t
  :diminish super-save-mode
  :config
  (setq auto-save-default nil)
  (super-save-mode 1))

(use-package org
  :defer 10
  :diminish org-indent-mode
  :config
  (add-to-list 'load-path (expand-file-name "lib/org/contrib/lisp/" emacs-d))
  (setq org-src-fontify-natively t
        ;; org-startup-indented t
        org-startup-folded nil
        org-startup-truncated nil
        org-adapt-indentation nil
        org-preview-latex-default-process 'dvisvgm
        org-inhibit-startup-visibility-stuff nil
        org-M-RET-may-split-line nil
        org-catch-invisible-edits nil
        org-footnote-auto-adjust t
        org-hide-emphasis-markers t
        org-return-follows-link t
        org-startup-with-inline-images t
        org-log-done 'time)

  (defun org-metaright2 (&optional arg)
    "My evil version of `org-metaright', to be bound to M-l and
forward to downcase-word"
    (interactive "P")
    (cond
     ((run-hook-with-args-until-success 'org-metaright-hook))
     ((org-at-table-p) (call-interactively 'org-table-move-column))
     ((org-at-drawer-p) (call-interactively 'org-indent-drawer))
     ((org-at-block-p) (call-interactively 'org-indent-block))
     ((org-with-limited-levels
       (or (org-at-heading-p)
           (and (org-region-active-p)
                (save-excursion
                  (goto-char (region-beginning))
                  (org-at-heading-p)))))
      (when (org-check-for-hidden 'headlines) (org-hidden-tree-error))
      (call-interactively 'org-do-demote))
     ;; At an inline task.
     ((org-at-heading-p)
      (call-interactively 'org-inlinetask-demote))
     ((or (org-at-item-p)
          (and (org-region-active-p)
               (save-excursion
                 (goto-char (region-beginning))
                 (org-at-item-p))))
      (when (org-check-for-hidden 'items) (org-hidden-tree-error))
      (call-interactively 'org-indent-item))
     (t (call-interactively 'downcase-word))))

  (define-key org-mode-map "\M-n" 'org-metadown)
  (define-key org-mode-map "\M-p" 'org-metaup)
  (define-key org-mode-map "\M-h" 'org-metaleft)
  (define-key org-mode-map "\M-l" 'org-metaright2)
  (evil-define-key 'normal org-mode-map
    [C-return] (lambda () (interactive) (org-insert-heading-respect-content) (evil-append 1))
    [M-return] (lambda () (interactive) (org-meta-return) (evil-append 1))
    [return] 'org-open-at-point
    "t" 'org-todo
    "$" 'org-end-of-line
    "^" 'org-beginning-of-line
    ;; "-" 'org-ctrl-c-minus
    "<" 'org-metaleft
    ">" 'org-metaright)
  (evil-define-key 'insert org-mode-map
    "\C-j" 'org-return
    "\M-j" 'org-meta-return)

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
    ("o" nil "quit")
    ("C-g" nil))
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

(use-package org-download
  :after org
  :config
  (org-download-enable)
  (setq org-download-method 'attach))

(use-package ox
  :after org
  :config
  (setq org-export-backends '(ascii html latex odt gfm)
        org-export-coding-system 'utf-8
        org-html-html5-fancy t
        org-html-postamble nil)
  (fset 'latexify-line
        (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([95 105 36 escape 65 36 escape] 0 "%d")) arg))))

(use-package osx-dictionary
  :disabled t
  :commands osx-dictionary-search-pointer
  :init
  (evil-leader/set-key "xwd" 'osx-dictionary-search-pointer)
  :config
  (evilified-state-evilify-map osx-dictionary-mode-map
    :mode osx-dictionary-mode
    :bindings
    "q" 'osx-dictionary-quit
    "r" 'osx-dictionary-read-word
    "s" 'osx-dictionary-search-input
    "o" 'osx-dictionary-open-dictionary.app))

(use-package pdf-tools
  :disabled t
  :defer t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :init
  (setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")
  :config
  (pdf-tools-install))

(use-package worf
  :diminish worf-mode
  :after org
  :init
  (add-hook 'org-mode-hook 'worf-mode)
  :config
  (define-key worf-mode-map "\C-j" nil)
  (define-key worf-mode-map "\[" nil)
  (define-key worf-mode-map "\]" nil))

(use-package woman
  :defer t
  :config
  (evil-set-initial-state 'woman-mode 'insert))

(use-package shackle
  :config
  (shackle-mode 1)
  (setq shackle-rules '((compilation-mode :noselect t)
                        (help-mode :noselect t)
                        ("*undo-tree*" :size 0.3)
                        (woman-mode :popup t))))

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

(use-package rainbow-mode
  :commands rainbow-mode
  :diminish rainbow-mode
  :init (evil-leader/set-key "C" 'rainbow-mode))

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

(use-package saveplace
  :if (version< "25" emacs-version)
  :config (save-place-mode))

(use-package sentence-navigation
  :disabled t
  :defer t
  :init
  (define-key evil-normal-state-map ")" 'sentence-nav-evil-forward)
  (define-key evil-normal-state-map "(" 'sentence-nav-evil-backward)
  (define-key evil-motion-state-map ")" 'sentence-nav-evil-forward)
  (define-key evil-motion-state-map "(" 'sentence-nav-evil-backward)
  (define-key evil-normal-state-map "g)" 'sentence-nav-evil-forward-end)
  (define-key evil-normal-state-map "g(" 'sentence-nav-evil-backward-end)
  (define-key evil-outer-text-objects-map "s" 'sentence-nav-evil-outer-sentence)
  (define-key evil-inner-text-objects-map "s" 'sentence-nav-evil-inner-sentence)
  (bind-key "M-e" 'sentence-nav-forward)
  (bind-key "M-a" 'sentence-nav-backward))

(use-package shell-pop
  :bind (("s-`" . shell-pop))
  :init
  (setq shell-pop-window-position 'bottom
        shell-pop-window-height 30
        shell-pop-full-span t
        shell-pop-shell-type '("terminal" "*terminal*"
                               (lambda () (term explicit-shell-file-name))))
  :config
  (use-package term
    :config
    (setq term-suppress-hard-newline t
          term-scroll-to-bottom-on-output t)
    (define-key term-raw-map (kbd "s-v") 'term-paste)
    (evil-define-key 'normal term-raw-map "p" 'term-paste)
    (add-hook 'term-mode-hook (lambda () (toggle-truncate-lines 1)))))

(use-package smartparens
  :defer t
  :init
  (setq sp-cancel-autoskip-on-backward-movement t
        sp-autoskip-closing-pair 'always
        sp-show-pair-from-inside nil
        sp-show-pair-delay 0
        sp-highlight-pair-overlay nil)
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
  (defun soo-end-of-sexp-or-next ()
    (interactive)
    (if (looking-at "[])}]")
        (sp-end-of-next-sexp)
      (sp-end-of-sexp)))
  (defun soo-insert-at-bos ()
    (interactive)
    (progn (sp-beginning-of-sexp) (evil-insert-state)))
  (defun soo-insert-at-eos ()
    (interactive)
    (progn (sp-end-of-sexp) (evil-insert-state)))
  (let ((m smartparens-mode-map))
    (define-key m (kbd "M-a") 'sp-beginning-of-sexp)
    (define-key m (kbd "M-e") 'soo-end-of-sexp-or-next)
    (define-key m [C-backspace] 'sp-backward-kill-sexp)
    (define-key m (kbd "C-)") 'sp-forward-slurp-sexp)
    (define-key m (kbd "C-(") 'sp-backward-slurp-sexp)
    (define-key m (kbd "C-{") 'sp-backward-barf-sexp)
    (define-key m (kbd "C-}") 'sp-forward-barf-sexp))
  (evil-define-key 'normal smartparens-mode-map
    "\M-i" 'soo-insert-at-bos
    "\M-a" 'soo-insert-at-eos))

(use-package smex :defer t :config (setq smex-history-length 32))

(use-package simple
  :diminish (auto-fill-mode visual-line-mode)
  :init
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'auto-fill-mode)
  :config
  (column-number-mode))

(use-package typo
  :disabled t
  :init
  (typo-global-mode 1)
  (setq typo-language 'English)
  (add-hook 'text-mode-hook 'typo-mode))

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
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-auto-save-history t))

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
         ("s-6" . select-window-6))
  :config
  (let ((m window-numbering-keymap))
    (define-key m "\M-0" nil)
    (define-key m "\M-1" nil)
    (define-key m "\M-2" nil)
    (define-key m "\M-3" nil)
    (define-key m "\M-4" nil)
    (define-key m "\M-5" nil)
    (define-key m "\M-6" nil)
    (define-key m "\M-7" nil)
    (define-key m "\M-8" nil)
    (define-key m "\M-9" nil))
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

;; Personalize
(require 'sooheon)
(require 'soo-haskell)

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
