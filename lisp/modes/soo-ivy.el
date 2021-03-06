(use-package counsel
  :ensure t
  :commands (counsel-M-x
             counsel-describe-function
             counsel-describe-variable
             counsel-descbinds
             counsel-describe-face
             counsel-find-file
             counsel-imenu
             counsel-load-library
             counsel-yank-pop
             counsel-info-lookup-symbol
             counsel-mark-ring
             counsel-info-lookup-symbol)
  :general ([remap execute-extended-command] 'counsel-M-x
            [remap describe-function] 'counsel-describe-function
            [remap describe-variable] 'counsel-describe-variable
            [remap describe-bindings] 'counsel-descbinds
            [remap find-file] 'counsel-find-file
            [remap imenu] 'counsel-imenu
            [remap load-library] 'counsel-load-library
            [remap yank-pop] 'counsel-yank-pop
            [remap info-lookup-symbol] 'counsel-info-lookup-symbol
            [remap menu-bar-open] 'counsel-tmm
            [remap list-bookmarks] 'counsel-bookmark
            "C-c f" 'counsel-git
            "C-c g" 'counsel-git-grep
            "C-x l" 'counsel-locate
            "C-x C-l" 'find-library
            "C-c o" 'counsel-outline)
  (nmap :prefix "SPC"
    "f" 'counsel-find-file
    "r" 'counsel-recentf
    "hv" 'counsel-describe-variable
    "hf" 'counsel-describe-function
    "hk" 'describe-key
    "th" 'counsel-load-theme
    "ap" 'counsel-list-processes
    "M-y" 'counsel-yank-pop)
  :init
  (setq counsel-mode-override-describe-bindings t)
  :config
  (use-package smex :config (setq smex-history-length 32)))

(use-package swiper
  :ensure t
  :general ([remap isearch-forward] 'counsel-grep-or-swiper
            "s-f" 'counsel-grep-or-swiper
            "C-c u" 'swiper-all)
  :config
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :commands (magit-status epkg-describe-package)
  :general ("s-b" 'ivy-switch-buffer
            "C-c r" 'ivy-resume
            "C-c j" 'lispy-goto
            "C-h C-v" 'counsel-set-variable
            "C-c v" 'ivy-push-view
            "C-c V" 'ivy-pop-view)
  (:keymaps 'ivy-minibuffer-map
   "s-f" 'ivy-next-line-or-history
   [escape] 'minibuffer-keyboard-quit
   "<s-backspace>" (lambda () (interactive) (kill-line 0))
   "<M-backspace>" 'ivy-backward-kill-word
   "C-w" 'ivy-backward-kill-word)
  (nmap "gb" 'ivy-switch-buffer)
  (nmap :prefix "SPC" "b" 'ivy-switch-buffer)
  :config
  (ivy-mode 1)
  (setq
   ivy-extra-directories '("./")
   ivy-use-virtual-buffers t
   ivy-count-format "(%d/%d) "
   ivy-height 12
   ivy-re-builders-alist '((t . ivy--regex-plus)
                           ;; (t . ivy--regex-fuzzy)
                           )
   ivy-initial-inputs-alist '((org-refile . "^")
                              (org-agenda-refile . "^")
                              (org-capture-refile . "^")
                              (man . "^")
                              (woman . "^"))
   ivy-action-wrap t
   ivy-sort-functions-alist
   '((read-file-name-internal . ivy-sort-file-function-default)
     (internal-complete-buffer . ivy-sort-file-function-default)
     (counsel-git-grep-function)
     (Man-goto-section)
     (org-refile)
     (t . string-lessp))))

(use-package ivy-hydra
  :ensure t
  :commands soo-ivy/body
  :general (:keymaps 'ivy-minibuffer-map "C-o" 'soo-ivy/body)
  :config
  (defhydra soo-ivy (:hint nil :color pink)
    "
 Move     ^^^^^^^^^^ | Call         ^^^^ | Cancel^^ | Options^^ |     Action _w_/_s_/_a_: %s(ivy-action-name)
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
    ("o" ivy-occur :exit t)))

(use-package flx
  :ensure t)

(provide 'soo-ivy)
