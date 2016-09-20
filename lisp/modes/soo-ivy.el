(use-package counsel
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
            "C-c g" 'counsel-git
            "C-c j" 'counsel-git-grep
            "C-x l" 'counsel-locate
            "C-x C-l" 'find-library
            "C-c o" 'counsel-outline)
  :init
  (nmap :prefix "SPC"
    "r" 'counsel-recentf
    "hv" 'counsel-describe-variable
    "hf" 'counsel-describe-function
    "hk" 'describe-key
    "f" 'counsel-find-file
    "th" 'counsel-load-theme
    "ap" 'counsel-list-processes)
  (define-key evil-normal-state-map "\M-y" 'counsel-yank-pop)
  :config
  (use-package smex :config (csetq smex-history-length 32)))

(use-package swiper
  :general ([remap isearch-forward] 'counsel-grep-or-swiper
            "s-f" 'counsel-grep-or-swiper
            "C-c u" 'swiper-all))

(use-package ivy
  :diminish ivy-mode
  :commands (magit-status epkg-describe-package)
  :general ("s-b" 'ivy-switch-buffer
            "C-c r" 'ivy-resume
            "<f2> j" 'counsel-set-variable
            "C-c v" 'ivy-push-view
            "C-c V" 'ivy-pop-view)
  :init
  (nmap :prefix "SPC" "b" 'ivy-switch-buffer)
  :config
  (ivy-mode 1)
  (use-package flx)
  (setq ivy-extra-directories '("./")
        ivy-use-virtual-buffers t
        ivy-count-format "%d "
        ivy-height 12
        ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)
                                (t . ivy--regex-plus)
                                ;; (swiper . ivy--regex-plus)
                                ;; (counsel-recentf . ivy--regex-plus)
                                ;; (wspecial-worf-goto . ivy--regex-plus)
                                ;; (t . ivy--regex-fuzzy)
                                )
        ivy-initial-inputs-alist '((org-refile . "^")
                                   (org-agenda-refile . "^")
                                   (org-capture-refile . "^")
                                   (man . "^")
                                   (woman . "^"))
        ivy-action-wrap t
        ivy-sort-matches-functions-alist
        '((t . nil)
          (ivy-switch-buffer . ivy-sort-function-buffer)
          (counsel-find-file . ivy-sort-function-buffer)))
  (let ((m ivy-minibuffer-map))
    (define-key m [escape] 'minibuffer-keyboard-quit)
    (define-key m (kbd "<s-backspace>")
      (lambda () (interactive) (kill-line 0))))

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

(provide 'soo-ivy)
