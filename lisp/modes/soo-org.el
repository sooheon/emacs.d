(use-package org
  :defer 20
  :ensure nil
  :general
  ("C-c a" 'org-agenda)
  :config
  (setq org-hide-emphasis-markers t)
  (general-define-key :keymaps 'org-mode-map
    "M-n" 'org-metadown
    "M-p" 'org-metaup
    "M-h" 'org-metaleft
    "M-H" 'org-shiftmetaleft
    "M-l" 'org-metaright2
    "M-L" 'org-shiftmetaright
    "M-j" (lambda () (interactive)
            (org-meta-return)
            (evil-insert 1))
    "C-j" 'org-return
    "M-$" 'open-$)
  (nmap :keymaps 'org-mode-map
    [C-return] (lambda () (interactive)
                 (org-insert-heading-respect-content) (evil-append 1))
    [M-return] (lambda () (interactive) (org-meta-return) (evil-append 1))
    [return] 'org-open-at-point
    "t" 'org-todo
    "$" 'org-end-of-line
    "^" 'org-beginning-of-line
    ;; "-" 'org-ctrl-c-minus
    "<" 'org-metaleft
    ">" 'org-metaright)
  :config
  (setq org-hide-emphasis-markers t))


;; FIXME: https://bitbucket.org/mituharu/emacs-mac/commits/6e8c84bd419ab425c3359b4ca17e2da9e23136ad
(define-key org-mode-map (kbd "C-c l") 'org-store-link)
(use-package org-download
  :after org
  :config
  (org-download-enable)
  (setq org-download-method 'attach))

(use-package worf
  :after org
  :diminish worf-mode
  :config
  (general-define-key :keymaps 'worf-mode-map
    "M-j" nil
    "C-j" nil
    "[" nil
    "]" nil))

(use-package auctex :defer t)

(use-package cdlatex :defer t
  :config
  (setq cdlatex-command-alist
        '(("angl" "Insert annuity symbol" "_{\\angln i}"
           cdlatex-position-cursor nil nil t)
          ("dd" "Insert ddot" "\\ddot{?}"
           cdlatex-position-cursor nil nil t))))

(use-package pamparam
  :ensure nil
  :general
  (:keymaps 'org-mode-map
   "C-c y" 'pam-drill))

(use-package org-pomodoro
  :disabled t
  :general
  ("C-s-r" 'org-pomodoro))

;;;###autoload
(defun soo-org-hook ()
  (worf-mode)
  (turn-on-org-cdlatex)
  ;; (auto-fill-mode 1)
  (smartparens-mode 1)
  (org-indent-mode)
  (toggle-truncate-lines -1))
(add-hook 'org-mode-hook 'soo-org-hook)

(defun latexify-line ()
  (interactive)
  (if (region-active-p)
      (print "to be impl.")
    (save-excursion
      (beginning-of-line)
      (insert "$")
      (end-of-line)
      (insert "$"))))

;;;###autoload
(defun open-$ ()
  (interactive)
  (if (region-active-p)
      (print "to be impl.")
    (progn
      (insert "$$$$")
      (backward-char)
      (backward-char)
      (evil-insert 1))))

(setq-default org-export-in-background nil
              org-src-fontify-natively t
              org-M-RET-may-split-line nil
              org-catch-invisible-edits 'show
              org-footnote-auto-adjust t
              org-hide-emphasis-markers t
              org-return-follows-link t
              org-startup-with-latex-preview t
              org-startup-with-inline-images t
              org-log-done 'time
              org-highlight-latex-and-related '(latex script entities)
              org-adapt-indentation nil
              org-preview-latex-default-process 'dvisvgm)

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

;; Org Babel
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      geiser-default-implementation 'guile
      org-babel-load-languages '((python . t)
                                 (clojure . t)
                                 (dot . t))
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

(setq org-export-backends '(ascii html latex odt gfm)
      org-export-coding-system 'utf-8
      org-html-html5-fancy t
      org-html-postamble nil
      org-export-with-smart-quotes t)

(with-eval-after-load 'smartparens
  (sp-local-pair 'org-mode "\\left(" " \\right)")
  (sp-local-pair 'org-mode "\\left[" " \\right]")
  (sp-local-pair 'org-mode "\\left{" " \\right}")
  (sp-local-pair 'org-mode "\\left|" " \\right|"))

(provide 'soo-org)
