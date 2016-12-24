(use-package shell-pop
  :disabled t
  :defer t
  :general
  ("s-`" 'shell-pop)
  :init
  (setq shell-pop-window-position 'bottom
        shell-pop-window-height 30
        shell-pop-full-span t
        shell-pop-autocd-to-working-dir nil
        shell-pop-restore-window-configuration nil)
  ;; For some reason, returning to a term buffer with some text causes cursor to
  ;; be misplaced. This hook places cursor back at the prompt.
  (add-hook 'shell-pop-in-after-hook
            (lambda () (goto-char (point-max)) (backward-char 1)))
  ;; Redefine shell-pop: With arg, auto-cd to working directory, otherwise just
  ;; pop the shell. Don't make multiple shells.
  (defalias 'shell-pop
    (lambda (arg)
      (interactive "P")
      (if (string= (buffer-name) shell-pop-last-shell-buffer-name)
          (if (null arg)
              (shell-pop-out)
            (shell-pop--switch-to-shell-buffer (prefix-numeric-value arg)))
        (progn (if (null arg)
                   (setq shell-pop-autocd-to-working-dir nil)
                 (setq shell-pop-autocd-to-working-dir t))
               (shell-pop-up shell-pop-last-shell-buffer-index))))))

(use-package typo
  :disabled t
  :init
  (typo-global-mode 1)
  (setq typo-language 'English)
  (add-hook 'text-mode-hook 'typo-mode))
