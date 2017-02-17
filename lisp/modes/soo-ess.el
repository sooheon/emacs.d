(use-package ess
  :defer t
  :mode (("\\.sp\\'" . S-mode)
         ("/R/.*\\.q\\'" . R-mode)
         ("\\.[qsS]\\'" . S-mode)
         ("\\.ssc\\'" . S-mode)
         ("\\.SSC\\'" . S-mode)
         ("\\.[rR]\\'" . R-mode)
         ("\\.[rR]nw\\'" . Rnw-mode)
         ("\\.[sS]nw\\'" . Snw-mode)
         ("\\.[rR]profile\\'" . R-mode)
         ("NAMESPACE\\'" . R-mode)
         ("CITATION\\'" . R-mode)
         ("\\.omg\\'" . omegahat-mode)
         ("\\.hat\\'" . omegahat-mode)
         ("\\.lsp\\'" . XLS-mode)
         ("\\.do\\'" . STA-mode)
         ("\\.ado\\'" . STA-mode)
         ("\\.[Ss][Aa][Ss]\\'" . SAS-mode)
         ("\\.jl\\'" . ess-julia-mode)
         ("\\.[Ss]t\\'" . S-transcript-mode)
         ("\\.Sout" . S-transcript-mode)
         ("\\.[Rr]out" . R-transcript-mode)
         ("\\.Rd\\'" . Rd-mode)
         ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
         ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
  :commands (R stata julia SAS)
  :init
  (add-hook 'ess-mode-hook 'soo-run-prog-mode-hook)
  (add-hook 'inferior-ess-mode-hook 'soo-run-prog-mode-hook)
  :config
  (setq ess-first-continued-statement-offset 2
        ess-continued-statement-offset 0
        ess-expression-offset 2
        ess-nuke-trailing-whitespace-p t
        ess-default-style 'DEFAULT)
  (define-key ess-mode-map (kbd "<s-return>") 'ess-eval-line)
  (nmap :keymaps 'ess-mode-map
    "K" 'ess-display-help-on-object)
  (define-key inferior-ess-mode-map (kbd "M-n") 'comint-next-input)
  (define-key inferior-ess-mode-map (kbd "M-p") 'comint-previous-input)
  (define-key inferior-ess-mode-map (kbd "C-d") nil))

(use-package ess-smart-equals
  :defer t
  :init
  (add-hook 'ess-mode-hook 'ess-smart-equals-mode)
  (add-hook 'inferior-ess-mode-hook 'ess-smart-equals-mode))

(provide 'soo-ess)
