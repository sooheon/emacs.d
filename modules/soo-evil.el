(use-package evil-commentary
  :diminish evil-commentary-mode
  :init (evil-commentary-mode))

(use-package evil-matchit
  :defer 3
  :config
  (global-evil-matchit-mode))

(use-package evil-visualstar
  :defer 4
  :config
  (global-evil-visualstar-mode))

(use-package evil-surround
  :defer 4
  :config
  (global-evil-surround-mode 1))

(use-package evil-numbers
  :bind (:map evil-normal-state-map
              ("C-A" . evil-numbers/inc-at-pt)
              ("C-X" . evil-numbers/dec-at-pt)))

(provide 'soo-evil)
