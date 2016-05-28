(defconst emacs-d (file-name-directory (or load-file-name buffer-file-name))
  "The giant turtle on whose shell the world rests.")
(setq package-user-dir (expand-file-name "elpa" emacs-d))
(package-initialize)
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))
(package-refresh-contents)

(defconst soo-packages
  '(ace-link ample-theme auto-compile avy bind-map cider clojure-mode closql company dash diff-hl diminish elisp-slime-nav emacsql epkg evil evil-commentary evil-leader evil-magit evil-matchit evil-multiedit evil-numbers evil-snipe evil-surround evil-textobj-anyblock evil-visualstar finalize flx flycheck flyspell-correct git-modes hydra iedit ivy ivy-hydra swiper counsel magit packed popwin projectile queue reveal-in-osx-finder shackle shell-pop smartparens smex solarized-theme spacemacs-theme spinner undo-tree use-package which-key window-numbering with-editor worf ws-butler zenburn-theme))

;; Install required
(dolist (package soo-packages)
  (unless (package-installed-p package)
    (ignore-errors
      (package-install package))))

;; Upgrade installed
(save-window-excursion
  (package-list-packages t)
  (package-menu-mark-upgrades)
  (condition-case nil
      (package-menu-execute t)
    (error
     (package-menu-execute))))
