(defconst emacs-d (file-name-directory (or load-file-name buffer-file-name))
  "The giant turtle on whose shell the world rests.")
(setq package-user-dir (expand-file-name "elpa" emacs-d))
(package-initialize)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-refresh-contents)

(defconst soo-packages
  '(ace-link
    ample-theme
    anaconda-mode
    auto-compile
    avy
    bind-map
    cider
    clojure-mode
    closql
    company
    company-anaconda
    counsel
    counsel-projectile
    dash
    diff-hl
    diminish
    dired+
    elisp-slime-nav
    emacsql
    epkg
    evil
    evil-commentary
    evil-cleverparens
    evil-leader
    evil-magit
    evil-matchit
    evil-multiedit
    evil-numbers
    evil-snipe
    evil-surround
    evil-textobj-anyblock
    evil-visualstar
    finalize
    flx
    flycheck
    flyspell-correct
    git-modes
    highlight-escape-sequences
    hydra
    iedit
    ivy
    ivy-hydra
    magit
    markdown-mode
    markdown-toc
    osx-dictionary
    packed
    pandoc-mode
    pdf-tools
    projectile
    py-yapf
    queue
    rainbow-delimiters
    reveal-in-osx-finder
    shackle
    shell-pop
    smartparens
    smex
    solarized-theme
    spacemacs-theme
    speck
    spinner
    swiper
    undo-tree
    use-package
    which-key
    window-numbering
    with-editor
    worf
    ws-butler
    zenburn-theme))

(setq package-selected-packages soo-packages)

;; Install required
(dolist (package soo-packages)
  (unless (package-installed-p package)
    (ignore-errors
      (package-install package))))

;; Upgrade installed
(save-window-excursion
  (package-list-packages t)
  (package-menu-mark-upgrades)
  (package-menu-execute t))
