(defconst emacs-d (file-name-directory (or load-file-name buffer-file-name))
  "The giant turtle on whose shell the world rests.")
(setq package-user-dir (expand-file-name "elpa" emacs-d))
(package-initialize)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-refresh-contents)

(defconst soo-packages
  '(ace-link
    anaconda-mode
    auto-compile
    avy
    bind-map
    cider
    circe
    clojure-mode
    closql
    company
    company-anaconda
    counsel
    counsel-projectile
    dash
    diff-hl
    diminish
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
    expand-region
    finalize
    flx
    flycheck
    flyspell-correct
    function-args
    git-modes
    gist
    highlight-escape-sequences
    hydra
    jedi
    lispy
    lispyville
    iedit
    inf-clojure
    ivy
    ivy-hydra
    magit
    markdown-mode
    multiple-cursors
    org-download
    osx-dictionary
    pandoc-mode
    pdf-tools
    projectile
    py-yapf
    pyenv-mode
    pyenv-mode-auto
    queue
    rainbow-delimiters
    rainbow-mode
    reveal-in-osx-finder
    sentence-navigation
    shackle
    shell-pop
    smartparens
    smex
    solarized-theme
    spacemacs-theme
    speck
    super-save
    swiper
    term-manager
    typo
    undo-tree
    use-package
    which-key
    window-numbering
    with-editor
    worf
    ws-butler
    zenburn-theme))

;; install required
(dolist (package soo-packages)
  (unless (package-installed-p package)
    (ignore-errors
      (package-install package))))

;; upgrade installed
(save-window-excursion
  (package-list-packages t)
  (package-menu-mark-upgrades)
  (condition-case nil
      (package-menu-execute t)
    (error
     (package-menu-execute))))
