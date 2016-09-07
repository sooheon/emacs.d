(defconst emacs-d (file-name-directory (or load-file-name buffer-file-name))
  "The giant turtle on whose shell the world rests.")
(setq package-user-dir (expand-file-name "elpa" emacs-d))
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
(package-refresh-contents)

(defconst soo-packages
  '(ace-link
    anaconda-mode
    auto-compile
    avy
    auctex
    bind-map
    cargo
    cdlatex
    cider
    circe
    clojure-mode
    clj-refactor
    closql
    company
    company-anaconda
    counsel
    counsel-projectile
    dash
    diff-hl
    diminish
    elisp-slime-nav
    evil
    evil-commentary
    evil-cleverparens
    evil-exchange
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
    general
    git-modes
    gist
    highlight-escape-sequences
    hindent
    hydra
    jedi
    lispy
    lispyville
    iedit
    inf-clojure
    intero
    ivy
    ivy-hydra
    magit
    markdown-mode
    ;; multiple-cursors
    ;; org-plus-contrib
    osx-dictionary
    pandoc-mode
    pdf-tools
    projectile
    py-yapf
    pyenv-mode
    pyenv-mode-auto
    queue
    racer
    rainbow-delimiters
    rainbow-mode
    restclient
    reveal-in-osx-finder
    rust-mode
    sentence-navigation
    shackle
    shell-pop
    shm
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
    zenburn-theme
    atom-one-dark-theme))

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
