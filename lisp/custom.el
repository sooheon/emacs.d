(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-indent-level 0)
 '(LaTeX-left-right-indent-level 0)
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(ansi-term-color-vector
   [unspecified "#090300" "#db2d20" "#01a252" "#fded02" "#01a0e4" "#a16a94" "#01a0e4" "#a5a2a2"] t)
 '(auto-image-file-mode t)
 '(cider-cljs-boot-repl
   "(do (require 'adzerk.boot-cljs-repl) (adzerk.boot-cljs-repl/start-repl))")
 '(cider-cljs-lein-repl
   "(do (require 'figwheel-sidecar.repl-api) (figwheel-sidecar.repl-api/start-figwheel!) (figwheel-sidecar.repl-api/cljs-repl))")
 '(cider-preferred-build-tool "boot")
 '(cider-repl-display-help-banner nil)
 '(cider-save-file-on-load (quote t))
 '(cljr-clojure-test-namespace-under-test-alias "")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" "cdbd0a803de328a4986659d799659939d13ec01da1f482d838b68038c1bb35e8" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(debug-on-error nil)
 '(dired-dwim-target t)
 '(eldoc-idle-delay 0)
 '(epa-pinentry-mode (quote loopback))
 '(evil-disable-insert-state-bindings t)
 '(evil-lookup-func (quote man))
 '(evil-magit-want-horizontal-movement nil)
 '(evil-want-C-w-delete t)
 '(eyebrowse-mode nil)
 '(eyebrowse-switch-back-and-forth nil)
 '(eyebrowse-wrap-around t)
 '(fci-rule-color "#383838")
 '(frame-resize-pixelwise t)
 '(global-hl-line-mode nil)
 '(help-window-select t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(lispy-eval-display-style (quote message))
 '(magit-branch-arguments nil)
 '(magit-diff-use-overlays nil)
 '(mark-even-if-inactive t)
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(ns-right-alternate-modifier nil)
 '(org-download-method (quote directory))
 '(org-indent-indentation-per-level 1)
 '(org-latex-classes
   (quote
    (("letter" "\\documentclass[14pt]{letter}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
 '(org-latex-create-formula-image-program (quote imagemagick))
 '(org-pomodoro-format "%s")
 '(org-pomodoro-long-break-format "%s")
 '(org-pomodoro-short-break-format "%s")
 '(org-preview-latex-default-process (quote dvisvgm))
 '(package-selected-packages
   (quote
    (simple elisp-mode lispy org-plus-contrib parinfer undo-tree helm ob-clojure link-hint persp-mode window-purpose yapfify frame company-restclient restclient restclient-mode eyebrowse org-board terminal-here company-sourcekit swift-mode smart-mode-line paradox async hy-mode vim-empty-lines ein circe-notifications vlf hl-todo base16-theme farmhouse-theme yaml-mode doom-themes ace-window rainbow-mode org-download htmlize rust-mode haskell-mode ess-smart-equals ess ess-site lpy tex cdlatex texmathp magithub suggest wgrep general hungry-delete hungry-delete-mode toml-mode soap atom-one-dark-theme ggtags cpputils-cmake irony clang-format company-statistics osx-trash help evil-cleverparens-text-objects dired racer cargo golden-ratio jade yasnippet artbollocks-mode company-quickhelp evil-exchange clj-refactor with-editor swiper smartparens queue pyenv-mode projectile magit ivy hydra flycheck finalize evil diminish dash counsel company closql clojure-mode avy anaconda-mode ace-link lispyville auctex exec-path-from-shell evil-mc git-modes function-args zenburn-theme ws-butler worf window-numbering which-key use-package typo term-manager super-save speck spacemacs-theme solarized-theme smex shm shell-pop shackle sentence-navigation reveal-in-osx-finder rainbow-delimiters pyenv-mode-auto py-yapf pdf-tools pandoc-mode osx-dictionary multiple-cursors markdown-mode jedi ivy-hydra intero inf-clojure hindent highlight-escape-sequences gist flyspell-correct flx expand-region evil-visualstar evil-textobj-anyblock evil-surround evil-snipe evil-numbers evil-multiedit evil-matchit evil-magit evil-commentary epkg elisp-slime-nav diff-hl counsel-projectile company-anaconda circe cider bind-map auto-compile)))
 '(paradox-automatically-star t)
 '(paradox-github-token t)
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "ltximg")))
 '(purpose-user-mode-purposes
   (quote
    ((prog-mode . edit)
     (text-mode . edit)
     (comint-mode . terminal)
     (occur-mode . search)
     (grep-mode . search)
     (compilation-mode . search)
     (package-menu-mode . package)
     (paradox-menu-mode . package))))
 '(purpose-user-name-purposes (quote (("\".gitignore\"" . edit))))
 '(purpose-user-regexp-purposes (quote (("\"^ \\\\*Minibuf-[0-9]*\\\\*$\" " . minibuf))))
 '(rainbow-identifiers-choose-face-function (quote rainbow-identifiers-cie-l*a*b*-choose-face))
 '(rainbow-identifiers-cie-l*a*b*-color-count 1024)
 '(rainbow-identifiers-cie-l*a*b*-lightness 80)
 '(rainbow-identifiers-cie-l*a*b*-saturation 25)
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values
   (quote
    ((eval progn
           (pp-buffer)
           (indent-buffer))
     (projectile-project-compilation-cmd . "bear make")
     (projectile-project-run-cmd . "make run")
     (cider-boot-parameters . "repl -s -H :: wait dev")
     (cider-boot-parameters . "repl -s H :: wait dev")
     (outline-minor-mode)
     (whitespace-style face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark)
     (checkdoc-package-keywords-flag))))
 '(scroll-bar-mode nil)
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#657b83" 0.2))
 '(sml/replacer-regexp-list
   (quote
    (("^*/code/" ":code:")
     ("^~/Dropbox/QxBranch/" ":Qx:")
     ("^~/Dropbox/[Ww]ork/" ":Work:")
     ("^~/org/" ":Org:")
     ("^~/\\.emacs\\.d/elpa/" ":ELPA:")
     ("^~/\\.emacs\\.d/" ":ED:")
     ("^/sudo:.*:" ":SU:")
     ("^~/Documents/" ":Doc:")
     ("^~/Dropbox/" ":DB:")
     ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
     ("^~/[Gg]it/" ":Git:")
     ("^~/[Gg]it[Hh]ub/" ":Git:")
     ("^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")
     ("^:DB:/code" ":code:"))))
 '(solarized-distinct-doc-face t)
 '(solarized-high-contrast-mode-line t)
 '(solarized-use-more-italic t)
 '(solarized-use-variable-pitch nil)
 '(sp-highlight-wrap-overlay nil)
 '(sp-highlight-wrap-tag-overlay nil)
 '(sp-lisp-modes
   (quote
    (cider-clojure-interaction-mode cider-repl-mode clojure-mode clojurec-mode clojurescript-mode clojurex-mode common-lisp-mode emacs-lisp-mode eshell-mode geiser-repl-mode inf-clojure-mode inferior-emacs-lisp-mode inferior-lisp-mode inferior-scheme-mode lisp-interaction-mode lisp-mode monroe-mode racket-mode racket-repl-mode scheme-interaction-mode scheme-mode slime-repl-mode stumpwm-mode hy-mode)))
 '(speck-delay 0.5)
 '(speck-hunspell-dictionary-alist (quote (("en" . "en_US"))))
 '(speck-hunspell-minimum-word-length 3)
 '(speck-mode-keys
   (quote
    ([ignore]
     [ignore]
     [67108923]
     [201326651]
     [67108907]
     [201326635]
     [67108897]
     [201326625]
     [67108927]
     [201326655])))
 '(speck-replace-keys
   (quote
    ((help .
           [67108927])
     (help . "")
     (help .
           [f1])
     (help .
           [help])
     (accept .
             [67108897])
     (accept-and-quit .
                      [67108910])
     (reject-and-quit .
                      [67108909])
     (reject-and-quit . "")
     (reject-and-quit . "")
     (reject-and-quit .
                      [escape])
     (forward .
              [67108923])
     (backward .
               [201326651]))))
 '(speck-replace-preserve-point (quote after))
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(which-key-show-prefix (quote bottom))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(persp-face-lighter-buffer-not-in-persp ((t (:background "gray90" :foreground "#F00"))))
 '(speck-mouse ((t (:background "thistle" :foreground "black"))))
 '(speck-query ((t (:background "yellow" :foreground "black")))))
