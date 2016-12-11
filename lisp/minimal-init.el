(defvar emacs-d user-emacs-directory)
(setq no-littering-etc-directory (expand-file-name ".etc/" emacs-d)
      no-littering-var-directory (expand-file-name ".var/" emacs-d))
(add-to-list 'load-path (expand-file-name "lib/no-littering" emacs-d))
(require 'no-littering)

(package-initialize)
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(setq mac-pass-command-to-system nil
      mac-right-command-modifier 'control
      mac-command-modifier 'super
      mac-option-modifier 'meta)
