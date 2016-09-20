(defconst emacs-d (file-name-directory (or load-file-name buffer-file-name))
  "The giant turtle on whose shell the world rests.")
(setq package-user-dir (expand-file-name "elpa" emacs-d))
(setq package-archives '(("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(package-refresh-contents)

(defconst soo-packages
  '(evil
    use-package
    general))

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
