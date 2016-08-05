(require 'python)
(defvar no-pip
  (string-match "Command not found\\|no pip in"
                (shell-command-to-string "which pip")))
(unless no-pip
  (use-package jedi
    :config
    (setq python-environment-directory "~/.pyenv/versions")
    (define-key jedi-mode-map [C-tab] nil)
    (setq jedi:use-shortcuts nil)
    (setq jedi:complete-on-dot nil)
    (setq jedi:setup-function nil)
    (setq jedi:mode-function nil)
    ;; (define-key jedi-mode-map (kbd "M-.") 'jedi:goto-definition)
    ;; (define-key jedi-mode-map (kbd "K") 'jedi:show-doc)
    ))

(require 'lpy)
(define-key lpy-mode-map (kbd "C-h") nil)
(define-key lpy-mode-map (kbd "[") nil)
(define-key lpy-mode-map (kbd "(") nil)
(define-key python-mode-map (kbd "C-j") 'newline-and-indent)
(define-key python-mode-map (kbd "C-m") 'newline)
(define-key python-mode-map (kbd "C-c C-z") 'ora-python-switch-to-shell)
(define-key python-mode-map (kbd "C-c C-l") 'ora-python-send)

(require 'le-python)

;;;###autoload
(defun soo-python-hook ()
  (unless no-pip
    (jedi:setup))
  (remove-hook 'post-command-hook 'jedi:handle-post-command t)
  (setq lispy-no-space t)
  (setq forward-sexp-function 'ora-c-forward-sexp-function)
  (lpy-mode)
  (setq completion-at-point-functions '(lispy-python-completion-at-point t)))

(defun ora-python-switch-to-shell ()
  (interactive)
  (let ((buffer (process-buffer (lispy--python-proc))))
    (if buffer
        (pop-to-buffer buffer)
      (run-python "python")
      (pop-to-buffer "*Python*"))))

(defun ora-python-shell-send-region (start end &optional nomain)
  "Send the region delimited by START and END to inferior Python process."
  (interactive "r")
  (let* ((python--use-fake-loc
          (not buffer-file-name))
         (string (python-shell-buffer-substring start end nomain))
         (process (python-shell-get-or-create-process))
         (_ (string-match "\\`\n*\\(.*\\)" string)))
    (let* ((temp-file-name (python-shell--save-temp-file string))
           (file-name (or (buffer-file-name) temp-file-name)))
      (python-shell-send-file file-name process temp-file-name t)
      (unless python--use-fake-loc
        (with-current-buffer (process-buffer process)
          (compilation-fake-loc (copy-marker start) temp-file-name
                                2))))))

(defun ora-python-send ()
  (interactive)
  (if (region-active-p)
      (ora-python-shell-send-region (region-beginning)
                                    (region-end))
    (ora-python-shell-send-region (point-min)
                                  (point-max))))

(use-package pyenv-mode
  :disabled t
  :if (executable-find "pyenv")
  :commands (pyenv-mode-set pyenv-mode-unset)
  :init
  (require 'pyenv-mode-auto))

(use-package company-jedi
  :disabled t
  :init
  (add-hook 'python-mode-hook
            '(lambda () (add-to-list 'company-backends 'company-jedi))))

(use-package anaconda-mode
  :disabled t
  :defer t
  :diminish anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (setq anaconda-mode-installation-directory (expand-file-name "etc/anaconda-mode" emacs-d))
  (add-hook 'python-mode-hook 'anaconda-mode)
  :config
  (evil-define-key 'normal anaconda-mode-map "K" 'anaconda-mode-show-doc)
  (evilified-state-evilify anaconda-mode-view-mode anaconda-mode-view-mode-map
    (kbd "q") 'quit-window)
  (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
    (evil--jumps-push))
  (require 'company-anaconda)
  (add-to-list 'company-backends 'company-anaconda))

(use-package py-yapf
  :commands py-yapf-buffer
  :init
  (evil-leader/set-key-for-mode 'python-mode "=" 'py-yapf-buffer))
