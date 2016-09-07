(require 'python)
(csetq python-shell-completion-native-enable nil)
(add-hook 'inferior-python-mode-hook 'company-mode)
(add-hook 'inferior-python-mode-hook 'smartparens-mode)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i" ; http://tinyurl.com/j44y64d
      )
(defun py-yapf-and-send-buffer ()
  (interactive)
  (py-yapf-buffer)
  (python-shell-send-buffer))
(bind-key "C-c C-c" 'py-yapf-and-send-buffer python-mode-map)

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
(diminish 'lpy-mode "lpy")
(define-key lpy-mode-map (kbd "C-h") nil)
(define-key lpy-mode-map (kbd "[") nil)
(define-key lpy-mode-map (kbd "(") nil)
(define-key lpy-mode-map (kbd "=") nil)
(define-key lpy-mode-map (kbd ">") nil)
(define-key lpy-mode-map (kbd "<") nil)
(define-key lpy-mode-map (kbd ",") nil)
(define-key python-mode-map (kbd "C-j") 'newline-and-indent)
(define-key python-mode-map (kbd "C-m") 'newline)
;; (define-key python-mode-map (kbd "C-c C-z") 'ora-python-switch-to-shell)
;; (define-key python-mode-map (kbd "C-c C-l") 'ora-python-send)

(require 'le-python)

(use-package anaconda-mode
  :diminish anaconda-mode
  :config
  (evil-define-key 'normal anaconda-mode-map "K" 'anaconda-mode-show-doc)
  (evil-set-initial-state 'anaconda-mode-view-mode 'insert)
  (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
    (evil--jumps-push))
  (require 'company-anaconda)
  (add-to-list 'company-backends 'company-anaconda))

;;;###autoload
(defun soo-python-hook ()
  ;; (unless no-pip (jedi:setup))
  ;; (remove-hook 'post-command-hook 'jedi:handle-post-command t)
  (setq lispy-no-space t)
  (setq forward-sexp-function 'ora-c-forward-sexp-function)
  (lpy-mode 1)
  (anaconda-mode 1)
  (anaconda-eldoc-mode 1))

(defun ora-python-switch-to-shell ()
  "If *Python* is running, switch to it. Else, run new python
buffer."
  (interactive)
  (let ((buffer (process-buffer (lispy--python-proc))))
    (if buffer
        (pop-to-buffer buffer)
      (run-python)
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
  "If region is active, send region to REPL. Else, send entire
buffer."
  (interactive)
  (if (region-active-p)
      (ora-python-shell-send-region (region-beginning)
                                    (region-end))
    (ora-python-shell-send-region (point-min)
                                  (point-max))))

;; (use-package pyenv-mode
;;   :disabled t
;;   :if (executable-find "pyenv")
;;   :commands (pyenv-mode-set pyenv-mode-unset)
;;   :init
;;   (require 'pyenv-mode-auto))

;; (use-package company-jedi
;;   :disabled t
;;   :init
;;   (add-hook 'python-mode-hook
;;             '(lambda () (add-to-list 'company-backends 'company-jedi))))

(use-package py-yapf
  :commands py-yapf-buffer
  :init
  (nmap :prefix "SPC"
        :keymaps 'python-mode-map
        "=" 'py-yapf-buffer))
