(require 'pinentry)
(require 'epa)

(defvar have-private-key
  (= (string-to-number
      (shell-command-to-string "gpg2 --list-secret-keys | grep C969712A8DD4C0141241406EDC0A92F02AC815CE | wc -l"))
     1))

(defun load-gpg (file)
  (if have-private-key
      (load file)
    (message "WARNING: Couldn't load %s (No gpg key found)" file)))

(defun read-secrets ()
  (let ((secrets-file (expand-file-name "lisp/secrets.el.gpg" user-emacs-directory)))
    (if (not have-private-key)
        (message "ERROR: Private GPG key not found")
      (start-process "gpg-agent" nil "gpg-agent" "--daemon")
      (setq password-cache-expiry nil
            pinentry--socket-dir temporary-file-directory)
      (unless (file-exists-p (concat pinentry--socket-dir "pinentry"))
        (pinentry-start)
        (add-hook 'kill-emacs-hook 'pinentry-stop))
      (add-to-list 'load-suffixes ".el.gpg")
      (load-gpg "secrets"))))
