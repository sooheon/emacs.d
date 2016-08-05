(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key (kbd "s-u") 'universal-argument)
(global-set-key (kbd "s-W") 'delete-frame)
(bind-key "s-k" 'kill-this-buffer)
;; (bind-key "s-K" 'kill-this-buffer)
(define-key evil-normal-state-map "zf" '(lambda () (interactive)
                                          (reposition-window)
                                          (reposition-window)))
(global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)
(set-fontset-font "fontset-default" 'hangul '("NanumGothic" . "unicode-bmp"))

;;;###autoload
(defun soo-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(bind-key "C-a" 'soo-move-beginning-of-line)

;;;###autoload
(defun soo--close-window-dwim (&optional window)
  "DWIM window closing function.
If window is a *Help* window, call `quit-window' so that the
buffer will not be selected again with `other-buffer'. When there
are multiple windows in frame, call `delete-window' on current
window. If there is only one window in frame, call
`bury-buffer'."
  (interactive)
  (let ((window (window-normalize-window window)))
    (if (eq major-mode 'help-mode)
        (call-interactively 'quit-window)
      (if (window-parent window)
          (call-interactively 'delete-window)
        (call-interactively 'bury-buffer)))))

(bind-key "s-w" 'soo--close-window-dwim)

;;;###autoload
(defun soo--terminal-pop ()
  (interactive)
  (do-applescript
   (format "
tell application \"Terminal\"
  activate
  tell application \"System Events\" to keystroke \"t\" using {command down}
  delay 0.2
  do script \"cd %s\" in window 1
end tell
"
           default-directory)))

(define-key evil-normal-state-map "got" 'soo--terminal-pop)

(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

;;;###autoload
(defun sooheon--toggle-right-option-key ()
  (interactive)
  (if (eq mac-right-option-modifier nil)
      (progn (message "Setting right option key to meta")
             (setq mac-right-option-modifier 'meta))
    (progn (message "Setting right option key to nil")
           (setq mac-right-option-modifier nil))))
(evil-leader/set-key "t\C-o" 'sooheon--toggle-right-option-key)

;;;###autoload
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(bind-key [remap fill-paragraph] 'endless/fill-or-unfill)

;;;###autoload
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if you
         ;; don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;; This line actually replaces Emacs' entire narrowing keymap, that's how
;; much I like this command. Only copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)
(add-hook 'LaTeX-mode-hook
          (lambda () (define-key LaTeX-mode-map "\C-xn" nil)))

;;;###autoload
(defun sooheon--delete-to-bol ()
  (interactive)
  (if (fboundp 'lispyville-delete)
      (lispyville-delete (line-beginning-position) (point))
    (evil-delete (line-beginning-position) (point))))

;; Other emacs-mac/OSX keybindings
(bind-key "s-s" 'save-buffer)
(bind-key "s-q" 'save-buffers-kill-terminal)
(bind-key "s-v" 'yank)
(bind-key "s-c" 'evil-yank)
(bind-key "s-a" 'mark-whole-buffer)
(bind-key "s-x" 'kill-region)
(bind-key "s-n" 'make-frame)
(bind-key "s-l" 'evil-avy-goto-line)
(bind-key "C-s-f" 'toggle-frame-fullscreen)
(evil-define-key 'insert global-map
  "\C-o" 'evil-execute-in-normal-state
  (kbd "<s-backspace>") 'sooheon--delete-to-bol)

;; (defun inc-face-height ()
;;   (interactive)
;;   (let ((new-height (+ 10 (face-attribute 'default :height))))
;;     (set-face-attribute 'default nil :height new-height)
;;     (message (format "Font height is now %d" new-height))))
;; (bind-key "s-=" 'inc-face-height)

;; (defun dec-face-height ()
;;   (interactive)
;;   (let ((new-height (- (face-attribute 'default :height) 10)))
;;     (set-face-attribute 'default nil :height new-height)
;;     (message (format "Font height is now %d" new-height))))
;; (bind-key "s--" 'dec-face-height)

;;;###autoload
(defun youtube-dl ()
  (interactive)
  (let* ((str (current-kill 0))
         (default-directory "~/Downloads")
         (proc (get-buffer-process (ansi-term "/bin/bash"))))
    (term-send-string
     proc
     (concat "cd ~/Downloads && youtube-dl " str "\n"))))

;;;###autoload
(defun update-all-autoloads ()
  (interactive)
  (cd (expand-file-name "lisp" emacs-d))
  (let ((generated-autoload-file
         (expand-file-name "loaddefs.el")))
    (when (not (file-exists-p generated-autoload-file))
      (with-current-buffer (find-file-noselect generated-autoload-file)
        (insert ";;") ;; create the file with non-zero size to appease autoload
        (save-buffer)))
    (mapcar #'update-directory-autoloads
            '("" "modes" ;; "git/org-fu"
              ))))

;;;###autoload
(defun ora-c-forward-sexp-function (arg)
  (let ((forward-sexp-function nil))
    (forward-sexp arg))
  (when (and (eq (char-after) ?.)
             (looking-back "[0-9]+" (line-beginning-position)))
    (forward-char)
    (skip-chars-forward "0-9")))

;;;###autoload
(evil-define-text-object evil-inner-$ (count &optional beg end type)
  (evil-select-paren "\\$" "\\$" beg end type count nil))

;;;###autoload
(evil-define-text-object evil-outer-$ (count &optional beg end type)
  (evil-select-paren "\\$" "\\$" beg end type count t))

(define-key evil-inner-text-objects-map "$" 'evil-inner-$)
(define-key evil-outer-text-objects-map "$" 'evil-outer-$)
(with-eval-after-load 'evil-surround
  (push '(36 "$" . "$") evil-surround-pairs-alist))

;;;###autoload
(evil-define-text-object evil-inner-* (count &optional beg end type)
  (evil-select-paren "\\*" "\\*" beg end type count nil))

;;;###autoload
(evil-define-text-object evil-outer-* (count &optional beg end type)
  (evil-select-paren "\\*" "\\*" beg end type count t))

(define-key evil-inner-text-objects-map "*" 'evil-inner-*)
(define-key evil-outer-text-objects-map "*" 'evil-outer-*)
(with-eval-after-load 'evil-surround
  (push '(42 "*" . "*") evil-surround-pairs-alist))

;;;###autoload
(evil-define-text-object evil-inner-/ (count &optional beg end type)
  (evil-select-paren "\\/" "\\/" beg end type count nil))

;;;###autoload
(evil-define-text-object evil-outer-/ (count &optional beg end type)
  (evil-select-paren "\\/" "\\/" beg end type count t))

(define-key evil-inner-text-objects-map "/" 'evil-inner-/)
(define-key evil-outer-text-objects-map "/" 'evil-outer-/)
(with-eval-after-load 'evil-surround
  (push '(47 . "/") evil-surround-pairs-alist))