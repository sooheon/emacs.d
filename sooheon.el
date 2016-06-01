(setq scroll-preserve-screen-position t
      scroll-margin 2
      scroll-conservatively 101)
(blink-cursor-mode -1)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key (kbd "s-u") 'universal-argument)
(global-set-key (kbd "s-W") 'delete-frame)
(bind-key "s-k" 'bury-buffer)
(bind-key "s-K" 'kill-this-buffer)
(define-key evil-normal-state-map "zf" '(lambda () (interactive)
                                          (reposition-window)
                                          (reposition-window)))
(global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)
(set-fontset-font "fontset-default" 'hangul '("NanumGothic" . "unicode-bmp"))
(setq-default fringe-indicator-alist '((truncation left-arrow right-arrow)
                                       (continuation
                                        nil ;; left-curly-arrow
                                        right-curly-arrow)
                                       (overlay-arrow . right-triangle)
                                       (up . up-arrow)
                                       (down . down-arrow)
                                       (top top-left-angle top-right-angle)
                                       (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
                                       (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
                                       (empty-line . empty-line)
                                       (unknown . question-mark)))

(defun spacemacs/smart-move-beginning-of-line (arg)
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

(bind-key "C-a" #'spacemacs/smart-move-beginning-of-line)

(defun soo--delete-window-or-bury-buffer (&optional window)
  (interactive)
  (let ((window (window-normalize-window window)))
    (if (not (window-parent window))
        (call-interactively 'bury-buffer)
      (call-interactively 'delete-window))))

(bind-key "s-w" #'soo--delete-window-or-bury-buffer)

(defun soo--terminal-pop ()
  (interactive)
  (do-applescript
   (format "
tell application \"Terminal\"
  activate
  tell application \"System Events\" to keystroke \"t\" using {command down}
  delay 0.2
  do script with command \"cd %s\" in window 1
end tell
"
           default-directory)))

(define-key evil-normal-state-map "got" #'soo--terminal-pop)

(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

;; Right alt nil for deadkeys
(defun sooheon--toggle-right-option-key ()
  (interactive)
  (if (eq mac-right-option-modifier nil)
      (progn (message "Setting right option key to meta")
             (setq mac-right-option-modifier 'meta))
    (progn (message "Setting right option key to nil")
           (setq mac-right-option-modifier nil))))
(evil-leader/set-key
  "t\C-o" 'sooheon--toggle-right-option-key)

;; fill/un-fill with one key: http://tinyurl.com/gn2tswy
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))
(bind-key [remap fill-paragraph] #'endless/fill-or-unfill)

;; Narrow or widen dwim
;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
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

;; Deleting with S-backspace work with cleverparens
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
