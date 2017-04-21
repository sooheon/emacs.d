;;;###autoload
(defun smart-move-beginning-of-line (arg)
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

;;;###autoload
(defun soo--close-window-dwim (&optional window)
  "DWIM window closing function.
If window is a *Help* window, call `quit-window' so that the
buffer will not be selected again with `other-buffer'. Do the
same for *terminal* buffers, also deleting window if term is run
in split. Otherwise, when there are split windows in frame,
call `delete-window' on current window. If there is only one
window in frame, call `bury-buffer'."
  (interactive)
  (let ((window (window-normalize-window window)))
    (cond
     ((eq major-mode 'help-mode) (call-interactively 'quit-window))
     ((eq major-mode 'term-mode)
      (call-interactively 'quit-window)
      (when (window-parent window)
        (call-interactively 'delete-window)))
     (t (progn
          (call-interactively 'bury-buffer)
          (when (window-parent window)
            (call-interactively 'delete-window)))))))

;;;###autoload
(defun soo-terminal-focus ()
  (interactive)
  (do-applescript
   "do shell script \"open -a iTerm\"\n"))

;;;###autoload
(defun soo-terminal-pop ()
  "If iTerm is not open, launch it. If iTerm session is busy, split
off a new pane. Switch to iTerm and cd to default-directory."
  (interactive)
  (do-applescript
   (format "
tell application \"iTerm\"
  activate
  try
    set w to current window
  on error
    set w to (make new window)
  end try

  tell w
    if (is at shell prompt of the current session) then
      tell the current session to write text \"cd \'%s\'\"
    else
      tell current session to split horizontally with default profile
      tell application \"System Events\" to keystroke \"]\" using command down
      tell the current session to write text \"cd \'%s\'\"
    end if
  end tell
end tell"
           (expand-file-name (or default-directory "~"))
           (expand-file-name (or default-directory "~")))))

;;;###autoload
(defun soo-terminal-pop-tmux ()
  (interactive)
  (do-applescript
   (format "
tell application \"iTerm\"
  activate
  tell application \"System Events\" to keystroke \"b\" using control down
  tell application \"System Events\" to keystroke \"\\\"\"
  tell current session of current window to write text \"cd %s\"
end tell"
           (or default-directory "~"))))

;;;###autoload
(defun soo-terminal-pop-new-tab ()
  (interactive)
  (do-applescript
   (format "
tell application \"iTerm\"
  activate
  tell the current session of current window
    if (is processing) then
      create tab with default profile
      write text \"cd %s\"
    else
      write text \"pushd %s\"
    end if
  end tell
end tell
"
           (or default-directory "~")
           (or default-directory "~"))))

;;;###autoload
(defun sooheon--toggle-right-option-key ()
  (interactive)
  (if (eq mac-right-option-modifier nil)
      (progn (message "Setting right option key to meta")
             (setq mac-right-option-modifier 'meta))
    (progn (message "Setting right option key to nil")
           (setq mac-right-option-modifier nil))))

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

;;;###autoload
(defun sooheon--delete-to-bol (arg)
  (interactive "p")
  (if (bound-and-true-p lispyville-mode)
      (lispyville--safe-manipulate (line-beginning-position) (point) t)
    (kill-line (- 1 arg)))
  (indent-for-tab-command))

;;;###autoload
(defun inc-face-height ()
  (interactive)
  (let ((new-height (+ 10 (face-attribute 'default :height))))
    (set-face-attribute 'default nil :height new-height)
    (message (format "Font height is now %d" new-height))))

;;;###autoload
(defun dec-face-height ()
  (interactive)
  (let ((new-height (- (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height new-height)
    (message (format "Font height is now %d" new-height))))

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
  (cd (concat user-emacs-directory "lisp/"))
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
(defun vinegar/dired-diff ()
    "Ediff marked files in dired or selected files in separate window"
    (interactive)
    (let* ((marked-files (dired-get-marked-files nil nil))
           (other-win (get-window-with-predicate
                       (lambda (window)
                         (with-current-buffer (window-buffer window)
                           (and (not (eq window (selected-window)))
                                (eq major-mode 'dired-mode))))))
           (other-marked-files (and other-win
                                    (with-current-buffer (window-buffer other-win)
                                      (dired-get-marked-files nil)))))
      (cond ((= (length marked-files) 2)
             (ediff-files (nth 0 marked-files)
                          (nth 1 marked-files)))
            ((= (length marked-files) 3)
             (ediff-files3 (nth 0 marked-files)
                           (nth 1 marked-files)
                           (nth 2 marked-files)))
            ((and (= (length marked-files) 1)
                  (= (length other-marked-files) 1))
             (ediff-files (nth 0 marked-files)
                          (nth 0 other-marked-files)))
            ((= (length marked-files) 1)
             (call-interactively 'dired-diff))
            (t (error "Mark exactly 2 files, at least 1 locally")))))

;; https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
;;;###autoload
(defun Fuco1/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

;;;###autoload
(defun vinegar/dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (message "h")
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer)            ; otherwise just revert to re-show
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))

;;;###autoload
(defun my-find-huge-file-literally-hook ()
  "If a file is over a given size, make the buffer fundamental."
  (when (> (buffer-size) (* 1024 1024))
    ;; (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

;;;###autoload
(defun soo-run-prog-mode-hook ()
  "Runs `prog-mode-hook'. Useful for modes that don't derive from
`prog-mode' but should."
  (run-hooks 'prog-mode-hook))

;;;###autoload
(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the reverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

;;;###autoload
(defun ora-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))
