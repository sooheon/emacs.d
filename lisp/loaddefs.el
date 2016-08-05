;;

;;;### (autoloads nil "modes/soo-python" "modes/soo-python.el" (22436
;;;;;;  11203 0 0))
;;; Generated autoloads from modes/soo-python.el

(autoload 'soo-python-hook "modes/soo-python" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("custom.el" "modes/soo-haskell.el" "packages.el")
;;;;;;  (22436 10728 0 0))

;;;***

;;;### (autoloads nil "auto" "auto.el" (22436 11394 0 0))
;;; Generated autoloads from auto.el

(autoload 'soo-move-beginning-of-line "auto" "\
Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there.

\(fn ARG)" t nil)

(autoload 'soo--close-window-dwim "auto" "\
DWIM window closing function.
If window is a *Help* window, call `quit-window' so that the
buffer will not be selected again with `other-buffer'. When there
are multiple windows in frame, call `delete-window' on current
window. If there is only one window in frame, call
`bury-buffer'.

\(fn &optional WINDOW)" t nil)

(autoload 'sooheon--toggle-right-option-key "auto" "\


\(fn)" t nil)

(autoload 'endless/fill-or-unfill "auto" "\
Like `fill-paragraph', but unfill if used twice.

\(fn)" t nil)

(autoload 'narrow-or-widen-dwim "auto" "\
Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed.

\(fn P)" t nil)

(autoload 'sooheon--delete-to-bol "auto" "\


\(fn)" t nil)

(autoload 'update-all-autoloads "auto" "\


\(fn)" t nil)

(autoload 'ora-c-forward-sexp-function "auto" "\


\(fn ARG)" nil nil)

;;;***
