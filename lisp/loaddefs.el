;;

;;;### (autoloads nil "modes/soo-clojure" "modes/soo-clojure.el"
;;;;;;  (22578 36043 0 0))
;;; Generated autoloads from modes/soo-clojure.el

(autoload 'soo-clojure-hook "modes/soo-clojure" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "modes/soo-haskell" "modes/soo-haskell.el"
;;;;;;  (22479 41727 0 0))
;;; Generated autoloads from modes/soo-haskell.el

(autoload 'soo-haskell-hook "modes/soo-haskell" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "modes/soo-org" "modes/soo-org.el" (22555 34806
;;;;;;  0 0))
;;; Generated autoloads from modes/soo-org.el

(autoload 'soo-org-hook "modes/soo-org" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "modes/soo-python" "modes/soo-python.el" (22532
;;;;;;  62137 0 0))
;;; Generated autoloads from modes/soo-python.el

(autoload 'soo-python-hook "modes/soo-python" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("custom.el" "keybinds.el" "modes/soo-ess.el"
;;;;;;  "modes/soo-ivy.el" "modes/soo-rust.el" "packages.el") (22578
;;;;;;  42892 0 0))

;;;***

;;;### (autoloads nil "auto" "auto.el" (22578 47848 0 0))
;;; Generated autoloads from auto.el

(autoload 'smart-move-beginning-of-line "auto" "\
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
buffer will not be selected again with `other-buffer'. Do the
same for *terminal* buffers, also deleting window if term is run
in split. Otherwise, when there are split windows in frame,
call `delete-window' on current window. If there is only one
window in frame, call `bury-buffer'.

\(fn &optional WINDOW)" t nil)

(autoload 'soo-terminal-focus "auto" "\


\(fn)" t nil)

(autoload 'soo-terminal-pop "auto" "\


\(fn)" t nil)

(autoload 'soo-terminal-pop-project-root "auto" "\


\(fn)" t nil)

(autoload 'soo-terminal-pop-new-tab "auto" "\


\(fn)" t nil)

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

(autoload 'inc-face-height "auto" "\


\(fn)" t nil)

(autoload 'dec-face-height "auto" "\


\(fn)" t nil)

(autoload 'youtube-dl "auto" "\


\(fn)" t nil)

(autoload 'update-all-autoloads "auto" "\


\(fn)" t nil)

(autoload 'ora-c-forward-sexp-function "auto" "\


\(fn ARG)" nil nil)

(autoload 'vinegar/dired-diff "auto" "\
Ediff marked files in dired or selected files in separate window

\(fn)" t nil)

(autoload 'Fuco1/lisp-indent-function "auto" "\
This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  (this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation.

\(fn INDENT-POINT STATE)" nil nil)

(autoload 'vinegar/dotfiles-toggle "auto" "\
Show/hide dot-files

\(fn)" t nil)

(autoload 'my-find-huge-file-literally-hook "auto" "\
If a file is over a given size, make the buffer fundamental.

\(fn)" nil nil)

(autoload 'soo-run-prog-mode-hook "auto" "\
Runs `prog-mode-hook'. Useful for modes that don't derive from
`prog-mode' but should.

\(fn)" nil nil)

;;;***
