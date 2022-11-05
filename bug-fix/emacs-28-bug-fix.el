
(when (= emacs-major-version 28)
  ;; Fix for error "Error: Wrong number of arguments: (3 . 4), 2"
  ;;
  ;; https://www.reddit.com/r/emacs/comments/kqb9s9/comment/gj3cfn3/?utm_source=share&utm_medium=web2x&context=3
  ;;
  ;; https://github.com/syl20bnr/spacemacs/issues/14631
  ;; https://gist.github.com/hlissner/175c21000114d1dba7a47678191a888a

  (defun make-obsolete (obsolete-name current-name &optional when)
    "Make the byte-compiler warn that function OBSOLETE-NAME is obsolete.
OBSOLETE-NAME should be a function name or macro name (a symbol).

The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message
\(it should end with a period, and not start with a capital).
WHEN should be a string indicating when the function
was first made obsolete, for example a date or a release number."
    (declare (advertised-calling-convention
              ;; New code should always provide the `when' argument.
              (obsolete-name current-name when) "23.1"))
    (put obsolete-name 'byte-obsolete-info
         ;; The second entry used to hold the `byte-compile' handler, but
         ;; is not used any more nowadays.
         (purecopy (list current-name nil when)))
    obsolete-name)

  (defmacro define-obsolete-function-alias (obsolete-name current-name
                                                          &optional when docstring)
    "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.

\(define-obsolete-function-alias \\='old-fun \\='new-fun \"22.1\" \"old-fun's doc.\")

is equivalent to the following two lines of code:

\(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
\(make-obsolete \\='old-fun \\='new-fun \"22.1\")

WHEN should be a string indicating when the function was first
made obsolete, for example a date or a release number.

See the docstrings of `defalias' and `make-obsolete' for more details."
    (declare (doc-string 4)
             (advertised-calling-convention
              ;; New code should always provide the `when' argument.
              (obsolete-name current-name when &optional docstring) "23.1"))
    `(progn
       (defalias ,obsolete-name ,current-name ,docstring)
       (make-obsolete ,obsolete-name ,current-name ,when)))

  (defun make-obsolete-variable (obsolete-name current-name &optional when access-type)
    "Make the byte-compiler warn that OBSOLETE-NAME is obsolete.
The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message.
WHEN should be a string indicating when the variable
was first made obsolete, for example a date or a release number.
ACCESS-TYPE if non-nil should specify the kind of access that will trigger
  obsolescence warnings; it can be either `get' or `set'."
    (declare (advertised-calling-convention
              ;; New code should always provide the `when' argument.
              (obsolete-name current-name when &optional access-type) "23.1"))
    (put obsolete-name 'byte-obsolete-variable
         (purecopy (list current-name access-type when)))
    obsolete-name)

  (defmacro define-obsolete-variable-alias (obsolete-name current-name
						          &optional when docstring)
    "Make OBSOLETE-NAME a variable alias for CURRENT-NAME and mark it obsolete.
This uses `defvaralias' and `make-obsolete-variable' (which see).
See the Info node `(elisp)Variable Aliases' for more details.

If CURRENT-NAME is a defcustom or a defvar (more generally, any variable
where OBSOLETE-NAME may be set, e.g. in an init file, before the
alias is defined), then the define-obsolete-variable-alias
statement should be evaluated before the defcustom, if user
customizations are to be respected.  The simplest way to achieve
this is to place the alias statement before the defcustom (this
is not necessary for aliases that are autoloaded, or in files
dumped with Emacs).  This is so that any user customizations are
applied before the defcustom tries to initialize the
variable (this is due to the way `defvaralias' works).

WHEN should be a string indicating when the variable was first
made obsolete, for example a date or a release number.

For the benefit of Customize, if OBSOLETE-NAME has
any of the following properties, they are copied to
CURRENT-NAME, if it does not already have them:
`saved-value', `saved-variable-comment'."
    (declare (doc-string 4)
             (advertised-calling-convention
              ;; New code should always provide the `when' argument.
              (obsolete-name current-name when &optional docstring) "23.1"))
    `(progn
       (defvaralias ,obsolete-name ,current-name ,docstring)
       ;; See Bug#4706.
       (dolist (prop '(saved-value saved-variable-comment))
         (and (get ,obsolete-name prop)
              (null (get ,current-name prop))
              (put ,current-name prop (get ,obsolete-name prop))))
       (make-obsolete-variable ,obsolete-name ,current-name ,when)))

  (defun start-process-shell-command (name buffer &rest args)
    "Start a program in a subprocess.  Return the process object for it.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
COMMAND is the shell command to run.

An old calling convention accepted any number of arguments after COMMAND,
which were just concatenated to COMMAND.  This is still supported but strongly
discouraged."
    (declare (advertised-calling-convention (name buffer command) "23.1"))
    ;; We used to use `exec' to replace the shell with the command,
    ;; but that failed to handle (...) and semicolon, etc.
    (start-process name buffer shell-file-name shell-command-switch
		   (mapconcat 'identity args " "))))
