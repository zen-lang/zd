;;; zd-mode.el --- Major mode for Zd code -*- lexical-binding: t; -*-
(eval-when-compile
  (defvar calculate-lisp-indent-last-sexp)
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (defvar paredit-space-for-delimiter-predicates)
  (defvar paredit-version)
  (defvar paredit-mode))

(require 'cl-lib)
(require 'imenu)
(require 'newcomment)
(require 'align)
(require 'subr-x)
(require 'lisp-mnt)

(declare-function lisp-fill-paragraph  "lisp-mode" (&optional justify))

(defgroup zd nil
  "Major mode for editing Zd code."
  :prefix "zd-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/zd-emacs/zd-mode")
  :link '(emacs-commentary-link :tag "Commentary" "zd-mode"))

(defconst zd-mode-version
  (eval-when-compile
    (lm-version (or load-file-name buffer-file-name)))
  "The current version of `zd-mode'.")

(defface zd-keyword-face
  '((t (:inherit font-lock-constant-face)))
  "Face used to font-lock Zd keywords (:something)."
  :package-version '(zd-mode . "3.0.0"))

(defface zd-character-face
  '((t (:inherit font-lock-string-face)))
  "Face used to font-lock Zd character literals."
  :package-version '(zd-mode . "3.0.0"))

(defcustom zd-indent-style 'always-align
  "Indentation style to use for function forms and macro forms.
There are two cases of interest configured by this variable.

- Case (A) is when at least one function argument is on the same
  line as the function name.
- Case (B) is the opposite (no arguments are on the same line as
  the function name).  Note that the body of macros is not
  affected by this variable, it is always indented by
  `lisp-body-indent' (default 2) spaces.

Note that this variable configures the indentation of function
forms (and function-like macros), it does not affect macros that
already use special indentation rules.

The possible values for this variable are keywords indicating how
to indent function forms.

    `always-align' - Follow the same rules as `lisp-mode'.  All
    args are vertically aligned with the first arg in case (A),
    and vertically aligned with the function name in case (B).
    For instance:
        (reduce merge
                some-coll)
        (reduce
         merge
         some-coll)

    `always-indent' - All args are indented like a macro body.
        (reduce merge
          some-coll)
        (reduce
          merge
          some-coll)

    `align-arguments' - Case (A) is indented like `lisp', and
    case (B) is indented like a macro body.
        (reduce merge
                some-coll)
        (reduce
          merge
          some-coll)"
  :safe #'symbolp
  :type '(choice (const :tag "Same as `lisp-mode'" 'always-align)
                 (const :tag "Indent like a macro body" 'always-indent)
                 (const :tag "Indent like a macro body unless first arg is on the same line"
                        'align-arguments))
  :package-version '(zd-mode . "5.2.0"))

(defcustom zd-use-backtracking-indent t
  "When non-nil, enable context sensitive indentation."
  :type 'boolean
  :safe 'booleanp)

(defcustom zd-max-backtracking 3
  "Maximum amount to backtrack up a list to check for context."
  :type 'integer
  :safe 'integerp)

(defcustom zd-docstring-fill-column fill-column
  "Value of `fill-column' to use when filling a docstring."
  :type 'integer
  :safe 'integerp)

(defcustom zd-docstring-fill-prefix-width 2
  "Width of `fill-prefix' when filling a docstring.
The default value conforms with the de facto convention for
Zd docstrings, aligning the second line with the opening
double quotes on the third column."
  :type 'integer
  :safe 'integerp)

(defcustom zd-omit-space-between-tag-and-delimiters '(?\[ ?\{ ?\()
  "Allowed opening delimiter characters after a reader literal tag.
For example, \[ is allowed in :db/id[:db.part/user]."
  :type '(set (const :tag "[" ?\[)
              (const :tag "{" ?\{)
              (const :tag "(" ?\()
              (const :tag "\"" ?\"))
  :safe (lambda (value)
          (and (listp value)
               (cl-every 'characterp value))))

(defcustom zd-build-tool-files
  '("project.zd"      ; Leiningen
    "build.boot"       ; Boot
    "build.gradle"     ; Gradle
    "build.gradle.kts" ; Gradle
    "deps.edn"         ; Zd CLI (a.k.a. tools.deps)
    "shadow-zds.edn"  ; shadow-zds
    )
  "A list of files, which identify a Zd project's root.
Out-of-the box `zd-mode' understands lein, boot, gradle,
 shadow-zds and tools.deps."
  :type '(repeat string)
  :package-version '(zd-mode . "5.0.0")
  :safe (lambda (value)
          (and (listp value)
               (cl-every 'stringp value))))

(defcustom zd-project-root-function #'zd-project-root-path
  "Function to locate zd project root directory."
  :type 'function
  :risky t
  :package-version '(zd-mode . "5.7.0"))

(defcustom zd-refactor-map-prefix (kbd "C-c C-r")
  "Zd refactor keymap prefix."
  :type 'string
  :package-version '(zd-mode . "5.6.0"))

(defvar zd-refactor-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-t") #'zd-thread)
    (define-key map (kbd "t") #'zd-thread)
    (define-key map (kbd "C-u") #'zd-unwind)
    (define-key map (kbd "u") #'zd-unwind)
    (define-key map (kbd "C-f") #'zd-thread-first-all)
    (define-key map (kbd "f") #'zd-thread-first-all)
    (define-key map (kbd "C-l") #'zd-thread-last-all)
    (define-key map (kbd "l") #'zd-thread-last-all)
    (define-key map (kbd "C-p") #'zd-cycle-privacy)
    (define-key map (kbd "p") #'zd-cycle-privacy)
    (define-key map (kbd "C-(") #'zd-convert-collection-to-list)
    (define-key map (kbd "(") #'zd-convert-collection-to-list)
    (define-key map (kbd "C-'") #'zd-convert-collection-to-quoted-list)
    (define-key map (kbd "'") #'zd-convert-collection-to-quoted-list)
    (define-key map (kbd "C-{") #'zd-convert-collection-to-map)
    (define-key map (kbd "{") #'zd-convert-collection-to-map)
    (define-key map (kbd "C-[") #'zd-convert-collection-to-vector)
    (define-key map (kbd "[") #'zd-convert-collection-to-vector)
    (define-key map (kbd "C-#") #'zd-convert-collection-to-set)
    (define-key map (kbd "#") #'zd-convert-collection-to-set)
    (define-key map (kbd "C-i") #'zd-cycle-if)
    (define-key map (kbd "i") #'zd-cycle-if)
    (define-key map (kbd "C-w") #'zd-cycle-when)
    (define-key map (kbd "w") #'zd-cycle-when)
    (define-key map (kbd "C-o") #'zd-cycle-not)
    (define-key map (kbd "o") #'zd-cycle-not)
    (define-key map (kbd "n i") #'zd-insert-ns-form)
    (define-key map (kbd "n h") #'zd-insert-ns-form-at-point)
    (define-key map (kbd "n u") #'zd-update-ns)
    (define-key map (kbd "n s") #'zd-sort-ns)
    (define-key map (kbd "n r") #'zd-rename-ns-alias)
    (define-key map (kbd "s i") #'zd-introduce-let)
    (define-key map (kbd "s m") #'zd-move-to-let)
    (define-key map (kbd "s f") #'zd-let-forward-slurp-sexp)
    (define-key map (kbd "s b") #'zd-let-backward-slurp-sexp)
    (define-key map (kbd "C-a") #'zd-add-arity)
    (define-key map (kbd "a") #'zd-add-arity)
    map)
  "Keymap for Zd refactoring commands.")
(fset 'zd-refactor-map zd-refactor-map)

(defvar zd-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    (define-key map (kbd "C-:") #'zd-toggle-keyword-string)
    (define-key map (kbd "C-c SPC") #'zd-align)
    (define-key map zd-refactor-map-prefix 'zd-refactor-map)
    (easy-menu-define zd-mode-menu map "Zd Mode Menu"
      '("Zd"
        ["Toggle between string & keyword" zd-toggle-keyword-string]
        ["Align expression" zd-align]
        ["Cycle privacy" zd-cycle-privacy]
        ["Cycle if, if-not" zd-cycle-if]
        ["Cycle when, when-not" zd-cycle-when]
        ["Cycle not" zd-cycle-not]
        ["Add function arity" zd-add-arity]
        ("ns forms"
         ["Insert ns form at the top" zd-insert-ns-form]
         ["Insert ns form here" zd-insert-ns-form-at-point]
         ["Update ns form" zd-update-ns]
         ["Sort ns form" zd-sort-ns]
         ["Rename ns alias" zd-rename-ns-alias])
        ("Convert collection"
         ["Convert to list" zd-convert-collection-to-list]
         ["Convert to quoted list" zd-convert-collection-to-quoted-list]
         ["Convert to map" zd-convert-collection-to-map]
         ["Convert to vector" zd-convert-collection-to-vector]
         ["Convert to set" zd-convert-collection-to-set])
        ("Refactor -> and ->>"
         ["Thread once more" zd-thread]
         ["Fully thread a form with ->" zd-thread-first-all]
         ["Fully thread a form with ->>" zd-thread-last-all]
         "--"
         ["Unwind once" zd-unwind]
         ["Fully unwind a threading macro" zd-unwind-all])
        ("Let expression"
         ["Introduce let" zd-introduce-let]
         ["Move to let" zd-move-to-let]
         ["Forward slurp form into let" zd-let-forward-slurp-sexp]
         ["Backward slurp form into let" zd-let-backward-slurp-sexp])
        ("Documentation"
         ["View a Zd guide" zd-view-guide]
         ["View a Zd reference section" zd-view-reference-section]
         ["View the Zd cheatsheet" zd-view-cheatsheet]
         ["View the Zd style guide" zd-view-style-guide])
        "--"
        ["Report a zd-mode bug" zd-mode-report-bug]
        ["Zd-mode version" zd-mode-display-version]))
    map)
  "Keymap for Zd mode.")

(defvar zd-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Initialize ASCII charset as symbol syntax
    (modify-syntax-entry '(0 . 127) "_" table)

    ;; Word syntax
    (modify-syntax-entry '(?0 . ?9) "w" table)
    (modify-syntax-entry '(?a . ?z) "w" table)
    (modify-syntax-entry '(?A . ?Z) "w" table)

    ;; Whitespace
    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\xa0 " " table) ; non-breaking space
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\f " " table)
    ;; Setting commas as whitespace makes functions like `delete-trailing-whitespace' behave unexpectedly (#561)
    (modify-syntax-entry ?, "." table)

    ;; Delimiters
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; Prefix chars
    (modify-syntax-entry ?` "'" table)
    (modify-syntax-entry ?~ "'" table)
    (modify-syntax-entry ?^ "'" table)
    (modify-syntax-entry ?@ "'" table)
    (modify-syntax-entry ?? "_ p" table) ; ? is a prefix outside symbols
    (modify-syntax-entry ?# "_ p" table) ; # is allowed inside keywords (#399)
    (modify-syntax-entry ?' "_ p" table) ; ' is allowed anywhere but the start of symbols

    ;; Others
    (modify-syntax-entry ?\; "<" table) ; comment start
    (modify-syntax-entry ?\n ">" table) ; comment end
    (modify-syntax-entry ?\" "\"" table) ; string
    (modify-syntax-entry ?\\ "\\" table) ; escape

    table)
  "Syntax table for Zd mode.")

(defconst zd--prettify-symbols-alist
  '(("fn"  . ?λ)))

(defvar-local zd-expected-ns-function nil
  "The function used to determine the expected namespace of a file.
`zd-mode' ships a basic function named `zd-expected-ns'
that does basic heuristics to figure this out.
CIDER provides a more complex version which does classpath analysis.")

(defun zd-mode-display-version ()
  "Display the current `zd-mode-version' in the minibuffer."
  (interactive)
  (message "zd-mode (version %s)" zd-mode-version))

(defconst zd-mode-report-bug-url "https://github.com/zd-emacs/zd-mode/issues/new"
  "The URL to report a `zd-mode' issue.")

(defun zd-mode-report-bug ()
  "Report a bug in your default browser."
  (interactive)
  (browse-url zd-mode-report-bug-url))

(defconst zd-guides-base-url "https://zd.org/guides/"
  "The base URL for official Zd guides.")

(defconst zd-guides '(("Getting Started" . "getting_started")
                           ("FAQ" . "faq")
                           ("spec" . "spec")
                           ("Destructuring" . "destructuring")
                           ("Threading Macros" . "threading_macros")
                           ("Comparators" . "comparators")
                           ("Reader Conditionals" . "reader_conditionals"))
  "A list of all official Zd guides.")

(defun zd-view-guide ()
  "Open a Zd guide in your default browser.

The command will prompt you to select one of the available guides."
  (interactive)
  (let ((guide (completing-read "Select a guide: " (mapcar #'car zd-guides))))
    (when guide
      (let ((guide-url (concat zd-guides-base-url (cdr (assoc guide zd-guides)))))
        (browse-url guide-url)))))

(defconst zd-reference-base-url "https://zd.org/reference/"
  "The base URL for the official Zd reference.")

(defconst zd-reference-sections '(("The Reader" . "reader")
                                       ("The REPL and main" . "repl_and_main")
                                       ("Evaluation" . "evaluation")
                                       ("Special Forms" . "special_forms")
                                       ("Macros" . "macros")
                                       ("Other Functions" . "other_functions")
                                       ("Data Structures" . "data_structures")
                                       ("Datatypes" . "datatypes")
                                       ("Sequences" . "sequences")
                                       ("Transients" . "transients")
                                       ("Transducers" . "transducers")
                                       ("Multimethods and Hierarchies" . "multimethods")
                                       ("Protocols" . "protocols")
                                       ("Metadata" . "metadata")
                                       ("Namespaces" . "namespaces")
                                       ("Libs" . "libs")
                                       ("Vars and Environments" . "vars")
                                       ("Refs and Transactions" . "refs")
                                       ("Agents" . "agents")
                                       ("Atoms" . "atoms")
                                       ("Reducers" . "reducers")
                                       ("Java Interop" . "java_interop")
                                       ("Compilation and Class Generation" . "compilation")
                                       ("Other Libraries" . "other_libraries")
                                       ("Differences with Lisps" . "lisps")))

(defun zd-view-reference-section ()
  "Open a Zd reference section in your default browser.

The command will prompt you to select one of the available sections."
  (interactive)
  (let ((section (completing-read "Select a reference section: " (mapcar #'car zd-reference-sections))))
    (when section
      (let ((section-url (concat zd-reference-base-url (cdr (assoc section zd-reference-sections)))))
        (browse-url section-url)))))

(defconst zd-cheatsheet-url "https://zd.org/api/cheatsheet"
  "The URL of the official Zd cheatsheet.")

(defun zd-view-cheatsheet ()
  "Open the Zd cheatsheet in your default browser."
  (interactive)
  (browse-url zd-cheatsheet-url))

(defconst zd-style-guide-url "https://guide.zd.style"
  "The URL of the Zd style guide.")

(defun zd-view-style-guide ()
  "Open the Zd style guide in your default browser."
  (interactive)
  (browse-url zd-style-guide-url))

(defun zd-space-for-delimiter-p (endp delim)
  "Prevent paredit from inserting useless spaces.
See `paredit-space-for-delimiter-predicates' for the meaning of
ENDP and DELIM."
  (and (not endp)
       ;; don't insert after opening quotes, auto-gensym syntax, or reader tags
       (not (looking-back
             (if (member delim zd-omit-space-between-tag-and-delimiters)
                 "\\_<\\(?:'+\\|#.*\\)"
               "\\_<\\(?:'+\\|#\\)")
             (point-at-bol)))))

(defconst zd--collection-tag-regexp "#\\(::[a-zA-Z0-9._-]*\\|:?\\([a-zA-Z0-9._-]+/\\)?[a-zA-Z0-9._-]+\\)"
  "Collection reader macro tag regexp.
It is intended to check for allowed strings that can come before a
collection literal (e.g. '[]' or '{}'), as reader macro tags.
This includes #fully.qualified/my-ns[:kw val] and #::my-ns{:kw
val} as of Zd 1.9.")

(make-obsolete-variable 'zd--collection-tag-regexp nil "5.12.0")
(make-obsolete #'zd-no-space-after-tag #'zd-space-for-delimiter-p "5.12.0")

(declare-function paredit-open-curly "ext:paredit" t t)
(declare-function paredit-close-curly "ext:paredit" t t)
(declare-function paredit-convolute-sexp "ext:paredit")

(defun zd--replace-let-bindings-and-indent ()
  "Replace let bindings and indent."
  (save-excursion
    (backward-sexp)
    (when (looking-back zd--let-regexp nil)
      (zd--replace-sexps-with-bindings-and-indent))))

(defun zd-paredit-setup (&optional keymap)
  "Make \"paredit-mode\" play nice with `zd-mode'.

If an optional KEYMAP is passed the changes are applied to it,
instead of to `zd-mode-map'.
Also advice `paredit-convolute-sexp' when used on a let form as drop in
replacement for `zdr-expand-let`."
  (when (>= paredit-version 21)
    (let ((keymap (or keymap zd-mode-map)))
      (define-key keymap "{" #'paredit-open-curly)
      (define-key keymap "}" #'paredit-close-curly))
    (make-local-variable 'paredit-space-for-delimiter-predicates)
    (add-to-list 'paredit-space-for-delimiter-predicates
                 #'zd-space-for-delimiter-p)
    (advice-add 'paredit-convolute-sexp :after #'zd--replace-let-bindings-and-indent)))

(defun zd-mode-variables ()
  "Set up initial buffer-local variables for Zd mode."
  (add-to-list 'imenu-generic-expression '(nil zd-match-next-def 0))
  (setq-local indent-tabs-mode nil)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local outline-regexp ";;;;* ")
  (setq-local outline-level 'lisp-outline-level)
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1) ; default to `;;' in comment-region
  (setq-local comment-column 40)
  (setq-local comment-use-syntax t)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local electric-pair-skip-whitespace 'chomp)
  (setq-local electric-pair-open-newline-between-pairs nil)
  (setq-local fill-paragraph-function #'zd-fill-paragraph)
  (setq-local adaptive-fill-function #'zd-adaptive-fill-function)
  (setq-local normal-auto-fill-function #'zd-auto-fill-function)
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq-local indent-line-function #'zd-indent-line)
  (setq-local indent-region-function #'zd-indent-region)
  (setq-local lisp-indent-function #'zd-indent-function)
  (setq-local lisp-doc-string-elt-property 'zd-doc-string-elt)
  (setq-local zd-expected-ns-function #'zd-expected-ns)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local prettify-symbols-alist zd--prettify-symbols-alist)
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local beginning-of-defun-function #'zd-beginning-of-defun-function))

(defsubst zd-in-docstring-p ()
  "Check whether point is in a docstring."
  (let ((ppss (syntax-ppss)))
    ;; are we in a string?
    (when (nth 3 ppss)
      ;; check font lock at the start of the string
      (eq (get-text-property (nth 8 ppss) 'face)
          'font-lock-doc-face))))

;;;###autoload
(define-derived-mode zd-mode prog-mode "Zd"
  "Major mode for editing Zd code.

\\{zd-mode-map}"
  (zd-mode-variables)
  (zd-font-lock-setup)
  (add-hook 'paredit-mode-hook #'zd-paredit-setup)
  ;; `electric-layout-post-self-insert-function' prevents indentation in strings
  ;; and comments, force indentation of non-inlined docstrings:
  (add-hook 'electric-indent-functions
            (lambda (_char) (if (and (zd-in-docstring-p)
                                     ;; make sure we're not dealing with an inline docstring
                                     ;; e.g. (def foo "inline docstring" bar)
                                     (save-excursion
                                       (beginning-of-line-text)
                                       (eq (get-text-property (point) 'face)
                                           'font-lock-doc-face)))
                                'do-indent))))

(defcustom zd-verify-major-mode t
  "If non-nil, warn when activating the wrong `major-mode'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(zd-mode "5.3.0"))

(defun zd--check-wrong-major-mode ()
  "Check if the current `major-mode' matches the file extension.

If it doesn't, issue a warning if `zd-verify-major-mode' is
non-nil."
  (when (and zd-verify-major-mode
             (stringp (buffer-file-name)))
    (let* ((case-fold-search t)
           (problem (cond ((and (string-match "\\.zd\\'" (buffer-file-name))
                                (not (eq major-mode 'zd-mode)))
                           'zd-mode)
                          ((and (string-match "\\.zds\\'" (buffer-file-name))
                                (not (eq major-mode 'zdscript-mode)))
                           'zdscript-mode)
                          ((and (string-match "\\.zdc\\'" (buffer-file-name))
                                (not (eq major-mode 'zdc-mode)))
                           'zdc-mode))))
      (when problem
        (message "[WARNING] %s activated `%s' instead of `%s' in this buffer.
This could cause problems.
\(See `zd-verify-major-mode' to disable this message.)"
                 (if (eq major-mode real-this-command)
                     "You have"
                   "Something in your configuration")
                 major-mode
                 problem)))))

(add-hook 'zd-mode-hook #'zd--check-wrong-major-mode)

(defsubst zd-docstring-fill-prefix ()
  "The prefix string used by `zd-fill-paragraph'.
It is simply `zd-docstring-fill-prefix-width' number of spaces."
  (make-string zd-docstring-fill-prefix-width ? ))

(defun zd-adaptive-fill-function ()
  "Zd adaptive fill function.
This only takes care of filling docstring correctly."
  (when (zd-in-docstring-p)
    (zd-docstring-fill-prefix)))

(defun zd-fill-paragraph (&optional justify)
  "Like `fill-paragraph', but can handle Zd docstrings.
If JUSTIFY is non-nil, justify as well as fill the paragraph."
  (if (zd-in-docstring-p)
      (let ((paragraph-start
             (concat paragraph-start
                     "\\|\\s-*\\([(:\"[]\\|~@\\|`(\\|#'(\\)"))
            (paragraph-separate
             (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
            (fill-column (or zd-docstring-fill-column fill-column))
            (fill-prefix (zd-docstring-fill-prefix)))
        ;; we are in a string and string start pos (8th element) is non-nil
        (let* ((beg-doc (nth 8 (syntax-ppss)))
               (end-doc (save-excursion
                          (goto-char beg-doc)
                          (or (ignore-errors (forward-sexp) (point))
                              (point-max)))))
          (save-restriction
            (narrow-to-region beg-doc end-doc)
            (fill-paragraph justify))))
    (let ((paragraph-start (concat paragraph-start
                                   "\\|\\s-*\\([(:\"[]\\|`(\\|#'(\\)"))
          (paragraph-separate
           (concat paragraph-separate "\\|\\s-*\".*[,\\.[]$")))
      (or (fill-comment-paragraph justify)
          (fill-paragraph justify))
      ;; Always return `t'
      t)))

(defun zd-auto-fill-function ()
  "Zd auto-fill function."
  ;; Check if auto-filling is meaningful.
  (let ((fc (current-fill-column)))
    (when (and fc (> (current-column) fc))
      (let ((fill-column (if (zd-in-docstring-p)
                             zd-docstring-fill-column
                           fill-column))
            (fill-prefix (zd-adaptive-fill-function)))
        (do-auto-fill)))))


;;; #_ comments font-locking
;; Code heavily borrowed from Slime.
;; https://github.com/slime/slime/blob/master/contrib/slime-fontifying-fu.el#L186
(defvar zd--comment-macro-regexp
  (rx (seq (+ (seq "#_" (* " ")))) (group-n 1 (not (any " "))))
  "Regexp matching the start of a comment sexp.
The beginning of match-group 1 should be before the sexp to be
marked as a comment.  The end of sexp is found with
`zd-forward-logical-sexp'.")

(defvar zd--reader-and-comment-regexp
  (rx (or (seq (+ (seq "#_" (* " ")))
               (group-n 1 (not (any " "))))
          (seq (group-n 1 "(comment" symbol-end))))
  "Regexp matching both `#_' macro and a comment sexp." )

(defcustom zd-comment-regexp zd--comment-macro-regexp
  "Comment mode.

The possible values for this variable are keywords indicating
what is considered a comment (affecting font locking).

    - Reader macro `#_' only - the default
    - Reader macro `#_' and `(comment)'"
  :type '(choice (const :tag "Reader macro `#_' and `(comment)'" zd--reader-and-comment-regexp)
                 (other :tag "Reader macro `#_' only" zd--comment-macro-regexp))
  :package-version '(zd-mode . "5.7.0"))

(defun zd--search-comment-macro-internal (limit)
  "Search for a comment forward stopping at LIMIT."
  (when (search-forward-regexp zd-comment-regexp limit t)
    (let* ((md (match-data))
           (start (match-beginning 1))
           (state (syntax-ppss start)))
      ;; inside string or comment?
      (if (or (nth 3 state)
              (nth 4 state))
          (zd--search-comment-macro-internal limit)
        (goto-char start)
        ;; Count how many #_ we got and step by that many sexps
        ;; For (comment ...), step at least 1 sexp
        (zd-forward-logical-sexp
         (max (count-matches (rx "#_") (elt md 0) (elt md 1))
              1))
        ;; Data for (match-end 1).
        (setf (elt md 3) (point))
        (set-match-data md)
        t))))

(defun zd--search-comment-macro (limit)
  "Find comment macros and set the match data.
Search from point up to LIMIT.  The region that should be
considered a comment is between `(match-beginning 1)'
and `(match-end 1)'."
  (let ((result 'retry))
    (while (and (eq result 'retry) (<= (point) limit))
      (condition-case nil
          (setq result (zd--search-comment-macro-internal limit))
        (end-of-file (setq result nil))
        (scan-error  (setq result 'retry))))
    result))


;;; General font-locking
(defun zd-match-next-def ()
  "Scans the buffer backwards for the next \"top-level\" definition.
Called by `imenu--generic-function'."
  ;; we have to take into account namespace-definition forms
  ;; e.g. s/defn
  (when (re-search-backward "^[ \t]*(\\([a-z0-9.-]+/\\)?\\(def\\sw*\\)" nil t)
    (save-excursion
      (let (found?
            (deftype (match-string 2))
            (start (point)))
        (down-list)
        (forward-sexp)
        (while (not found?)
          (ignore-errors
            (forward-sexp))
          (or (when (char-equal ?\[ (char-after (point)))
                (backward-sexp))
              (when (char-equal ?\) (char-after (point)))
                (backward-sexp)))
          (cl-destructuring-bind (def-beg . def-end) (bounds-of-thing-at-point 'sexp)
            (if (char-equal ?^ (char-after def-beg))
                (progn (forward-sexp) (backward-sexp))
              (setq found? t)
              (when (string= deftype "defmethod")
                (setq def-end (progn (goto-char def-end)
                                     (forward-sexp)
                                     (point))))
              (set-match-data (list def-beg def-end)))))
        (goto-char start)))))

(eval-and-compile
  (defconst zd--sym-forbidden-rest-chars "][\";@\\^`~\(\)\{\}\\,\s\t\n\r"
    "A list of chars that a Zd symbol cannot contain.
See definition of 'macros': URL `http://git.io/vRGLD'.")
  (defconst zd--sym-forbidden-1st-chars (concat zd--sym-forbidden-rest-chars "0-9:'")
    "A list of chars that a Zd symbol cannot start with.
See the for-loop: URL `http://git.io/vRGTj' lines: URL
`http://git.io/vRGIh', URL `http://git.io/vRGLE' and value
definition of 'macros': URL `http://git.io/vRGLD'.")
  (defconst zd--sym-regexp
    (concat "[^" zd--sym-forbidden-1st-chars "][^" zd--sym-forbidden-rest-chars "]*")
    "A regexp matching a Zd symbol or namespace alias.
Matches the rule `zd--sym-forbidden-1st-chars' followed by
any number of matches of `zd--sym-forbidden-rest-chars'."))

(defconst zd-font-lock-keywords
  (eval-when-compile
    `( ;; Top-level variable definition
      (,(concat " \\([a-z]*/\\)$")
       1 font-lock-keyword-face)


      (,(concat "\\<\\(@[.a-zA-Z]*\\)\\>$")
       1 font-lock-keyword-face)

      (,(concat "\\(\\^\\[\\)")
       1 font-lock-keyword-face)


      (,(concat "\\(~#\\)")
       1 font-lock-comment-face)

      (,(concat "\\((([^)]))\\)")
       1 'font-lock-doc-face)

      ;; ;; Type definition
      ;; (,(concat "(\\(?:zd.core/\\)?\\("
      ;;           (regexp-opt '("defstruct" "deftype" "defprotocol"
      ;;                         "defrecord"))
      ;;           ;; type declarations
      ;;           "\\)\\>"
      ;;           ;; Any whitespace
      ;;           "[ \r\n\t]*"
      ;;           ;; Possibly type or metadata
      ;;           "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
      ;;           "\\(\\sw+\\)?")
      ;;  (1 font-lock-keyword-face)
      ;;  (2 font-lock-type-face nil t))

      ;; Function definition (anything that starts with def and is not
      ;; listed above)
      (,(concat "(\\(?:" zd--sym-regexp "/\\)?"
                "\\(def[^ \r\n\t]*\\)"
                ;; Function declarations
                "\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                (concat "\\(" zd--sym-regexp "\\)?"))
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))

      ;; Macros similar to let, when, and while
      (,(rx symbol-start
            (or "let" "when" "while") "-"
            (1+ (or (syntax word) (syntax symbol)))
            symbol-end)
       0 font-lock-keyword-face)
      ;; :title and tag
      ;; (,(concat "^\\(:title\\|:tags\\)\\>")
      ;;  1 font-lock-keyword-face)

      ;; (,(concat "^\\(@.*\\)$")
      ;;  1 font-lock-type-face)
      ;; Global constants - nil, true, false
      (,(concat "\\<" (regexp-opt '("true" "false" "nil") t) "\\>")
       0 font-lock-constant-face)
      ;; Character literals - \1, \a, \newline, \u0000
      (,(rx "\\" (or any
                    "newline" "space" "tab" "formfeed" "backspace"
                    "return"
                    (: "u" (= 4 (char "0-9a-fA-F")))
                    (: "o" (repeat 1 3 (char "0-7"))))
            word-boundary)
       0 'zd-character-face)

      ;; TODO dedupe the code for matching of keywords, type-hints and unmatched symbols

      ;; keywords: {:oneword/ve/yCom|pLex.stu-ff 0}
      (,(concat "\\(:\\{1,2\\}\\)\\(" zd--sym-regexp "?\\)\\(/\\)\\(" zd--sym-regexp "\\)")
       (1 'zd-keyword-face)
       (2 font-lock-type-face)
       ;; (2 'zd-keyword-face)
       (3 'default)
       (4 'zd-keyword-face))
      (,(concat "\\(~?\\)\\(:\\{1,2\\}\\)\\(" zd--sym-regexp "\\)")
       (1 'font-lock-comment-face)
       (2 'zd-keyword-face)
       (3 'zd-keyword-face))

      (,(concat "\\(#?\\^\\)\\(" zd--sym-regexp "\\)")
       (1 'default)
       (2 font-lock-type-face))

      ;; zd symbols not matched by the previous regexps; influences CIDER's
      ;; dynamic syntax highlighting (CDSH). See https://git.io/vxEEA:
      (,(concat "\\(" zd--sym-regexp "?\\)\\(/\\)\\(" zd--sym-regexp "\\)")
       (1 font-lock-type-face)
       ;; 2nd and 3th matching groups can be font-locked to `nil' or `default'.
       ;; CDSH seems to kick in only for functions and variables referenced w/o
       ;; writing their namespaces.
       (2 nil)
       (3 nil))
      (,(concat "\\(" zd--sym-regexp "\\)")
       ;; this matching group must be font-locked to `nil' otherwise CDSH breaks.
       (1 nil))
      ;; Special forms
      (,(concat
         "("
         (regexp-opt
          '("def" "do" "if" "let" "let*" "var" "fn" "fn*" "loop" "loop*"
            "recur" "throw" "try" "catch" "finally"
            "set!" "new" "."
            "monitor-enter" "monitor-exit" "quote") t)
         "\\>")
       1 font-lock-keyword-face)
      ;; Built-in binding and flow of control forms
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("letfn" "case" "cond" "cond->" "cond->>" "condp"
            "for" "when" "when-not" "when-let" "when-first" "when-some"
            "if-let" "if-not" "if-some"
            ".." "->" "->>" "as->" "doto" "and" "or"
            "dosync" "doseq" "dotimes" "dorun" "doall"
            "ns" "in-ns"
            "with-open" "with-local-vars" "binding"
            "with-redefs" "with-redefs-fn"
            "declare") t)
         "\\>")
       1 font-lock-keyword-face)

      ;; #_ and (comment ...) macros.
      (zd--search-comment-macro 1 font-lock-comment-face t)
      ;; Highlight `code` marks, just like `elisp'.
      (,(rx "`" (group-n 1 (optional "#'")
                         (+ (or (syntax symbol) (syntax word)))) "`")
       (1 'font-lock-constant-face prepend))
      ;; Highlight [[var]] comments
      (,(rx "[[" (group-n 1 (optional "#'")
                         (+ (or (syntax symbol) (syntax word)))) "]]")
       (1 'font-lock-constant-face prepend))
      ;; Highlight escaped characters in strings.
      (zd-font-lock-escaped-chars 0 'bold prepend)
      ;; Highlight grouping constructs in regular expressions
      (zd-font-lock-regexp-groups
       (1 'font-lock-regexp-grouping-construct prepend))))
  "Default expressions to highlight in Zd mode.")

(defun zd-font-lock-syntactic-face-function (state)
  "Find and highlight text with a Zd-friendly syntax table.

This function is passed to `font-lock-syntactic-face-function',
which is called with a single parameter, STATE (which is, in
turn, returned by `parse-partial-sexp' at the beginning of the
highlighted region)."
  (if (nth 3 state)
      ;; This is a (doc)string
      (let* ((startpos (nth 8 state))
             (listbeg (nth 1 state))
             (firstsym (and listbeg
                            (save-excursion
                              (goto-char listbeg)
                              (and (looking-at "([ \t\n]*\\(\\(\\sw\\|\\s_\\)+\\)")
                                   (match-string 1)))))
             (docelt (and firstsym
                          (function-get (intern-soft firstsym)
                                        lisp-doc-string-elt-property))))
        (if (and docelt
                 ;; It's a string in a form that can have a docstring.
                 ;; Check whether it's in docstring position.
                 (save-excursion
                   (when (functionp docelt)
                     (goto-char (match-end 1))
                     (setq docelt (funcall docelt)))
                   (goto-char listbeg)
                   (forward-char 1)
                   (ignore-errors
                     (while (and (> docelt 0) (< (point) startpos)
                                 (progn (forward-sexp 1) t))
                       ;; ignore metadata and type hints
                       (unless (looking-at "[ \n\t]*\\(\\^[A-Z:].+\\|\\^?{.+\\)")
                         (setq docelt (1- docelt)))))
                   (and (zerop docelt) (<= (point) startpos)
                        (progn (forward-comment (point-max)) t)
                        (= (point) (nth 8 state))))
                 ;; In a def, at last position is not a docstring
                 (not (and (string= "def" firstsym)
                           (save-excursion
                             (goto-char startpos)
                             (goto-char (end-of-thing 'sexp))
                             (looking-at "[ \r\n\t]*\)")))))
            font-lock-doc-face
          font-lock-string-face))
    font-lock-comment-face))

(defun zd-font-lock-setup ()
  "Configures font-lock for editing Zd code."
  (setq-local font-lock-multiline t)
  (add-to-list 'font-lock-extend-region-functions
               #'zd-font-lock-extend-region-def t)
  (setq font-lock-defaults
        '(zd-font-lock-keywords    ; keywords
          nil nil
          (("+-*/.<>=!?$%_&:" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . zd-font-lock-syntactic-face-function))))

(defun zd-font-lock-def-at-point (point)
  "Range between the top-most def* and the fourth element after POINT.
Note that this means that there is no guarantee of proper font
locking in def* forms that are not at top level."
  (goto-char point)
  (ignore-errors
    (beginning-of-defun))

  (let ((beg-def (point)))
    (when (and (not (= point beg-def))
               (looking-at "(def"))
      (ignore-errors
        ;; move forward as much as possible until failure (or success)
        (forward-char)
        (dotimes (_ 4)
          (forward-sexp)))
      (cons beg-def (point)))))

(defun zd-font-lock-extend-region-def ()
  "Set region boundaries to include the first four elements of def* forms."
  (let ((changed nil))
    (let ((def (zd-font-lock-def-at-point font-lock-beg)))
      (when def
        (cl-destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-beg)
                     (< font-lock-beg def-end))
            (setq font-lock-beg def-beg
                  changed t)))))
    (let ((def (zd-font-lock-def-at-point font-lock-end)))
      (when def
        (cl-destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-end)
                     (< font-lock-end def-end))
            (setq font-lock-end def-end
                  changed t)))))
    changed))

(defun zd--font-locked-as-string-p (&optional regexp)
  "Non-nil if the char before point is font-locked as a string.
If REGEXP is non-nil, also check whether current string is
preceeded by a #."
  (let ((face (get-text-property (1- (point)) 'face)))
    (and (or (and (listp face)
                  (memq 'font-lock-string-face face))
             (eq 'font-lock-string-face face))
         (or (zd-string-start t)
             (unless regexp
               (zd-string-start nil))))))

(defun zd-font-lock-escaped-chars (bound)
  "Highlight \escaped chars in strings.
BOUND denotes a buffer position to limit the search."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward "\\\\." bound t))

      (setq found (zd--font-locked-as-string-p)))
    found))

(defun zd-font-lock-regexp-groups (bound)
  "Highlight grouping constructs in regular expression.

BOUND denotes the maximum number of characters (relative to the
point) to check."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward (eval-when-compile
                                     (concat
                                      ;; A group may start using several alternatives:
                                      "\\(\\(?:"
                                      ;; 1. (? special groups
                                      "(\\?\\(?:"
                                      ;; a) non-capturing group (?:X)
                                      ;; b) independent non-capturing group (?>X)
                                      ;; c) zero-width positive lookahead (?=X)
                                      ;; d) zero-width negative lookahead (?!X)
                                      "[:=!>]\\|"
                                      ;; e) zero-width positive lookbehind (?<=X)
                                      ;; f) zero-width negative lookbehind (?<!X)
                                      "<[=!]\\|"
                                      ;; g) named capturing group (?<name>X)
                                      "<[[:alnum:]]+>"
                                      "\\)\\|" ;; end of special groups
                                      ;; 2. normal capturing groups (
                                      ;; 3. we also highlight alternative
                                      ;; separarators |, and closing parens )
                                      "[|()]"
                                      "\\)\\)"))
                                   bound t))
      (setq found (zd--font-locked-as-string-p 'regexp)))
    found))

;; Docstring positions
(put 'ns 'zd-doc-string-elt 2)
(put 'def 'zd-doc-string-elt 2)
(put 'defn 'zd-doc-string-elt 2)
(put 'defn- 'zd-doc-string-elt 2)
(put 'defmulti 'zd-doc-string-elt 2)
(put 'defmacro 'zd-doc-string-elt 2)
(put 'definline 'zd-doc-string-elt 2)
(put 'defprotocol 'zd-doc-string-elt 2)
(put 'deftask 'zd-doc-string-elt 2) ;; common Boot macro

;;; Vertical alignment
(defcustom zd-align-forms-automatically nil
  "If non-nil, vertically align some forms automatically.
Automatically means it is done as part of indenting code.  This
applies to binding forms (`zd-align-binding-forms'), to cond
forms (`zd-align-cond-forms') and to map literals.  For
instance, selecting a map a hitting \\<zd-mode-map>`\\[indent-for-tab-command]'
will align the values like this:
    {:some-key 10
     :key2     20}"
  :package-version '(zd-mode . "5.1")
  :safe #'booleanp
  :type 'boolean)

(defconst zd--align-separator-newline-regexp "^ *$")

(defcustom zd-align-separator zd--align-separator-newline-regexp
  "The separator that will be passed to `align-region' when performing vertical alignment."
  :package-version '(zd-mode . "5.10")
  :type `(choice (const :tag "Make blank lines prevent vertical alignment from happening."
                        ,zd--align-separator-newline-regexp)
                 (other :tag "Allow blank lines to happen within a vertically-aligned expression."
                        'entire)))

(defcustom zd-align-reader-conditionals nil
  "Whether to align reader conditionals, as if they were maps."
  :package-version '(zd-mode . "5.10")
  :safe #'booleanp
  :type 'boolean)

(defcustom zd-align-binding-forms
  '("let" "when-let" "when-some" "if-let" "if-some" "binding" "loop"
    "doseq" "for" "with-open" "with-local-vars" "with-redefs")
  "List of strings matching forms that have binding forms."
  :package-version '(zd-mode . "5.1")
  :safe #'listp
  :type '(repeat string))

(defcustom zd-align-cond-forms
  '("condp" "cond" "cond->" "cond->>" "case" "are"
    "zd.core/condp" "zd.core/cond" "zd.core/cond->"
    "zd.core/cond->>" "zd.core/case" "zd.test/are")
  "List of strings identifying cond-like forms."
  :package-version '(zd-mode . "5.1")
  :safe #'listp
  :type '(repeat string))

(defvar zd--beginning-of-reader-conditional-regexp
  "#\\?@(\\|#\\?("
  "Regexp denoting the beginning of a reader conditional.")

(defun zd--position-for-alignment ()
  "Non-nil if the sexp around point should be automatically aligned.
This function expects to be called immediately after an
open-brace or after the function symbol in a function call.

First check if the sexp around point is a map literal, or is a
call to one of the vars listed in `zd-align-cond-forms'.  If
it isn't, return nil.  If it is, return non-nil and place point
immediately before the forms that should be aligned.

For instance, in a map literal point is left immediately before
the first key; while, in a let-binding, point is left inside the
binding vector and immediately before the first binding
construct."
  (let ((point (point)))
    ;; Are we in a map?
    (or (and (eq (char-before) ?{)
             (not (eq (char-before (1- point)) ?\#)))
        ;; Are we in a reader conditional?
        (and zd-align-reader-conditionals
             (looking-back zd--beginning-of-reader-conditional-regexp (- (point) 4)))
        ;; Are we in a cond form?
        (let* ((fun    (car (member (thing-at-point 'symbol) zd-align-cond-forms)))
               (method (and fun (zd--get-indent-method fun)))
               ;; The number of special arguments in the cond form is
               ;; the number of sexps we skip before aligning.
               (skip   (cond ((numberp method) method)
                             ((null method) 0)
                             ((sequencep method) (elt method 0)))))
          (when (and fun (numberp skip))
            (zd-forward-logical-sexp skip)
            (comment-forward (point-max))
            fun)) ; Return non-nil (the var name).
        ;; Are we in a let-like form?
        (when (member (thing-at-point 'symbol)
                      zd-align-binding-forms)
          ;; Position inside the binding vector.
          (zd-forward-logical-sexp)
          (backward-sexp)
          (when (eq (char-after) ?\[)
            (forward-char 1)
            (comment-forward (point-max))
            ;; Return non-nil.
            t)))))

(defun zd--find-sexp-to-align (end)
  "Non-nil if there's a sexp ahead to be aligned before END.
Place point as in `zd--position-for-alignment'."
  ;; Look for a relevant sexp.
  (let ((found))
    (while (and (not found)
                (search-forward-regexp
                 (concat (when zd-align-reader-conditionals
                           (concat zd--beginning-of-reader-conditional-regexp
                                   "\\|"))
                         "{\\|("
                         (regexp-opt
                          (append zd-align-binding-forms
                                  zd-align-cond-forms)
                          'symbols))
                 end 'noerror))

      (let ((ppss (syntax-ppss)))
        ;; If we're in a string or comment.
        (unless (or (elt ppss 3)
                    (elt ppss 4))
          ;; Only stop looking if we successfully position
          ;; the point.
          (setq found (zd--position-for-alignment)))))
    found))

(defun zd--search-whitespace-after-next-sexp (&optional bound _noerror)
  "Move point after all whitespace after the next sexp.

Set the match data group 1 to be this region of whitespace and
return point.

BOUND is bounds the whitespace search."
  (unwind-protect
      (ignore-errors
        (zd-forward-logical-sexp 1)
        (search-forward-regexp "\\([,\s\t]*\\)" bound)
        (pcase (syntax-after (point))
          ;; End-of-line, try again on next line.
          (`(12) (zd--search-whitespace-after-next-sexp bound))
          ;; Closing paren, stop here.
          (`(5 . ,_) nil)
          ;; Anything else is something to align.
          (_ (point))))
    (when (and bound (> (point) bound))
      (goto-char bound))))

(defun zd-align (beg end)
  "Vertically align the contents of the sexp around point.
If region is active, align it.  Otherwise, align everything in the
current \"top-level\" sexp.
When called from lisp code align everything between BEG and END."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (save-excursion
                   (let ((end (progn (end-of-defun)
                                     (point))))
                     (zd-backward-logical-sexp)
                     (list (point) end)))))
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (zd--find-sexp-to-align end)
      (let ((sexp-end (save-excursion
                        (backward-up-list)
                        (forward-sexp 1)
                        (point-marker)))
            (zd-align-forms-automatically nil)
            (count 1))
        ;; For some bizarre reason, we need to `align-region' once for each
        ;; group.
        (save-excursion
          (while (search-forward-regexp "^ *\n" sexp-end 'noerror)
            (cl-incf count)))
        ;; Pre-indent the region to avoid aligning to improperly indented
        ;; contents (#551). Also fixes #360.
        (indent-region (point) sexp-end)
        (dotimes (_ count)
          (align-region (point) sexp-end nil
                        `((zd-align (regexp . zd--search-whitespace-after-next-sexp)
                                         (group . 1)
                                         (separate . ,zd-align-separator)
                                         (repeat . t)))
                        nil))))))

;;; Indentation
(defun zd-indent-region (beg end)
  "Like `indent-region', but also maybe align forms.
Forms between BEG and END are aligned according to
`zd-align-forms-automatically'."
  (prog1 (let ((indent-region-function nil))
           (indent-region beg end))
    (when zd-align-forms-automatically
      (condition-case nil
          (zd-align beg end)
        (scan-error nil)))))

(defun zd-indent-line ()
  "Indent current line as Zd code."
  (if (zd-in-docstring-p)
      (save-excursion
        (beginning-of-line)
        (when (and (looking-at "^\\s-*")
                   (<= (string-width (match-string-no-properties 0))
                       (string-width (zd-docstring-fill-prefix))))
          (replace-match (zd-docstring-fill-prefix))))
    (lisp-indent-line)))

(defvar zd-get-indent-function nil
  "Function to get the indent spec of a symbol.
This function should take one argument, the name of the symbol as
a string.  This name will be exactly as it appears in the buffer,
so it might start with a namespace alias.

This function is analogous to the `zd-indent-function'
symbol property, and its return value should match one of the
allowed values of this property.  See `zd-indent-function'
for more information.")

(defun zd--get-indent-method (function-name)
  "Return the indent spec for the symbol named FUNCTION-NAME.
FUNCTION-NAME is a string.  If it contains a `/', also try only
the part after the `/'.

Look for a spec using `zd-get-indent-function', then try the
`zd-indent-function' and `zd-backtracking-indent'
symbol properties."
  (or (when (functionp zd-get-indent-function)
        (funcall zd-get-indent-function function-name))
      (get (intern-soft function-name) 'zd-indent-function)
      (get (intern-soft function-name) 'zd-backtracking-indent)
      (when (string-match "/\\([^/]+\\)\\'" function-name)
        (or (get (intern-soft (match-string 1 function-name))
                 'zd-indent-function)
            (get (intern-soft (match-string 1 function-name))
                 'zd-backtracking-indent)))
      ;; indent symbols starting with if, when, ...
      ;; such as if-let, when-let, ...
      ;; like if, when, ...
      (when (string-match (rx string-start (or "if" "when" "let" "while") (syntax symbol))
                          function-name)
        (zd--get-indent-method (substring (match-string 0 function-name) 0 -1)))))

(defvar zd--current-backtracking-depth 0)

(defun zd--find-indent-spec-backtracking ()
  "Return the indent sexp that applies to the sexp at point.
Implementation function for `zd--find-indent-spec'."
  (when (and (>= zd-max-backtracking zd--current-backtracking-depth)
             (not (looking-at "^")))
    (let ((zd--current-backtracking-depth (1+ zd--current-backtracking-depth))
          (pos 0))
      ;; Count how far we are from the start of the sexp.
      (while (ignore-errors (zd-backward-logical-sexp 1)
                            (not (or (bobp)
                                     (eq (char-before) ?\n))))
        (cl-incf pos))
      (let* ((function (thing-at-point 'symbol))
             (method (or (when function ;; Is there a spec here?
                           (zd--get-indent-method function))
                         (ignore-errors
                           ;; Otherwise look higher up.
                           (pcase (syntax-ppss)
                             (`(,(pred (< 0)) ,start . ,_)
                              (goto-char start)
                              (zd--find-indent-spec-backtracking)))))))
        (when (numberp method)
          (setq method (list method)))
        (pcase method
          ((pred functionp)
           (when (= pos 0)
             method))
          ((pred sequencep)
           (pcase (length method)
             (`0 nil)
             (`1 (let ((head (elt method 0)))
                   (when (or (= pos 0) (sequencep head))
                     head)))
             (l (if (>= pos l)
                    (elt method (1- l))
                  (elt method pos)))))
          ((or `defun `:defn)
           (when (= pos 0)
             :defn))
          (_
           (message "Invalid indent spec for `%s': %s" function method)
           nil))))))

(defun zd--find-indent-spec ()
  "Return the indent spec that applies to current sexp.
If `zd-use-backtracking-indent' is non-nil, also do
backtracking up to a higher-level sexp in order to find the
spec."
  (if zd-use-backtracking-indent
      (save-excursion
        (zd--find-indent-spec-backtracking))
    (let ((function (thing-at-point 'symbol)))
      (zd--get-indent-method function))))

(defun zd--keyword-to-symbol (keyword)
  "Convert KEYWORD to symbol."
  (intern (substring (symbol-name keyword) 1)))

(defun zd--normal-indent (last-sexp indent-mode)
  "Return the normal indentation column for a sexp.
Point should be after the open paren of the _enclosing_ sexp, and
LAST-SEXP is the start of the previous sexp (immediately before
the sexp being indented).  INDENT-MODE is any of the values
accepted by `zd-indent-style'."
  (goto-char last-sexp)
  (forward-sexp 1)
  (zd-backward-logical-sexp 1)
  (let ((last-sexp-start nil))
    (if (ignore-errors
          ;; `backward-sexp' until we reach the start of a sexp that is the
          ;; first of its line (the start of the enclosing sexp).
          (while (string-match
                  "[^[:blank:]]"
                  (buffer-substring (line-beginning-position) (point)))
            (setq last-sexp-start (prog1 (point)
                                    (forward-sexp -1))))
          t)
        ;; Here we have found an arg before the arg we're indenting which is at
        ;; the start of a line. Every mode simply aligns on this case.
        (current-column)
      ;; Here we have reached the start of the enclosing sexp (point is now at
      ;; the function name), so the behaviour depends on INDENT-MODE and on
      ;; whether there's also an argument on this line (case A or B).
      (let ((indent-mode (if (keywordp indent-mode)
                             ;; needed for backwards compatibility
                             ;; as before zd-mode 5.10 indent-mode was a keyword
                             (zd--keyword-to-symbol indent-mode)
                           indent-mode))
            (case-a ; The meaning of case-a is explained in `zd-indent-style'.
             (and last-sexp-start
                  (< last-sexp-start (line-end-position)))))
        (cond
         ((eq indent-mode 'always-indent)
          (+ (current-column) lisp-body-indent -1))
         ;; There's an arg after the function name, so align with it.
         (case-a (goto-char last-sexp-start)
                 (current-column))
         ;; Not same line.
         ((eq indent-mode 'align-arguments)
          (+ (current-column) lisp-body-indent -1))
         ;; Finally, just align with the function name.
         (t (current-column)))))))

(defun zd--not-function-form-p ()
  "Non-nil if form at point doesn't represent a function call."
  (or (member (char-after) '(?\[ ?\{))
      (save-excursion ;; Catch #?@ (:zds ...)
        (skip-chars-backward "\r\n[:blank:]")
        (when (eq (char-before) ?@)
          (forward-char -1))
        (and (eq (char-before) ?\?)
             (eq (char-before (1- (point))) ?\#)))
      ;; Car of form is not a symbol.
      (not (looking-at ".\\(?:\\sw\\|\\s_\\)"))))

;; Check the general context, and provide indentation for data structures and
;; special macros. If current form is a function (or non-special macro),
;; delegate indentation to `zd--normal-indent'.
(defun zd-indent-function (indent-point state)
  "When indenting a line within a function call, indent properly.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Zd function with a
non-nil property `zd-indent-function', that specifies how to do
the indentation.

The property value can be

- `:defn', meaning indent `defn'-style;
- an integer N, meaning indent the first N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body;
- a function to call just as this function was called.
  If that function returns nil, that means it doesn't specify
  the indentation.
- a list, which is used by `zd-backtracking-indent'.

This function also returns nil meaning don't specify the indentation."
  ;; Goto to the open-paren.
  (goto-char (elt state 1))
  ;; Maps, sets, vectors and reader conditionals.
  (if (zd--not-function-form-p)
      (1+ (current-column))
    ;; Function or macro call.
    (forward-char 1)
    (let ((method (zd--find-indent-spec))
          (last-sexp calculate-lisp-indent-last-sexp)
          (containing-form-column (1- (current-column))))
      (pcase method
        ((or (pred integerp) `(,method))
         (let ((pos -1))
           (condition-case nil
               (while (and (<= (point) indent-point)
                           (not (eobp)))
                 (zd-forward-logical-sexp 1)
                 (cl-incf pos))
             ;; If indent-point is _after_ the last sexp in the
             ;; current sexp, we detect that by catching the
             ;; `scan-error'. In that case, we should return the
             ;; indentation as if there were an extra sexp at point.
             (scan-error (cl-incf pos)))
           (cond
            ;; The first non-special arg. Rigidly reduce indentation.
            ((= pos (1+ method))
             (+ lisp-body-indent containing-form-column))
            ;; Further non-special args, align with the arg above.
            ((> pos (1+ method))
             (zd--normal-indent last-sexp 'always-align))
            ;; Special arg. Rigidly indent with a large indentation.
            (t
             (+ (* 2 lisp-body-indent) containing-form-column)))))
        (`:defn
         (+ lisp-body-indent containing-form-column))
        ((pred functionp)
         (funcall method indent-point state))
        ;; No indent spec, do the default.
        (`nil
         (let ((function (thing-at-point 'symbol)))
           (cond
            ;; Preserve useful alignment of :require (and friends) in `ns' forms.
            ((and function (string-match "^:" function))
             (zd--normal-indent last-sexp 'always-align))
            ;; This should be identical to the :defn above.
            ((and function
                  (string-match "\\`\\(?:\\S +/\\)?\\(def[a-z]*\\|with-\\)"
                                function)
                  (not (string-match "\\`default" (match-string 1 function))))
             (+ lisp-body-indent containing-form-column))
            ;; Finally, nothing special here, just respect the user's
            ;; preference.
            (t (zd--normal-indent last-sexp zd-indent-style)))))))))

;;; Setting indentation
(defun put-zd-indent (sym indent)
  "Instruct `zd-indent-function' to indent the body of SYM by INDENT."
  (put sym 'zd-indent-function indent))

(defmacro define-zd-indent (&rest kvs)
  "Call `put-zd-indent' on a series, KVS."
  `(progn
     ,@(mapcar (lambda (x) `(put-zd-indent
                             (quote ,(car x)) ,(cadr x)))
               kvs)))

(defun add-custom-zd-indents (name value)
  "Allow `zd-defun-indents' to indent user-specified macros.

Requires the macro's NAME and a VALUE."
  (custom-set-default name value)
  (mapcar (lambda (x)
            (put-zd-indent x 'defun))
          value))

(defcustom zd-defun-indents nil
  "List of additional symbols with defun-style indentation in Zd.

You can use this to let Emacs indent your own macros the same way
that it indents built-in macros like with-open.  This variable
only works when set via the customize interface (`setq' won't
work).  To set it from Lisp code, use
     (put-zd-indent \\='some-symbol :defn)."
  :type '(repeat symbol)
  :set 'add-custom-zd-indents)

(define-zd-indent
  ;; built-ins
  (ns 1)
  (fn :defn)
  (def :defn)
  (defn :defn)
  (bound-fn :defn)
  (if 1)
  (if-not 1)
  (case 1)
  (cond 0)
  (condp 2)
  (cond-> 1)
  (cond->> 1)
  (when 1)
  (while 1)
  (when-not 1)
  (when-first 1)
  (do 0)
  (delay 0)
  (future 0)
  (comment 0)
  (doto 1)
  (locking 1)
  (proxy '(2 nil nil (:defn)))
  (as-> 2)
  (fdef 1)

  (reify '(:defn (1)))
  (deftype '(2 nil nil (:defn)))
  (defrecord '(2 nil nil (:defn)))
  (defprotocol '(1 (:defn)))
  (definterface '(1 (:defn)))
  (extend 1)
  (extend-protocol '(1 :defn))
  (extend-type '(1 :defn))
  ;; specify and specify! are from ZdScript
  (specify '(1 :defn))
  (specify! '(1 :defn))
  (try 0)
  (catch 2)
  (finally 0)

  ;; binding forms
  (let 1)
  (letfn '(1 ((:defn)) nil))
  (binding 1)
  (loop 1)
  (for 1)
  (doseq 1)
  (dotimes 1)
  (when-let 1)
  (if-let 1)
  (when-some 1)
  (if-some 1)
  (this-as 1) ; ZdScript

  (defmethod :defn)

  ;; zd.test
  (testing 1)
  (deftest :defn)
  (are 2)
  (use-fixtures :defn)

  ;; core.logic
  (run :defn)
  (run* :defn)
  (fresh :defn)

  ;; core.async
  (alt! 0)
  (alt!! 0)
  (go 0)
  (go-loop 1)
  (thread 0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Better docstring filling for zd-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zd-string-start (&optional regex)
  "Return the position of the \" that begins the string at point.
If REGEX is non-nil, return the position of the # that begins the
regex at point.  If point is not inside a string or regex, return
nil."
  (when (nth 3 (syntax-ppss)) ;; Are we really in a string?
    (save-excursion
      (save-match-data
        ;; Find a quote that appears immediately after whitespace,
        ;; beginning of line, hash, or an open paren, brace, or bracket
        (re-search-backward "\\(\\s-\\|^\\|#\\|(\\|\\[\\|{\\)\\(\"\\)")
        (let ((beg (match-beginning 2)))
          (when beg
            (if regex
                (and (char-before beg) (eq ?# (char-before beg)) (1- beg))
              (when (not (eq ?# (char-before beg)))
                beg))))))))

(defun zd-char-at-point ()
  "Return the char at point or nil if at buffer end."
  (when (not (= (point) (point-max)))
    (buffer-substring-no-properties (point) (1+ (point)))))

(defun zd-char-before-point ()
  "Return the char before point or nil if at buffer beginning."
  (when (not (= (point) (point-min)))
    (buffer-substring-no-properties (point) (1- (point)))))

(defun zd-toggle-keyword-string ()
  "Convert the string or keyword at point to keyword or string."
  (interactive)
  (let ((original-point (point)))
    (while (and (> (point) 1)
                (not (equal "\"" (buffer-substring-no-properties (point) (+ 1 (point)))))
                (not (equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))))
      (backward-char))
    (cond
     ((equal 1 (point))
      (error "Beginning of file reached, this was probably a mistake"))
     ((equal "\"" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert ":" (substring (zd-delete-and-extract-sexp) 1 -1)))
     ((equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "\"" (substring (zd-delete-and-extract-sexp) 1) "\"")))
    (goto-char original-point)))

(defun zd-delete-and-extract-sexp ()
  "Delete the surrounding sexp and return it."
  (let ((begin (point)))
    (forward-sexp)
    (let ((result (buffer-substring begin (point))))
      (delete-region begin (point))
      result)))



(defcustom zd-cache-project-dir t
  "Whether to cache the results of `zd-project-dir'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(zd-mode . "5.8.0"))

(defvar-local zd-cached-project-dir nil
  "A project dir cache used to speed up related operations.")

(defun zd-project-dir (&optional dir-name)
  "Return the absolute path to the project's root directory.

Call is delegated down to `zd-project-root-function' with
optional DIR-NAME as argument.

When `zd-cache-project-dir' is t the results of the command
are cached in a buffer local variable (`zd-cached-project-dir')."
  (let ((project-dir (or zd-cached-project-dir
                         (funcall zd-project-root-function dir-name))))
    (when (and zd-cache-project-dir
               (derived-mode-p 'zd-mode)
               (not zd-cached-project-dir))
      (setq zd-cached-project-dir project-dir))
    project-dir))

(defun zd-project-root-path (&optional dir-name)
  "Return the absolute path to the project's root directory.

Use `default-directory' if DIR-NAME is nil.
Return nil if not inside a project."
  (let* ((dir-name (or dir-name default-directory))
         (choices (delq nil
                        (mapcar (lambda (fname)
                                  (locate-dominating-file dir-name fname))
                                zd-build-tool-files))))
    (when (> (length choices) 0)
      (car (sort choices #'file-in-directory-p)))))

(defun zd-project-relative-path (path)
  "Denormalize PATH by making it relative to the project root."
  (file-relative-name path (zd-project-dir)))


;;; ns manipulation
(defun zd-expected-ns (&optional path)
  "Return the namespace matching PATH.

PATH is expected to be an absolute file path.

If PATH is nil, use the path to the file backing the current buffer."
  (let* ((path (or path (file-truename (buffer-file-name))))
         (relative (zd-project-relative-path path))
         (sans-file-type (substring relative 0 (- (length (file-name-extension path t)))))
         (sans-file-sep (mapconcat 'identity (cdr (split-string sans-file-type "/")) "."))
         (sans-underscores (replace-regexp-in-string "_" "-" sans-file-sep)))
    ;; Drop prefix from ns for projects with structure src/{zd,zds,zdc}
    (replace-regexp-in-string "\\`zd[scx]?\\." "" sans-underscores)))

(defun zd-insert-ns-form-at-point ()
  "Insert a namespace form at point."
  (interactive)
  (insert (format "(ns %s)" (funcall zd-expected-ns-function))))

(defun zd-insert-ns-form ()
  "Insert a namespace form at the beginning of the buffer."
  (interactive)
  (widen)
  (goto-char (point-min))
  (zd-insert-ns-form-at-point))

(defun zd-update-ns ()
  "Update the namespace of the current buffer.
Useful if a file has been renamed."
  (interactive)
  (let ((nsname (funcall zd-expected-ns-function)))
    (when nsname
      (save-excursion
        (save-match-data
          (if (zd-find-ns)
              (progn
                (replace-match nsname nil nil nil 4)
                (message "ns form updated to `%s'" nsname)
                (setq zd-cached-ns nsname))
            (user-error "Can't find ns form")))))))

(defun zd--sort-following-sexps ()
  "Sort sexps between point and end of current sexp.
Comments at the start of a line are considered part of the
following sexp.  Comments at the end of a line (after some other
content) are considered part of the preceding sexp."
  ;; Here we're after the :require/:import symbol.
  (save-restriction
    (narrow-to-region (point) (save-excursion
                                (up-list)
                                (1- (point))))
    (skip-chars-forward "\r\n[:blank:]")
    (sort-subr nil
               (lambda () (skip-chars-forward "\r\n[:blank:]"))
               ;; Move to end of current top-level thing.
               (lambda ()
                 (condition-case nil
                     (while t (up-list))
                   (scan-error nil))
                 ;; We could be inside a symbol instead of a sexp.
                 (unless (looking-at "\\s-\\|$")
                   (zd-forward-logical-sexp))
                 ;; move past comments at the end of the line.
                 (search-forward-regexp "$"))
               ;; Move to start of ns name.
               (lambda ()
                 (comment-forward)
                 (skip-chars-forward "[:blank:]\n\r[(")
                 (zd-forward-logical-sexp)
                 (forward-sexp -1)
                 nil)
               ;; Move to end of ns name.
               (lambda ()
                 (zd-forward-logical-sexp)))
    (goto-char (point-max))
    ;; Does the last line now end in a comment?
    (when (nth 4 (parse-partial-sexp (point-min) (point)))
      (insert "\n"))))

(defun zd-sort-ns ()
  "Internally sort each sexp inside the ns form."
  (interactive)
  (comment-normalize-vars)
  (if (zd-find-ns)
      (save-excursion
        (goto-char (match-beginning 0))
        (redisplay)
        (let ((beg (point))
              (ns))
          (forward-sexp 1)
          (setq ns (buffer-substring beg (point)))
          (forward-char -1)
          (while (progn (forward-sexp -1)
                        (looking-at "(:[a-z]"))
            (save-excursion
              (forward-char 1)
              (forward-sexp 1)
              (zd--sort-following-sexps)))
          (goto-char beg)
          (if (looking-at (regexp-quote ns))
              (message "ns form is already sorted")
            (sleep-for 0.1)
            (redisplay)
            (message "ns form has been sorted")
            (sleep-for 0.1))))
    (user-error "Can't find ns form")))

(defconst zd-namespace-name-regex
  (rx line-start
      "("
      (zero-or-one (group (regexp "zd.core/")))
      (zero-or-one (submatch "in-"))
      "ns"
      (zero-or-one "+")
      (one-or-more (any whitespace "\n"))
      (zero-or-more (or (submatch (zero-or-one "#")
                                  "^{"
                                  (zero-or-more (not (any "}")))
                                  "}")
                        (zero-or-more "^:"
                                      (one-or-more (not (any whitespace)))))
                    (one-or-more (any whitespace "\n")))
      (zero-or-one (any ":'")) ;; (in-ns 'foo) or (ns+ :user)
      (group (one-or-more (not (any "()\"" whitespace))) symbol-end)))

(make-obsolete-variable 'zd-namespace-name-regex 'zd-namespace-regexp "5.12.0")

(defconst zd-namespace-regexp
  (rx line-start "(" (? "zd.core/") (or "in-ns" "ns" "ns+") symbol-end))

(defcustom zd-cache-ns nil
  "Whether to cache the results of `zd-find-ns'.

Note that this won't work well in buffers with multiple namespace
declarations (which rarely occur in practice) and you'll
have to invalidate this manually after changing the ns for
a buffer.  If you update the ns using `zd-update-ns'
the cached value will be updated automatically."
  :type 'boolean
  :safe #'booleanp
  :package-version '(zd-mode . "5.8.0"))

(defvar-local zd-cached-ns nil
  "A buffer ns cache used to speed up ns-related operations.")

(defun zd--find-ns-in-direction (direction)
  "Return the nearest namespace in a specific DIRECTION.
DIRECTION is `forward' or `backward'."
  (let ((candidate)
        (fn (if (eq direction 'forward)
                #'search-forward-regexp
              #'search-backward-regexp)))
    (while (and (not candidate)
                (funcall fn zd-namespace-regexp nil t))
      (let ((end (match-end 0)))
        (save-excursion
          (save-match-data
            (goto-char end)
            (zd-forward-logical-sexp)
            (unless (or (zd--in-string-p) (zd--in-comment-p))
              (setq candidate (string-remove-prefix "'" (thing-at-point 'symbol))))))))
    candidate))

(defun zd-find-ns ()
  "Return the namespace of the current Zd buffer.
Return the namespace closest to point and above it.  If there are
no namespaces above point, return the first one in the buffer.

The results will be cached if `zd-cache-ns' is set to t."
  (if (and zd-cache-ns zd-cached-ns)
      zd-cached-ns
    (let ((ns (save-excursion
                (save-restriction
                  (widen)

                  ;; Move to top-level to avoid searching from inside ns
                  (ignore-errors (while t (up-list nil t t)))

                  (or (zd--find-ns-in-direction 'backward)
                      (zd--find-ns-in-direction 'forward))))))
      (setq zd-cached-ns ns)
      ns)))

(defun zd-show-cache ()
  "Display cached values if present.
Useful for debugging."
  (interactive)
  (message "Cached Project: %s, Cached Namespace: %s" zd-cached-project-dir zd-cached-ns))

(defun zd-clear-cache ()
  "Clear all buffer-local cached values.

Normally you'd need to do this very infrequently - e.g.
after renaming the root folder of project or after
renaming a namespace."
  (interactive)
  (setq zd-cached-project-dir nil
        zd-cached-ns nil)
  (message "Buffer-local zd-mode cache cleared"))

(defconst zd-def-type-and-name-regex
  (concat "(\\(?:\\(?:\\sw\\|\\s_\\)+/\\)?"
          ;; Declaration
          "\\(def\\(?:\\sw\\|\\s_\\)*\\)\\>"
          ;; Any whitespace
          "[ \r\n\t]*"
          ;; Possibly type or metadata
          "\\(?:#?^\\(?:{[^}]*}\\|\\(?:\\sw\\|\\s_\\)+\\)[ \r\n\t]*\\)*"
          ;; Symbol name
          "\\(\\(?:\\sw\\|\\s_\\)+\\)"))

(defun zd-find-def ()
  "Find the var declaration macro and symbol name of the current form.
Returns a list pair, e.g. (\"defn\" \"abc\") or (\"deftest\" \"some-test\")."
  (save-excursion
    (unless (looking-at zd-def-type-and-name-regex)
      (beginning-of-defun))
    (when (search-forward-regexp zd-def-type-and-name-regex nil t)
      (list (match-string-no-properties 1)
            (match-string-no-properties 2)))))


;;; Sexp navigation

(defun zd--looking-at-non-logical-sexp ()
  "Return non-nil if text after point is \"non-logical\" sexp.
\"Non-logical\" sexp are ^metadata and #reader.macros."
  (comment-normalize-vars)
  (comment-forward (point-max))
  (looking-at-p "\\(?:#?\\^\\)\\|#:?:?[[:alpha:]]"))

(defun zd-forward-logical-sexp (&optional n)
  "Move forward N logical sexps.
This will skip over sexps that don't represent objects, so that ^hints and
#reader.macros are considered part of the following sexp."
  (interactive "p")
  (unless n (setq n 1))
  (if (< n 0)
      (zd-backward-logical-sexp (- n))
    (let ((forward-sexp-function nil))
      (while (> n 0)
        (while (zd--looking-at-non-logical-sexp)
          (forward-sexp 1))
        ;; The actual sexp
        (forward-sexp 1)
        (skip-chars-forward ",")
        (setq n (1- n))))))

(defun zd-backward-logical-sexp (&optional n)
  "Move backward N logical sexps.
This will skip over sexps that don't represent objects, so that ^hints and
#reader.macros are considered part of the following sexp."
  (interactive "p")
  (unless n (setq n 1))
  (if (< n 0)
      (zd-forward-logical-sexp (- n))
    (let ((forward-sexp-function nil))
      (while (> n 0)
        ;; The actual sexp
        (backward-sexp 1)
        ;; Non-logical sexps.
        (while (and (not (bobp))
                    (ignore-errors
                      (save-excursion
                        (backward-sexp 1)
                        (zd--looking-at-non-logical-sexp))))
          (backward-sexp 1))
        (setq n (1- n))))))

(defun zd-top-level-form-p (first-form)
  "Return truthy if the first form matches FIRST-FORM."
  (condition-case nil
      (save-excursion
        (beginning-of-defun)
        (forward-char 1)
        (zd-forward-logical-sexp 1)
        (zd-backward-logical-sexp 1)
        (looking-at-p first-form))
    (scan-error nil)
    (end-of-buffer nil)))

(defun zd-sexp-starts-until-position (position)
  "Return the starting points for forms before POSITION.
Positions are in descending order to aide in finding the first starting
position before the current position."
  (save-excursion
    (let (sexp-positions)
      (condition-case nil
          (while (< (point) position)
            (zd-forward-logical-sexp 1)
            (zd-backward-logical-sexp 1)
            (push (point) sexp-positions)
            (zd-forward-logical-sexp 1))
        (scan-error nil))
      sexp-positions)))

(defcustom zd-toplevel-inside-comment-form nil
  "Eval top level forms inside comment forms instead of the comment form itself.
Experimental.  Function `cider-defun-at-point' is used extensively so if we
change this heuristic it needs to be bullet-proof and desired.  While
testing, give an easy way to turn this new behavior off."
  :type 'boolean
  :safe #'booleanp
  :package-version '(zd-mode . "5.9.0"))

(defun zd-find-first (pred coll)
  "Find first element of COLL for which PRED return truthy."
  (let ((found)
        (haystack coll))
    (while (and (not found)
                haystack)
      (if (funcall pred (car haystack))
          (setq found (car haystack))
        (setq haystack (cdr haystack))))
    found))

(defun zd-beginning-of-defun-function (&optional n)
  "Go to top level form.
Set as `beginning-of-defun-function' so that these generic
operators can be used.  Given a positive N it will do it that
many times."
  (let ((beginning-of-defun-function nil))
    (if (and zd-toplevel-inside-comment-form
             (zd-top-level-form-p "comment"))
        (condition-case nil
            (save-match-data
              (let ((original-position (point))
                    zd-comment-end)
                (beginning-of-defun)
                (end-of-defun)
                (setq zd-comment-end (point))
                (beginning-of-defun)
                (forward-char 1)              ;; skip paren so we start at comment
                (zd-forward-logical-sexp) ;; skip past the comment form itself
                (if-let ((sexp-start (zd-find-first (lambda (beg-pos)
                                                           (< beg-pos original-position))
                                                         (zd-sexp-starts-until-position
                                                          zd-comment-end))))
                    (progn (goto-char sexp-start) t)
                  (beginning-of-defun n))))
          (scan-error (beginning-of-defun n)))
      (beginning-of-defun n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Refactoring support
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Threading macros related
(defcustom zd-thread-all-but-last nil
  "Non-nil means do not thread the last expression.
This means that `zd-thread-first-all' and
`zd-thread-last-all' not thread the deepest sexp inside the
current sexp."
  :package-version '(zd-mode . "5.4.0")
  :safe #'booleanp
  :type 'boolean)

(defun zd--point-after (&rest actions)
  "Return POINT after performing ACTIONS.

An action is either the symbol of a function or a two element
list of (fn args) to pass to `apply''"
  (save-excursion
    (dolist (fn-and-args actions)
      (let ((f (if (listp fn-and-args) (car fn-and-args) fn-and-args))
            (args (if (listp fn-and-args) (cdr fn-and-args) nil)))
        (apply f args)))
    (point)))

(defun zd--maybe-unjoin-line ()
  "Undo a `join-line' done by a threading command."
  (when (get-text-property (point) 'zd-thread-line-joined)
    (remove-text-properties (point) (1+ (point)) '(zd-thread-line-joined t))
    (insert "\n")))

(defun zd--unwind-last ()
  "Unwind a thread last macro once.

Point must be between the opening paren and the ->> symbol."
  (forward-sexp)
  (save-excursion
    (let ((contents (zd-delete-and-extract-sexp)))
      (when (looking-at " *\n")
        (join-line 'following))
      (zd--ensure-parens-around-function-names)
      (let* ((sexp-beg-line (line-number-at-pos))
             (sexp-end-line (progn (forward-sexp)
                                   (line-number-at-pos)))
             (multiline-sexp-p (not (= sexp-beg-line sexp-end-line))))
        (down-list -1)
        (if multiline-sexp-p
            (insert "\n")
          ;; `zd--maybe-unjoin-line' only works when unwinding sexps that were
          ;; threaded in the same Emacs session, but it also catches cases that
          ;; `multiline-sexp-p' doesn't.
          (zd--maybe-unjoin-line))
        (insert contents))))
  (forward-char))

(defun zd--ensure-parens-around-function-names ()
  "Insert parens around function names if necessary."
  (zd--looking-at-non-logical-sexp)
  (unless (looking-at "(")
    (insert-parentheses 1)
    (backward-up-list)))

(defun zd--unwind-first ()
  "Unwind a thread first macro once.

Point must be between the opening paren and the -> symbol."
  (forward-sexp)
  (save-excursion
    (let ((contents (zd-delete-and-extract-sexp)))
      (when (looking-at " *\n")
        (join-line 'following))
      (zd--ensure-parens-around-function-names)
      (down-list)
      (forward-sexp)
      (insert contents)
      (forward-sexp -1)
      (zd--maybe-unjoin-line)))
  (forward-char))

(defun zd--pop-out-of-threading ()
  "Raise a sexp up a level to unwind a threading form."
  (save-excursion
    (down-list 2)
    (backward-up-list)
    (raise-sexp)))

(defun zd--nothing-more-to-unwind ()
  "Return non-nil if a threaded form cannot be unwound further."
  (save-excursion
    (let ((beg (point)))
      (forward-sexp)
      (down-list -1)
      (backward-sexp 2) ;; the last sexp, the threading macro
      (when (looking-back "(\\s-*" (line-beginning-position))
        (backward-up-list)) ;; and the paren
      (= beg (point)))))

(defun zd--fix-sexp-whitespace (&optional move-out)
  "Fix whitespace after unwinding a threading form.

Optional argument MOVE-OUT, if non-nil, means moves up a list
before fixing whitespace."
  (save-excursion
    (when move-out (backward-up-list))
    (let ((sexp (bounds-of-thing-at-point 'sexp)))
      (zd-indent-region (car sexp) (cdr sexp))
      (delete-trailing-whitespace (car sexp) (cdr sexp)))))

;;;###autoload
(defun zd-unwind (&optional n)
  "Unwind thread at point or above point by N levels.
With universal argument \\[universal-argument], fully unwind thread."
  (interactive "P")
  (setq n (cond ((equal n '(4)) 999)
                (n) (1)))
  (save-excursion
    (let ((limit (save-excursion
                   (beginning-of-defun)
                   (point))))
      (ignore-errors
        (when (looking-at "(")
          (forward-char 1)
          (forward-sexp 1)))
      (while (> n 0)
        (search-backward-regexp "([^-]*->" limit)
        (if (zd--nothing-more-to-unwind)
            (progn (zd--pop-out-of-threading)
                   (zd--fix-sexp-whitespace)
                   (setq n 0)) ;; break out of loop
          (down-list)
          (cond
           ((looking-at "[^-]*->\\_>")  (zd--unwind-first))
           ((looking-at "[^-]*->>\\_>") (zd--unwind-last)))
          (zd--fix-sexp-whitespace 'move-out)
          (setq n (1- n)))))))

;;;###autoload
(defun zd-unwind-all ()
  "Fully unwind thread at point or above point."
  (interactive)
  (zd-unwind '(4)))

(defun zd--remove-superfluous-parens ()
  "Remove extra parens from a form."
  (when (looking-at "([^ )]+)")
    (delete-pair)))

(defun zd--thread-first ()
  "Thread a nested sexp using ->."
  (down-list)
  (forward-symbol 1)
  (unless (looking-at ")")
    (let ((contents (zd-delete-and-extract-sexp)))
      (backward-up-list)
      (just-one-space 0)
      (save-excursion
        (insert contents "\n")
        (zd--remove-superfluous-parens))
      (when (looking-at "\\s-*\n")
        (join-line 'following)
        (forward-char 1)
        (put-text-property (point) (1+ (point))
                           'zd-thread-line-joined t))
      t)))

(defun zd--thread-last ()
  "Thread a nested sexp using ->>."
  (forward-sexp 2)
  (down-list -1)
  (backward-sexp)
  (unless (eq (char-before) ?\()
    (let ((contents (zd-delete-and-extract-sexp)))
      (just-one-space 0)
      (backward-up-list)
      (insert contents "\n")
      (zd--remove-superfluous-parens)
      ;; zdr #255 Fix dangling parens
      (forward-sexp)
      (when (looking-back "^\\s-*\\()+\\)\\s-*" (line-beginning-position))
        (let ((pos (match-beginning 1)))
          (put-text-property pos (1+ pos) 'zd-thread-line-joined t))
        (join-line))
      t)))

(defun zd--threadable-p ()
  "Return non-nil if a form can be threaded."
  (save-excursion
    (forward-symbol 1)
    (looking-at "[\n\r\t ]*(")))

;;;###autoload
(defun zd-thread ()
  "Thread by one more level an existing threading macro."
  (interactive)
  (ignore-errors
    (when (looking-at "(")
      (forward-char 1)
      (forward-sexp 1)))
  (search-backward-regexp "([^-]*->")
  (down-list)
  (when (zd--threadable-p)
    (prog1 (cond
            ((looking-at "[^-]*->\\_>")  (zd--thread-first))
            ((looking-at "[^-]*->>\\_>") (zd--thread-last)))
      (zd--fix-sexp-whitespace 'move-out))))

(defun zd--thread-all (first-or-last-thread but-last)
  "Fully thread the form at point.

FIRST-OR-LAST-THREAD is \"->\" or \"->>\".

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `zd-thread-all-but-last'."
  (save-excursion
    (insert-parentheses 1)
    (insert first-or-last-thread))
  (while (save-excursion (zd-thread)))
  (when (or but-last zd-thread-all-but-last)
    (zd-unwind)))

;;;###autoload
(defun zd-thread-first-all (but-last)
  "Fully thread the form at point using ->.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `zd-thread-all-but-last'."
  (interactive "P")
  (zd--thread-all "-> " but-last))

;;;###autoload
(defun zd-thread-last-all (but-last)
  "Fully thread the form at point using ->>.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `zd-thread-all-but-last'."
  (interactive "P")
  (zd--thread-all "->> " but-last))

;;; Cycling stuff

(defcustom zd-use-metadata-for-privacy nil
  "If nil, `zd-cycle-privacy' will use (defn- f []).
If t, it will use (defn ^:private f [])."
  :package-version '(zd-mode . "5.5.0")
  :safe #'booleanp
  :type 'boolean)

;;;###autoload
(defun zd-cycle-privacy ()
  "Make public the current private def, or vice-versa.
See: https://github.com/zd-emacs/zd-refactor.el/wiki/zdr-cycle-privacy"
  (interactive)
  (save-excursion
    (ignore-errors (forward-char 7))
    (search-backward-regexp "(defn?\\(-\\| ^:private\\)?\\_>")
    (if (match-string 1)
        (replace-match "" nil nil nil 1)
      (goto-char (match-end 0))
      (insert (if (or zd-use-metadata-for-privacy
                      (equal (match-string 0) "(def"))
                  " ^:private"
                "-")))))

(defun zd--convert-collection (coll-open coll-close)
  "Convert the collection at (point) by unwrapping it an wrapping it between COLL-OPEN and COLL-CLOSE."
  (save-excursion
    (while (and
            (not (bobp))
            (not (looking-at "(\\|{\\|\\[")))
      (backward-char))
    (when (or (eq ?\# (char-before))
              (eq ?\' (char-before)))
      (delete-char -1))
    (when (and (bobp)
               (not (memq (char-after) '(?\{ ?\( ?\[))))
      (user-error "Beginning of file reached, collection is not found"))
    (insert coll-open (substring (zd-delete-and-extract-sexp) 1 -1) coll-close)))

;;;###autoload
(defun zd-convert-collection-to-list ()
  "Convert collection at (point) to list."
  (interactive)
  (zd--convert-collection "(" ")"))

;;;###autoload
(defun zd-convert-collection-to-quoted-list ()
  "Convert collection at (point) to quoted list."
  (interactive)
  (zd--convert-collection "'(" ")"))

;;;###autoload
(defun zd-convert-collection-to-map ()
  "Convert collection at (point) to map."
  (interactive)
  (zd--convert-collection "{" "}"))

;;;###autoload
(defun zd-convert-collection-to-vector ()
  "Convert collection at (point) to vector."
  (interactive)
  (zd--convert-collection "[" "]"))

;;;###autoload
(defun zd-convert-collection-to-set ()
  "Convert collection at (point) to set."
  (interactive)
  (zd--convert-collection "#{" "}"))

(defun zd--in-string-p ()
  "Check whether the point is currently in a string."
  (nth 3 (syntax-ppss)))

(defun zd--in-comment-p ()
  "Check whether the point is currently in a comment."
  (nth 4 (syntax-ppss)))

(defun zd--goto-if ()
  "Find the first surrounding if or if-not expression."
  (when (zd--in-string-p)
    (while (or (not (looking-at "("))
               (zd--in-string-p))
      (backward-char)))
  (while (not (looking-at "\\((if \\)\\|\\((if-not \\)"))
    (condition-case nil
        (backward-up-list)
      (scan-error (user-error "No if or if-not found")))))

;;;###autoload
(defun zd-cycle-if ()
  "Change a surrounding if to if-not, or vice-versa.

See: https://github.com/zd-emacs/zd-refactor.el/wiki/zdr-cycle-if"
  (interactive)
  (save-excursion
    (zd--goto-if)
    (cond
     ((looking-at "(if-not")
      (forward-char 3)
      (delete-char 4)
      (forward-sexp 2)
      (transpose-sexps 1))
     ((looking-at "(if")
      (forward-char 3)
      (insert "-not")
      (forward-sexp 2)
      (transpose-sexps 1)))))

;; TODO: Remove code duplication with `zd--goto-if'.
(defun zd--goto-when ()
  "Find the first surrounding when or when-not expression."
  (when (zd--in-string-p)
    (while (or (not (looking-at "("))
               (zd--in-string-p))
      (backward-char)))
  (while (not (looking-at "\\((when \\)\\|\\((when-not \\)"))
    (condition-case nil
        (backward-up-list)
      (scan-error (user-error "No when or when-not found")))))

;;;###autoload
(defun zd-cycle-when ()
  "Change a surrounding when to when-not, or vice-versa."
  (interactive)
  (save-excursion
    (zd--goto-when)
    (cond
     ((looking-at "(when-not")
      (forward-char 9)
      (delete-char -4))
     ((looking-at "(when")
      (forward-char 5)
      (insert "-not")))))

(defun zd-cycle-not ()
  "Add or remove a not form around the current form."
  (interactive)
  (save-excursion
    (condition-case nil
        (backward-up-list)
      (scan-error (user-error "`zd-cycle-not' must be invoked inside a list")))
    (if (looking-back "(not " nil)
        (progn
          (delete-char -5)
          (forward-sexp)
          (delete-char 1))
      (insert "(not ")
      (forward-sexp)
      (insert ")"))))

;;; let related stuff

(defvar zd--let-regexp
  "\(\\(when-let\\|if-let\\|let\\)\\(\\s-*\\|\\[\\)"
  "Regexp matching let like expressions, i.e. \"let\", \"when-let\", \"if-let\".

The first match-group is the let expression.

The second match-group is the whitespace or the opening square
bracket if no whitespace between the let expression and the
bracket.")

(defun zd--goto-let ()
  "Go to the beginning of the nearest let form."
  (when (zd--in-string-p)
    (while (or (not (looking-at "("))
               (zd--in-string-p))
      (backward-char)))
  (ignore-errors
    (while (not (looking-at zd--let-regexp))
      (backward-up-list)))
  (looking-at zd--let-regexp))

(defun zd--inside-let-binding-p ()
  "Return non-nil if point is inside a let binding."
  (ignore-errors
    (save-excursion
      (let ((pos (point)))
        (zd--goto-let)
        (re-search-forward "\\[")
        (if (< pos (point))
            nil
          (forward-sexp)
          (up-list)
          (< pos (point)))))))

(defun zd--beginning-of-current-let-binding ()
  "Move before the bound name of the current binding.
Assume that point is in the binding form of a let."
  (let ((current-point (point)))
    (zd--goto-let)
    (search-forward "[")
    (forward-char)
    (while (> current-point (point))
      (forward-sexp))
    (backward-sexp 2)))

(defun zd--previous-line ()
  "Keep the column position while go the previous line."
  (let ((col (current-column)))
    (forward-line -1)
    (move-to-column col)))

(defun zd--prepare-to-insert-new-let-binding ()
  "Move to right place in the let form to insert a new binding and indent."
  (if (zd--inside-let-binding-p)
      (progn
        (zd--beginning-of-current-let-binding)
        (newline-and-indent)
        (zd--previous-line)
        (indent-for-tab-command))
    (zd--goto-let)
    (search-forward "[")
    (backward-up-list)
    (forward-sexp)
    (down-list -1)
    (backward-char)
    (if (looking-at "\\[\\s-*\\]")
        (forward-char)
      (forward-char)
      (newline-and-indent))))

(defun zd--sexp-regexp (sexp)
  "Return a regexp for matching SEXP."
  (concat "\\([^[:word:]^-]\\)"
          (mapconcat #'identity (mapcar 'regexp-quote (split-string sexp))
                     "[[:space:]\n\r]+")
          "\\([^[:word:]^-]\\)"))

(defun zd--replace-sexp-with-binding (bound-name init-expr)
  "Replace a binding with its bound name in the let form.

BOUND-NAME is the name (left-hand side) of a binding.

INIT-EXPR is the value (right-hand side) of a binding."
  (save-excursion
    (while (re-search-forward
            (zd--sexp-regexp init-expr)
            (zd--point-after 'zd--goto-let 'forward-sexp)
            t)
      (replace-match (concat "\\1" bound-name "\\2")))))

(defun zd--replace-sexps-with-bindings (bindings)
  "Replace bindings with their respective bound names in the let form.

BINDINGS is the list of bound names and init expressions."
  (let ((bound-name (pop bindings))
        (init-expr (pop bindings)))
    (when bound-name
      (zd--replace-sexp-with-binding bound-name init-expr)
      (zd--replace-sexps-with-bindings bindings))))

(defun zd--replace-sexps-with-bindings-and-indent ()
  "Replace sexps with bindings."
  (zd--replace-sexps-with-bindings
   (zd--read-let-bindings))
  (zd-indent-region
   (zd--point-after 'zd--goto-let)
   (zd--point-after 'zd--goto-let 'forward-sexp)))

(defun zd--read-let-bindings ()
  "Read the bound-name and init expression pairs in the binding form.
Return a list: odd elements are bound names, even elements init expressions."
  (zd--goto-let)
  (down-list 2)
  (let* ((start (point))
         (sexp-start start)
         (end (save-excursion
                (backward-char)
                (forward-sexp)
                (down-list -1)
                (point)))
         bindings)
    (while (/= sexp-start end)
      (forward-sexp)
      (push
       (string-trim (buffer-substring-no-properties sexp-start (point)))
       bindings)
      (skip-chars-forward "\r\n\t[:blank:]")
      (setq sexp-start (point)))
    (nreverse bindings)))

(defun zd--introduce-let-internal (name &optional n)
  "Create a let form, binding the form at point with NAME.

Optional numeric argument N, if non-nil, introduces the let N
lists up."
  (if (numberp n)
      (let ((init-expr-sexp (zd-delete-and-extract-sexp)))
        (insert name)
        (ignore-errors (backward-up-list n))
        (insert "(let" (zd-delete-and-extract-sexp) ")")
        (backward-sexp)
        (down-list)
        (forward-sexp)
        (insert " [" name " " init-expr-sexp "]\n")
        (zd--replace-sexps-with-bindings-and-indent))
    (insert "[ " (zd-delete-and-extract-sexp) "]")
    (backward-sexp)
    (insert "(let " (zd-delete-and-extract-sexp) ")")
    (backward-sexp)
    (down-list 2)
    (insert name)
    (forward-sexp)
    (up-list)
    (newline-and-indent)
    (insert name)))

(defun zd--move-to-let-internal (name)
  "Bind the form at point to NAME in the nearest let."
  (if (not (save-excursion (zd--goto-let)))
      (zd--introduce-let-internal name)
    (let ((contents (zd-delete-and-extract-sexp)))
      (insert name)
      (zd--prepare-to-insert-new-let-binding)
      (insert contents)
      (backward-sexp)
      (insert " ")
      (backward-char)
      (insert name)
      (zd--replace-sexps-with-bindings-and-indent))))

(defun zd--let-backward-slurp-sexp-internal ()
  "Slurp the s-expression before the let form into the let form."
  (zd--goto-let)
  (backward-sexp)
  (let ((sexp (string-trim (zd-delete-and-extract-sexp))))
    (delete-blank-lines)
    (down-list)
    (forward-sexp 2)
    (newline-and-indent)
    (insert sexp)
    (zd--replace-sexps-with-bindings-and-indent)))

(defun zd-collect-ns-aliases (ns-form)
  "Collect all namespace aliases in NS-FORM."
  (with-temp-buffer
    (delay-mode-hooks
      (zd-mode)
      (insert ns-form)
      (goto-char (point-min))
      (let ((end (point-max))
            (rgx (rx ":as" (+ space)
                     (group-n 1 (+ (not (in " ,]\n"))))))
            (res ()))
        (while (re-search-forward rgx end 'noerror)
          (unless (or (zd--in-string-p) (zd--in-comment-p))
            (push (match-string-no-properties 1) res)))
        res))))

(defun zd--rename-ns-alias-internal (current-alias new-alias)
  "Rename a namespace alias CURRENT-ALIAS to NEW-ALIAS."
  (zd--find-ns-in-direction 'backward)
  (let ((rgx (concat ":as +" (regexp-quote current-alias) "\\_>"))
        (bound (save-excursion (forward-list 1) (point))))
    (when (search-forward-regexp rgx bound t)
      (replace-match (concat ":as " new-alias))
      (save-excursion
        (while (re-search-forward (concat (regexp-quote current-alias) "/") nil t)
          (when (not (nth 3 (syntax-ppss)))
            (replace-match (concat new-alias "/")))))
      (save-excursion
        (while (re-search-forward (concat "#::" (regexp-quote current-alias) "{") nil t)
          (replace-match (concat "#::" new-alias "{"))))
      (message "Successfully renamed alias '%s' to '%s'" current-alias new-alias))))

;;;###autoload
(defun zd-let-backward-slurp-sexp (&optional n)
  "Slurp the s-expression before the let form into the let form.
With a numeric prefix argument slurp the previous N s-expressions
into the let form."
  (interactive "p")
  (let ((n (or n 1)))
    (dotimes (_ n)
      (save-excursion (zd--let-backward-slurp-sexp-internal)))))

(defun zd--let-forward-slurp-sexp-internal ()
  "Slurp the next s-expression after the let form into the let form."
  (zd--goto-let)
  (forward-sexp)
  (let ((sexp (string-trim (zd-delete-and-extract-sexp))))
    (down-list -1)
    (newline-and-indent)
    (insert sexp)
    (zd--replace-sexps-with-bindings-and-indent)))

;;;###autoload
(defun zd-let-forward-slurp-sexp (&optional n)
  "Slurp the next s-expression after the let form into the let form.
With a numeric prefix argument slurp the next N s-expressions
into the let form."
  (interactive "p")
  (unless n (setq n 1))
  (dotimes (_ n)
    (save-excursion (zd--let-forward-slurp-sexp-internal))))

;;;###autoload
(defun zd-introduce-let (&optional n)
  "Create a let form, binding the form at point.
With a numeric prefix argument the let is introduced N lists up."
  (interactive "P")
  (zd--introduce-let-internal (read-from-minibuffer "Name of bound symbol: ") n))

;;;###autoload
(defun zd-move-to-let ()
  "Move the form at point to a binding in the nearest let."
  (interactive)
  (zd--move-to-let-internal (read-from-minibuffer "Name of bound symbol: ")))

;;;###autoload
(defun zd-rename-ns-alias ()
  "Rename a namespace alias."
  (interactive)
  (save-excursion
    (zd--find-ns-in-direction 'backward)
    (let* ((current-alias (completing-read "Current alias: "
                                           (zd-collect-ns-aliases
                                            (thing-at-point 'list))))
           (rgx (concat ":as +" (regexp-quote current-alias) "\\_>"))
           (bound (save-excursion (forward-list 1) (point))))
      (if (save-excursion (search-forward-regexp rgx bound t))
          (let ((new-alias (read-from-minibuffer "New alias: ")))
            (zd--rename-ns-alias-internal current-alias new-alias))
        (message "Cannot find namespace alias: '%s'" current-alias)))))

(defun zd--add-arity-defprotocol-internal ()
  "Add an arity to a signature inside a defprotocol.

Assumes cursor is at beginning of signature."
  (re-search-forward "\\[")
  (save-excursion (insert "] [")))

(defun zd--add-arity-reify-internal ()
  "Add an arity to a function inside a reify.

Assumes cursor is at beginning of function."
  (re-search-forward "\\(\\w+ \\)")
  (insert "[")
  (save-excursion (insert "])\n(" (match-string 0))))

(defun zd--add-arity-internal ()
  "Add an arity to a function.

Assumes cursor is at beginning of function."
  (let ((beg-line (what-line))
        (end (save-excursion (forward-sexp)
                             (point))))
    (down-list 2)
    (when (looking-back "{" 1) ;; skip metadata if present
      (up-list)
      (down-list))
    (cond
     ((looking-back "(" 1) ;; multi-arity fn
      (insert "[")
      (save-excursion (insert "])\n(")))
     ((looking-back "\\[" 1)  ;; single-arity fn
      (let* ((same-line (string= beg-line (what-line)))
             (new-arity-text (concat (when same-line "\n") "([")))
        (save-excursion
          (goto-char end)
          (insert ")"))

        (re-search-backward " +\\[")
        (replace-match new-arity-text)
        (save-excursion (insert "])\n([")))))))

;;;###autoload
(defun zd-add-arity ()
  "Add an arity to a function."
  (interactive)
  (let ((original-pos (point))
        (n 0))
    (while (not (looking-at-p "(\\(defn\\|letfn\\|fn\\|defmacro\\|defmethod\\|defprotocol\\|reify\\|proxy\\)"))
      (setq n (1+ n))
      (backward-up-list 1 t))
    (let ((beg (point))
          (end-marker (make-marker))
          (end (save-excursion (forward-sexp)
                               (point)))
          (jump-up (lambda (x)
                     (goto-char original-pos)
                     (backward-up-list x t))))
      (set-marker end-marker end)
      (cond
       ((looking-at-p "(\\(defn\\|fn\\|defmethod\\|defmacro\\)")
        (zd--add-arity-internal))
       ((looking-at-p "(letfn")
        (funcall jump-up (- n 2))
        (zd--add-arity-internal))
       ((looking-at-p "(proxy")
        (funcall jump-up (- n 1))
        (zd--add-arity-internal))
       ((looking-at-p "(defprotocol")
        (funcall jump-up (- n 1))
        (zd--add-arity-defprotocol-internal))
       ((looking-at-p "(reify")
        (funcall jump-up (- n 1))
        (zd--add-arity-reify-internal)))
      (indent-region beg end-marker))))


;;; ZdScript
(defconst zdscript-font-lock-keywords
  (eval-when-compile
    `(;; ZdScript built-ins
      (,(concat "(\\(?:\.*/\\)?"
                (regexp-opt '("js-obj" "js-delete" "zd->js" "js->zd"))
                "\\>")
       0 font-lock-builtin-face)))
  "Additional font-locking for `zdscript-mode'.")

;;;###autoload
(define-derived-mode zdscript-mode zd-mode "ZdScript"
  "Major mode for editing ZdScript code.

\\{zdscript-mode-map}"
  (font-lock-add-keywords nil zdscript-font-lock-keywords))

;;;###autoload
(define-derived-mode zdc-mode zd-mode "ZdC"
  "Major mode for editing ZdC code.

\\{zdc-mode-map}")

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist
               '("\\.\\(zd\\|dtm\\|edn\\)\\'" . zd-mode))
  (add-to-list 'auto-mode-alist '("\\.zdc\\'" . zdc-mode))
  (add-to-list 'auto-mode-alist '("\\.zds\\'" . zdscript-mode))
  ;; boot build scripts are Zd source files
  (add-to-list 'auto-mode-alist '("\\(?:build\\|profile\\)\\.boot\\'" . zd-mode)))

(provide 'zd-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; zd-mode.el ends here
