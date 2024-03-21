;;; litable.el --- dynamic evaluation replacement with emacs

;; Copyright (C) 2013-2014 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: lisp
;; Created: 8th April 2013
;; Package-requires: ((dash "2.6.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This allows light-table like dynamic evaluation with Emacs. It's
;; fun for investigating lisp or for particular light problems before
;; you delve in and start hacking serious functions together.

;; It's very much a work in progress, please heed that warning.

;;; Code:

(require 'dash)
(require 'thingatpt)

;;;;; global TODO
;; 1. investigate: http://lists.gnu.org/archive/html/gnu-emacs-sources/2009-04/msg00032.html
;;    and merge relevant parts.
;;
;; 2. update free variable bindings when `setq' call is made on them.

(defgroup litable nil
  "On-the-fly evaluation/substitution of emacs lisp code."
  :group 'completion
  :prefix "litable-")

(defcustom litable-result-overlay-text-function 'litable--create-result-overlay-text
  "Function used to create the result overlay text.
A function that is called with a string argument and an optional face
argument, and should evaluate to text with attendant properties."
  :group 'litable
  :type 'function)

(defcustom litable-substitution-overlay-text-function 'litable--create-substitution-overlay-text
  "Function used to create the substitution overlay text.
A function that is called with a string argument containing the
expression to be replaced, another string argument containing the
value to be used in the substitution, and an optional face argument.
The function should evaluate to text with the desired properties."
  :group 'litable
  :type 'function)

(defcustom litable-result-format " %s "
  "Format used to display a litable result.
A format string like \"=> %s\"."
  :group 'litable
  :type '(choice (string :tag "Format string")))

(defcustom litable-print-function 'pp-to-string
  "Function used to print results and inputs"
  :type '(choice
      (function-item :tag "pp-to-string" :value  pp-to-string)
      (function-item :tag "prin1-to-string"
             :value  prin1-to-string)
      (function :tag "Your own function"))
  :group 'litable)

(defface litable-result-face
  '((default :inherit (font-lock-warning-face)))
  "Face for displaying the litable result.
Defaults to inheriting font-lock-warning-face."
  :group 'litable)

(defface litable-substitution-face
  '((default :inherit (font-lock-type-face)))
  "Face for displaying the litable substitution.
Defaults to inheriting font-lock-type-face."
  :group 'litable)

(defvar litable-exceptions '(
                             (setq . 2)
                             )
  "A list of cons pairs (form-name . nth argument) where the
substitution should not occur.  The number includes the first
item, counting starts at 1.

For example:

  (setq . 2) ;; first argument is target name, do not substitute.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; let-form annotation

(defun litable--annotate-let-form (subs &optional point)
  "Annotate the let form following point.

Add an overlay over the let form that will keep track of the
variables bound there.  If an overlay is already oresent around
point, merge the variables into this overlay."
  (setq point (or point (point)))
  (let* ((let-form (sexp-at-point))
         (bounds (bounds-of-thing-at-point 'sexp))
         (var-form-bounds (save-excursion
                            (down-list)
                            (forward-list)
                            (backward-list)
                            (bounds-of-thing-at-point 'sexp)))
         (cvars (litable--extract-variables-with-defs (cadr let-form))) ; vars defined in current form
         (pvars (litable-get-let-bound-variable-values point)) ; vars defined in the very previous form
         (nvars (litable--merge-variables ; merged vars
                 (litable--get-active-overlay point) subs cvars))
         ov)
    (setq ov (make-overlay (car bounds) (cdr bounds)))
    (push ov litable-overlays)
    (overlay-put ov 'litable-let-form t)
    (overlay-put ov 'litable-let-form-type (car let-form))
    ;; TODO: this still ignores the `setq' updated local bindings.
    (overlay-put ov 'litable-let-form-cur nvars)
    (overlay-put ov 'litable-let-form-prev pvars)
    (overlay-put ov 'litable-var-form-bounds var-form-bounds)))

(defun litable--extract-variables (varlist)
  "Extract the variable names from VARLIST.
VARLIST is a list of the same format `let' accept as first
argument."
  (let (vars)
    (while varlist
      (let ((current (car varlist)))
        (pop varlist)
        (if (listp current)
            (push (car current) vars)
          (push current vars))))
    (nreverse vars)))

(defun litable--extract-variables-with-defs (varlist)
  "Extract the variable names from VARLIST.
VARLIST is a list of the same format `let' accept as first
argument."
  (let (vars)
    (while varlist
      (let ((current (car varlist)))
        (pop varlist)
        (if (listp current)
            (push (cons (car current) (cdr current)) vars)
          (push (cons current nil) vars))))
    (nreverse vars)))

(defun litable--overlays-at (&optional pos)
  "Simple wrapper of `overlays-at' to get only let-form overlays
from litable."
  (--filter (overlay-get it 'litable-let-form) (overlays-at (or pos (point)))))

(defun litable--point-in-overlay-p (overlay)
  "Return t if point is in OVERLAY."
  (and (< (point) (overlay-end overlay))
       (> (point) (overlay-start overlay))))

(defun litable--get-overlay-length (overlay)
  "Compute the length of OVERLAY."
  (- (overlay-end overlay) (overlay-start overlay)))

(defun litable--get-active-overlay (&optional pos)
  "Get active overlay.  Active overlay is the shortest overlay at
point."
  (let ((overlays (litable--overlays-at pos)))
    (cond
     ((not overlays) nil)
     ((not (cdr overlays)) (car overlays))
     (t
      (--reduce (if (< (litable--get-overlay-length it)
                       (litable--get-overlay-length acc)) it acc) overlays)))))

(defun litable--in-var-form-p (&optional pos)
  "Return non-nil if POS is inside a var-form of some let-form."
  (setq pos (or pos (point)))
  (let* ((active (litable--get-active-overlay pos))
         (bounds (and active (overlay-get active 'litable-var-form-bounds))))
    (when bounds
      (and (> pos (car bounds))
           (< pos (cdr bounds))))))

(defun litable-get-let-bound-variables (&optional point symbols)
  "Get a list of let-bound variables at POINT."
  (let ((active (litable--get-active-overlay point)))
    (when active
      (--map (if symbols (car it) (symbol-name (car it))) (overlay-get active 'litable-let-form-cur)))))

(defun litable-get-let-bound-parent-variables (&optional point symbols)
  "Get a list of let-bound variables in the parent form at POINT."
  (let ((active (litable--get-active-overlay point)))
    (when active
      (--map (if symbols (car it) (symbol-name (car it))) (overlay-get active 'litable-let-form-prev)))))

(defun litable-get-let-bound-variable-values (&optional point)
  (let ((active (litable--get-active-overlay point)))
    (when active
      (overlay-get active 'litable-let-form-cur))))

(defun litable-get-let-bound-parent-variable-values (&optional point)
  (let ((active (litable--get-active-overlay point)))
    (when active
      (overlay-get active 'litable-let-form-prev))))

(defun litable-annotate-let-forms (subs &optional point)
  "Annotate all let and let* forms in the defun at point."
  (setq point (or point (point)))
  (save-excursion
    (save-restriction
      (widen)
      (narrow-to-defun)
      ;; TODO: this can be made more efficient somehow -- just reuse
      ;; the overlays, or keep them be and skip evaling this function
      ;; alltogether. Will need a list of already "instrumented"
      ;; functions somewhere.
      (remove-overlays (point-min) (point-max) 'litable-let-form t)
      (goto-char (point-min))
      (while (re-search-forward "(let\\*?" nil t)
        (save-excursion
          (goto-char (match-beginning 0))
          ;; this gen error if the let form is invalid, or inside
          ;; macro etc. Just ignore it
          (ignore-errors (litable--annotate-let-form subs)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fake eval (eval with environment)

(defun litable--fake-eval (form environment &optional type)
  "Evaluate the FORM in ENVIRONMENT using the environment binding of TYPE.

TYPE can be a symbol `let' or `let*'."
  (setq type (or type 'let))
  (ignore-errors (litable--safe-eval `(,type ,environment ,form))))

(defun litable--alist-to-list (alist)
  "Change (a . b) into (a b)"
  (--map (list (car it) (cdr it)) alist))

(defun litable--merge-variables (overlay subs varlist)
  "Merge the varlist with the variables stored in overlays.

This will also evaluate the newly-bound variables."
  (let* ((pvars (or (and overlay (overlay-get overlay 'litable-let-form-cur)) subs))
         (environment (litable--alist-to-list pvars)))
    ;; TODO: THIS DOESN'T WORK WITH let*!! We need to update the
    ;; bindings one by one in that case, and merge after each update.
    (litable--alist-merge
     pvars
     (mapcar (lambda (it)
               (cons (car it)
                     (litable--fake-eval (cadr it) environment 'let)))
             varlist))))

;; TODO: this just sucks... make it better :P
(defun litable--alist-merge (al1 al2)
  "Merge alists AL1 and AL2.

Return a new copy independent of AL1 and AL2.

If the same key is present in both alists, use the value from AL2
in the result."
  (let ((re (--map (cons (car it) (cdr it)) al1)))
    (mapc (lambda (it)
            (let ((c (assoc (car it) re)))
              (if c
                  (setcdr c (cdr it))
                (!cons (cons (car it) (cdr it)) re))))
          al2)
    re))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; argument propagation in the defuns

;; TODO: Should this be protected against unpure? I think that's
;; unecessary as it seems only variable names are being evaluated, but
;; I'm not familiar enough with the code to know.
(defun litable--make-subs-list (arg-names values)
  "Return the list of cons pairs with symbol name in car and value in cdr."
  (let (r)
    (--each (-zip arg-names values)
      ;; do we want to eval here? TODO: Make it a customizable option!
      (!cons (cons (car it)
                   (if (and (listp (cdr it)) (eq 'quote (cadr it)))
                       (cdr it)
                     (eval (cdr it)))) r))
    r))

(defun litable--in-exception-form ()
  "Test if the point is in an exception form."
  (save-excursion
    (litable-backward-up-list)
    (let* ((s (sexp-at-point))
           (ex-form (assq (car s) litable-exceptions)))
      (when ex-form
        (down-list)
        (forward-sexp (cdr ex-form))
        (>= (point) me)))))

(defun litable--at-let-variable-def-p (me)
  "Test if the point is after a let variable definition."
  (/= me (save-excursion
           (litable-backward-up-list)
           (down-list)
           (forward-sexp)
           (point))))

(defun litable--construct-needle (variables)
  "Return a regexp that will search for the variable symbols."
  (regexp-opt (-map #'symbol-name variables) 'symbols))

;; - maybe add different colors for different arguments that get
;;   substituted. This might result in rainbows sometimes, maybe
;;   undesirable
;; TODO: general warning: LONGASS FUNCTION! Refactor this into
;; something more managable.
(defun litable-find-function-subs-arguments (form &optional depth)
  "Find the definition of \"form\" and substitute the arguments.

If depth = 0, also evaluate the current form and print the result."
  (setq depth (or depth 0))
  (let* ((symbol (and (listp form) (car form)))
         (name (and (symbolp symbol) (symbol-name symbol)))
         subs args needle)
    (when (and symbol
               (symbolp symbol)
               (not (keywordp symbol)))
      ;; recursively evaluate the arguments first
      (--each (cdr form) (litable-find-function-subs-arguments it (1+ depth)))
      (when (not (subrp (symbol-function symbol)))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char 1)
            (when (re-search-forward (regexp-quote (concat "(defun " name)) nil t)
              (forward-list) (backward-list)
              ;; TODO: &rest, &key should be handled in some special
              ;; way when doing the substitution
              (setq args (->> (sexp-at-point)
                           (delete '&optional)
                           (delete '&rest)))
              (setq subs (litable--make-subs-list args (cdr form)))
              (save-restriction
                (narrow-to-defun)
                (litable-annotate-let-forms subs)
                (setq needle (litable--construct-needle args))
                (let (mb me ms ignore)
                  (while (re-search-forward needle nil t)
                    (setq mb (match-beginning 0))
                    (setq me (match-end 0))
                    (setq ms (match-string 0))
                    ;; figure out the context here. If the sexp we're in is
                    ;; on the exception list, move along. Maybe we shouldn't
                    ;; censor some results though. TODO: Meditate on this
                    (when (litable--in-exception-form)
                      (setq ignore t))
                    ;; test the let form. TODO: this will go to special
                    ;; function when we decide to do let-def-eval?
                    (let ((in-var-form (litable--in-var-form-p)))
                      (when in-var-form
                        ;; we can still be at the "definition"
                        ;; instance, that is: (>x< (blabla x)). This
                        ;; should not get replaced by the normal value
                        ;; but by the newly eval'd value
                        (if (litable--at-let-variable-def-p me)
                            (setq ignore 'let)
                          (setq ignore 'let-def))))
                    (cond
                     ((eq ignore 'let)
                      (let* ((in-var-form (litable--in-var-form-p))
                             (vars (or (if in-var-form
                                           (litable-get-let-bound-parent-variable-values)
                                         (litable-get-let-bound-variable-values)) subs)))
                        (litable--create-substitution-overlay mb me (cdr (assoc (intern ms) vars)))))
                     ;; TODO: make this configurable too
                     ((eq ignore 'let-def)
                      (let ((vars (litable-get-let-bound-variable-values)))
                        (when vars
                          ;; TODO: make the face customizable
                          (litable--create-substitution-overlay
                           mb me (cdr (assoc (intern ms) vars)) 'litable-result-face))))
                     ((not ignore)
                      (let ((vars (or (litable-get-let-bound-variable-values) subs)))
                        (litable--create-substitution-overlay mb me (cdr (assoc (intern ms) vars))))))
                    (setq ignore nil)
                    ;; TODO: this can be precomputed and stored in the
                    ;; let-form overlay. I think `regexp-opt' can be
                    ;; fairly slow at times.
                    (setq needle (litable--construct-needle
                                  (or (litable-get-let-bound-variables nil t) args)))))
                ;; if depth > 0 means we're updating a defun, print the
                ;; end result after the end of the defun
                ;; TODO: add a customize to print the partial result
                ;; also if depth = 0 (it would be same as the final
                ;; result, but maybe the defun is on different screen
                ;; and so it will be invisible otherwise.)
                (when (> depth 0)
                  (save-excursion
                    (end-of-defun)
                    (backward-char)
                    (litable--print-result (litable--safe-eval form) (point) 'font-lock-constant-face)))
                ;; TODO: make the printing of input customizable
                (save-excursion
                  (beginning-of-defun)
                  (end-of-line)
                  ;; TODO: make the face customizable
                  (litable--print-input (cdr form) (point) 'font-lock-variable-name-face))))))))
    (when (and (= depth 0)
               (nth 1 (syntax-ppss)))
      (let ((ostart (save-excursion
                      (litable-goto-toplevel-form)
                      (forward-list)
                      (point))))
        (litable--print-result (litable--safe-eval form) ostart 'litable-result-face)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating and saving the pure-functions list

(defcustom litable-list-file "~/.emacs.d/.litable-lists.el"
  "The position of the file that keeps track of known pure functions."
  :group 'litable
  :type 'file)

(defun litable--save-lists ()
  "Saves pure functions list to `litable-list-file'"
  (with-temp-file litable-list-file
    (insert ";; This file is automatically generated by litable.el.")
    (newline)
    (insert ";; It keeps track of which functions (beyond the defaults) you trust to be pure.")
    (newline)
    (newline)
    (litable--dump-list 'litable-pure-functions-list)))

(defun litable--dump-list (list-symbol)
  "Insert (setq 'LIST-SYMBOL (append LIST-VALUE LIST-SYMBOL-default) to current buffer."
  (cl-symbol-macrolet ((value (symbol-value list-symbol)))
    (insert "(setq " (symbol-name list-symbol) "\n"
            "      '(")
    (newline-and-indent)
    (set list-symbol
         (sort value (lambda (x y) (string-lessp (symbol-name x)
                                                 (symbol-name y)))))
    (mapc #'(lambda (cmd) (insert (format "%S" cmd)) (newline-and-indent))
          value)
    (insert "))")
    (newline)))

(defvar litable-pure-functions-list
  '(
    %
    *
    +
    -
    -zip
    /
    1+
    1-
    LaTeX-back-to-indentation
    LaTeX-current-environment
    LaTeX-default-environment
    LaTeX-default-style
    LaTeX-find-matching-begin
    LaTeX-mark-environment
    TeX-active-master
    TeX-check-files
    TeX-fold-mode
    TeX-normal-mode
    TeX-output-extension
    abbreviate-file-name
    abs
    activate-mark
    add-text-properties
    alist
    and
    append
    aref
    assoc
    assq
    back-to-indentation
    backward-char
    backward-list
    backward-sexp
    backward-up-list
    backward-word
    beginning-of-buffer
    beginning-of-defun
    beginning-of-line
    beginning-of-thing
    boundp
    bounds-of-thing-at-point
    browse-url-encode-url
    buffer-end
    buffer-file-name
    buffer-list
    buffer-live-p
    buffer-modified-p
    buffer-name
    buffer-read-only
    buffer-string
    buffer-substring
    buffer-substring-no-properties
    c-end-of-defun
    caar
    cadr
    called-interactively-p
    capitalize
    car
    car-safe
    case
    catch
    cdar
    cddr
    cdr
    cdr-safe
    ceiling
    char-displayable-p
    char-to-string
    check-parens
    cl-copy-list
    cl-find
    cl-loop
    cl-member
    cl-remove-if
    cl-signum
    comment-region
    compare-strings
    compilation-buffer-internal-p
    completing-read
    con
    concat
    concatenate
    cond
    condition-case
    cons
    consp
    copy-sequence
    count
    count-if
    current-buffer
    current-column
    current-prefix-arg
    current-time
    current-time-string
    date-to-time
    decf
    default-directory
    directory-file-name
    directory-files
    dired-get-filename
    dired-next-line
    display-graphic-p
    dolist
    dotimes
    down-list
    downcase
    elt
    emacs-uptime
    end-of-defun
    end-of-line
    end-of-thing
    eobp
    eolp
    eq
    equal
    error
    error-message-string
    executable-find
    expand-file-name
    fboundp
    file-attributes
    file-directory-p
    file-exists-p
    file-expand-wildcards
    file-name
    file-name-absolute-p
    file-name-as-directory
    file-name-base
    file-name-directory
    file-name-extension
    file-name-nondirectory
    file-name-sans-extension
    file-readable-p
    file-regular-p
    file-relative-name
    file-remote-p
    file-writable-p
    find-if
    first
    float-time
    floor
    for
    format
    format-mode-line
    format-time-string
    forward-char
    forward-line
    forward-list
    forward-sexp
    frame-first-window
    frame-parameter
    frame-width
    fresets
    functionp
    get
    get-buffer-process
    get-buffer-window
    get-buffer-window-list
    get-char-property
    getenv
    gethash
    goto-char
    goto-line
    if
    ignore-errors
    int-to-string
    integer-or-marker-p
    integerp
    interactive
    jabber-muc-sender-p
    json-encode
    json-encode-alist
    json-encode-string
    json-join
    kbd
    key-binding
    keywordp
    lambda
    length
    let
    let*
    line-beginning-position
    line-end-position
    line-number-at-pos
    list
    list-system-processes
    listify-key-sequence
    listp
    litable-create-fake-cursor-at-point
    local-key-binding
    log
    looking-at
    looking-back
    loop
    make-hash-table
    make-marker
    make-overlay
    make-sparse-keymap
    make-string
    make-symbol
    mark
    mark-marker
    mark-sexp
    match-beginning
    match-data
    match-data-list
    match-end
    match-string
    match-string-no-properties
    max
    member
    memq
    message
    mew-summary-display
    min
    minibufferp
    minor-mode-key-binding
    mode-line-eol-desc
    move-beginning-of-line
    move-end-of-line
    move-overlay
    not
    nth
    null
    number-or-marker-p
    number-to-string
    numberp
    one-window-p
    or
    overlay-end
    overlay-get
    overlay-put
    overlay-start
    overlays
    overlays-at
    overlays-in
    paredit-backward-up
    plist-get
    point
    point-max
    point-min
    pp-to-string
    princ
    print
    process-attributes
    process-get
    process-status
    progn
    propertize
    quote
    random
    rassoc
    re-search-backward
    re-search-forward
    regexp-opt
    regexp-quote
    region-active-p
    region-beginning
    region-end
    remove-duplicates
    remove-if
    remove-if-not
    remove-overlays
    replace-regexp-in-string
    replace-string
    reverse
    save-current-buffer
    save-excursion
    save-match-data
    save-restriction
    secure-hash
    set
    set-buffer
    set-buffer-modified-p
    setf
    setq
    sexp-at-point
    signal
    sin
    skip-chars-backward
    skip-chars-forward
    split-string
    string
    string-equal
    string-lessp
    string-match
    string-match-p
    string-prefix-p
    string-to-char
    string-to-list
    string-to-number
    string<
    string=
    stringp
    strings
    subrp
    substring
    substring-no-properties
    symbol-function
    symbol-macrolet
    symbol-name
    symbol-regexp
    symbol-value
    symbolp
    tan
    text-properties-at
    thing-at-point
    thing-at-point-looking-at
    this-command-keys
    throw
    time-since
    time-to-seconds
    type-of
    unless
    unwind-protect
    upcase
    url-hexify-string
    use-region-p
    user-full-name
    variable-at-point
    variables
    vector
    verify-visited-file-modtime
    version-to-list
    warn
    when
    while
    window-list
    window-live-p
    window-start
    window-width
    with-current-buffer
    with-output-to-string
    with-temp-buffer
    y-or-n-p
    yes-or-no-p
    zerop
    )
  "List of additional function considered pure (and thus safe) by litable.

Litable will only execute functions marked as pure by the
byte-compiler, or whitelisted here. See `literable--pure-p'.

Functions that take predicates as arguments (such as `remove-if')
are included here even though they aren't necessarily pure. That
is because we assume the predicate will be a pure function.

Functions that evaluate arbitrary code (eval, apply, funcall) are
NOT included here and should never be.")

(if (file-exists-p litable-list-file)
    (load litable-list-file t)
  (litable--save-lists))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check for impure functions
(defvar litable--impure-found nil "Used to keep track of impure functions found.")

(defun litable-accept-as-pure (batch)
  "Saves as pure the currently found impure functions.

Asking for confirmation, adds each impure function found to
`litable-pure-functions-list' (and saves the list).

With BATCH prefix argument, asks only once for all."
  (interactive "P")
  (if batch
      (when (y-or-n-p (format "Save ALL these functions as pure? %s" litable--impure-found))
        (mapc (lambda (x) (add-to-list 'litable-pure-functions-list x))
              litable--impure-found))
   (dolist (cur litable--impure-found)
     (when (y-or-n-p (format "Save %s as pure?" cur))
       (add-to-list 'litable-pure-functions-list cur))))
  (litable--save-lists))

(defun litable--pure-at-point ()
  "Return symbol at point if it's a pure function."
  (let ((sym (symbol-at-point)))
    (when (and
           (functionp sym)
           (-contains-p litable-pure-functions-list sym))
      sym)))

(defun litable-remove-from-pure-list ()
  "Remove a function from the pure functions list.

Provide completion for the content of `litable-pure-functions-list'
using `completing-read', remove the selected candidate and save the list."
  (interactive)
  (if (null litable-pure-functions-list)
      (message "No function trusted to be pure.")
    (let ((symbol
           (completing-read "Pure function: "
                            litable-pure-functions-list
                            nil t
                            (if-let (sym-at-pt (litable--pure-at-point))
                                (symbol-name sym-at-pt) nil))))
      (setq litable-pure-functions-list
            (delq (intern symbol) litable-pure-functions-list))
      (litable--save-lists))))

(defun litable--pure-p (fn-symbol)
  "Return non-nil if fn-symbol is a pure function."
  (and (fboundp fn-symbol)
       (or
        (get fn-symbol 'side-effect-free)
        (member fn-symbol litable-pure-functions-list))))

(defun litable--safe-eval (form)
  "Check if FORM contains only known pure functions and eval it.

If it doesn't, don't eval and return a warning string.
Functions can be accepted as pure with `litable-accept-as-pure'."
  ;; We'll keep track of whether an impure function was found.
  ;; This is a setq instead of a let-form, because this way we can use
  ;; this information interactively in `litable-accept-as-pure'.
  (setq litable--impure-found nil)
  (litable--deep-search-for-impures form)
  ;; If it was, we report
  (if litable--impure-found
      (format "Unsafe functions: %S" litable--impure-found)
    ;; If it wasn't, we evaluate as expected
    (eval form)))

(defun litable--deep-search-for-impures (form)
  "Check whether all car's inside FORM are pure.

If any isn't a pure function, reports in the variable `litable--impure-found'."
  ;; It's possible we got passed a nil form, if so just ignore it.
  (when form
    (if (not (listp form))
        ;; If it's not a list, it is the function name
        (unless (litable--pure-p form)
          (add-to-list 'litable--impure-found form))
      ;; If it's a list, it is the entire function call. Check the name,
      ;; and search the arguments for more function calls. Plain
      ;; arguments don't get checked.
      (let ((function (car form))
            (rest (cdr form)))
        (cond
         ;; If it's a let form, try to do the right thing about its
         ;; arguments.
         ((string-match "\\`let\\*?\\'" (symbol-name function))
          (dolist (cur (car rest))       ;For each item in the first form
            (when (and (listp cur) (listp (cadr cur))) ;If the variable's value is set with a function call
              (litable--deep-search-for-impures (cadr cur)))) ;Check the function call
          (dolist (cur (cdr rest)) ;Then check the actual content of the let form
            (when (listp cur) (litable--deep-search-for-impures cur))))
         ;; If it's a lambda, we can skip the first argument (it's the
         ;; argument list) but we need to check the rest.
         ((eq function 'lambda)
          (dolist (cur (cdr rest))
            (when (listp cur) (litable--deep-search-for-impures cur))))
         ;; Anything inside a quote is considered safe, because
         ;; anything that could evaluate it (eval, funcall, etc) is
         ;; considered unsafe. The exception are functions with
         ;; predicates (remove-if), but we assume these functions will
         ;; use plain lambdas instead of quotes.
         ((eq function 'quote) nil)
         ((eq function 'function) nil)
         ;; A ` is similar to a quote, except we need to check the evaluated arguments
         ((eq function '\`)
          (dolist (cur (car rest)) ;; rest is a 1-element list. This 1 element is a list contain all the arguments to the `.
            (when (listp cur) (litable--deep-search-for-commas cur))))
         ;; Anything else is a typical function, just check the name and
         ;; the arguments.
         (t
          (litable--deep-search-for-impures function)
          (dolist (cur rest)
            (when (listp cur) (litable--deep-search-for-impures cur)))))))))

;; TODO: This is not being used right now. For some reason the
;; evaluator doesn't even get called when there's a "," in the form.
;; Is this intentional?
(defun litable--deep-search-for-commas (form)
  "Deep search in form for a \",\". When found, pass its argument to `litable--deep-search-for-impures'. "
  (when (listp form)
    (let ((function (car form))
          (rest (cdr form)))
      (if (eq function '\,)
          (litable--deep-search-for-impures rest)
        (dolist (cur rest)
          (when (listp cur) (litable--deep-search-for-commas cur)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing the result

;; TODO: both print-result and print-input should accumulate the
;; results in a variable (for each defun? -- alist?) and then only
;; print int in single overlay after all the updating is done
;; (i.e. when the final result is printed as well). Right now, each
;; input/output has its own overlay.

;; TODO: shorten the result if too long? Add customize limit for
;; cut-off. Maybe echo the full thing in the echo area/print in msg
;; log <- maybe not a good idea, it will produce tons of spam.
(defun litable--print-result (result pos face)
  "Print the RESULT of evaluating form at POS.
Fontify the result using FACE."
  (let* ((o (make-overlay pos pos))
         (print-quoted t)
         (s (format litable-result-format (funcall litable-print-function result))))
    (push o litable-overlays)
    (litable--set-result-overlay-priority o)
    (put-text-property 0 1 'cursor t s)
    (overlay-put o
                 'before-string
                 (funcall litable-result-overlay-text-function s face))))

(defun litable--create-result-overlay-text (s &optional face)
  "Create the text for the overlay that shows the result."
  (format "%s%s" " " (propertize s 'face (or face 'litable-result-face))))

(defun litable--print-input (input pos face)
  "Print the INPUT for the evaluated form at POS.
Fontify the input using FACE."
  (let ((o (make-overlay pos pos))
        (print-quoted t))
    (push o litable-overlays)
    (litable--set-result-overlay-priority o)
    (overlay-put o
                 ;; TODO: add customize to reverse the order of input args. Now it
                 ;; resemples LIFO, should be FIFO!
                 ;;
                 ;; (defun foo (x) <= (- 10 5) <= 2
                 ;;   (1+ x)) => 6 => 3
                 ;;
                 ;; (+ (foo (- 10 5)) (foo 2) 3 4) => 16
                 ;; before-string <-> after-string
                 'before-string
                 (propertize
                  ;; TODO: extract this format into customize
                  (format " <= %s" (mapconcat litable-print-function input ", "))
                  'face face))))

(defun litable--create-substitution-overlay (start end value &optional face)
  "Create the overlay that shows the substituted value."
  ;; TODO: make the face customizable
  (setq face (or face 'litable-substitution-face))
  (let (o (print-quoted t))
    (setq o (make-overlay start end))
    (push o litable-overlays)
    (litable--set-overlay-priority o)
    (overlay-put o 'display
                 ;; TODO: customize max-length
                 ;; for the subexpression, then
                 ;; cut off and replace with
                 ;; "bla..."
                 (funcall
                  litable-substitution-overlay-text-function
                  ms
                  (funcall litable-print-function value)))))

(defun litable--create-substitution-overlay-text (exp value &optional face)
  "Create the text for the overlay that shows the substitution."
  (format "%s %s" exp (propertize value 'face (or face 'litable-substitution-face))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation

(defun litable--next-sexp ()
  (ignore-errors
    (forward-sexp))
  (ignore-errors
    (forward-sexp))
  (ignore-errors
    (backward-sexp)))

;; stolen from mastering emacs comments
(defun litable-backward-up-list ()
  "Stupid backward-up-list doesn't work from inside a string and
I got tired of having to move outside the string to use it."
  (interactive)
  (when (in-string-p)
    (while (in-string-p)
      (backward-char)))
  (backward-up-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; updating the overlays

(defun litable-goto-toplevel-form ()
  (while (/= (car (syntax-ppss)) 0) (litable-backward-up-list)))

(defun litable-update-defs (&optional a b c)
  (litable-remove-overlays)
  (when a
    (ignore-errors
      (let ((form (save-excursion
                    (litable-goto-toplevel-form)
                    (sexp-at-point))))
        (litable-find-function-subs-arguments form)))))

(defvar litable--current-end-position nil "End position of current top level sexp.")
(defvar litable--current-beginning-position nil "Beginning position of current top level sexp.")

(defun litable-update-defs-if-moved ()
  "Run `litable-update-defs' only if moved to a different toplevel sexp."
  (when litable-update-on-move
    (let ((beginning (save-excursion (ignore-errors (beginning-of-defun)) (point)))
          (end (save-excursion (ignore-errors (end-of-defun)) (point))))
      (unless (and litable--current-end-position
                   litable--current-beginning-position
                   (or (= litable--current-beginning-position beginning)
                       (= litable--current-end-position end)))
        (setq litable--current-end-position end)
        (setq litable--current-beginning-position beginning)
        (litable-update-defs 1)))))


(defun litable-refresh ()
  (interactive)
  (litable-update-defs 1))

;; TODO: if the same function is eval'd twice, also all the overlays
;; are created twice. Maybe we should keep an alist (defun . overlays
;; in defun) and reuse/update them.  But, until we hit performance
;; issues, doesn't matter -- for now fixed with priorities.
(defvar litable-overlays nil)

(defcustom litable-overlay-priority 0
  "Overlay priority"
  :type 'integer
  :group 'litable)

(defcustom litable-result-overlay-priority 0
  "Result overlay priority"
  :type 'integer
  :group 'litable)

(defcustom litable-update-on-move t
  "If non-nil, overlays are updated when point moves.

This allows the overlay to \"follow\" the point.

Independent of this variable, overlays are also updated when the
buffer is edited."
  :type 'boolean
  :group 'litable
  :package-version '(litable . "0.0.20130408"))

;; internal variables
(defvar litable--overlay-priority litable-overlay-priority)
(defvar litable--result-overlay-priority litable-result-overlay-priority)

(defun litable--set-overlay-priority (overlay)
  (setq litable--overlay-priority (1+ litable--overlay-priority))
  (overlay-put overlay 'priority litable--overlay-priority))

(defun litable--set-result-overlay-priority (overlay)
  (setq litable--result-overlay-priority (1+ litable--result-overlay-priority))
  (overlay-put overlay 'priority litable--result-overlay-priority))

(defun litable-remove-overlays ()
  (--each litable-overlays (delete-overlay it))
  (setq litable-overlays nil)
  (setq litable--overlay-priority litable-overlay-priority)
  (setq litable--result-overlay-priority litable-result-overlay-priority))

(defvar litable-mode-map (make-sparse-keymap)
  "litable mode map.")

(defcustom litable-mode-hook nil
  "Hook for `litable-mode'."
  :type 'hook
  :group 'litable)

(defun litable-init ()
  "Initialize litable in the buffer."
  (add-hook 'after-change-functions 'litable-update-defs nil t)
  (add-hook 'post-command-hook 'litable-update-defs-if-moved nil t)
  (make-local-variable 'litable--current-end-position)
  (make-local-variable 'litable--current-beginning-position)
  (litable-update-defs 1)
  (run-hooks 'litable-mode-hook))

(defun litable-stop ()
  "Stop litable in the buffer."
  (remove-hook 'after-change-functions 'litable-update-defs t)
  (remove-hook 'post-command-hook 'litable-update-defs-if-moved t)
  (litable-remove-overlays))

;;;###autoload
(define-minor-mode litable-mode
  "Toggle litable-mode"
  :lighter " litable"
  :keymap litable-mode-map
  :group 'litable
  (if litable-mode
      (litable-init)
    (litable-stop)))

(provide 'litable)

;;; litable.el ends here
