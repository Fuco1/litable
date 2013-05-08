;;; litable.el --- dynamic evaluation replacement with emacs

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: lisp
;; Version: 0.0.20130408
;; Created: 8th April 2013
;; Package-requires: ((dash "1.1.0"))

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
;; fake eval (eval with enviroment)

(defun litable--fake-eval (form enviroment &optional type)
  "Evaluate the FORM in ENVIROMENT using the enviroment binding of TYPE.

TYPE can be a symbol `let' or `let*'."
  (setq type (or type 'let))
  (ignore-errors (eval `(,type ,enviroment ,form))))

(defun litable--alist-to-list (alist)
  "Change (a . b) into (a b)"
  (--map (list (car it) (cdr it)) alist))

(defun litable--merge-variables (overlay subs varlist)
  "Merge the varlist with the variables stored in overlays.

This will also evaluate the newly-bound variables."
  (let* ((pvars (or (and overlay (overlay-get overlay 'litable-let-form-cur)) subs))
         (enviroment (litable--alist-to-list pvars)))
    ;; TODO: THIS DOESN'T WORK WITH let*!! We need to update the
    ;; bindings one by one in that case, and merge after each update.
    (litable--alist-merge
     pvars
     (mapcar (lambda (it)
               (cons (car it)
                     (litable--fake-eval (cadr it) enviroment 'let)))
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
  (concat "\\_<"
          (regexp-opt (--map (regexp-quote (symbol-name it)) variables))
          "\\_>"))

;; - maybe add different colors for different arguments that get
;;   substituted. This might result in rainbows sometimes, maybe
;;   undersirable
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
                           mb me (cdr (assoc (intern ms) vars)) 'font-lock-warning-face))))
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
                    ;; TODO: make the face customizable
                    (litable--print-result (eval form) (point) 'font-lock-constant-face)))
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
        ;; TODO: make the face customizable
        (litable--print-result (eval form) ostart 'font-lock-warning-face)))))

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
         (s (format " => %s" result)))
    (push o litable-overlays)
    (litable--set-result-overlay-priority o)
    (put-text-property 0 1 'cursor t s)
    (overlay-put o
                 'before-string
                 (propertize
                  ;; TODO: extract this format into customize
                  s
                  'face face))))

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
                  (format " <= %s" (mapconcat 'prin1-to-string input ", "))
                  'face face))))

(defun litable--create-substitution-overlay (start end value &optional face)
  "Create the overlay that shows the substituted value."
  ;; TODO: make the face customizable
  (setq face (or face 'font-lock-type-face))
  (let (o (print-quoted t))
    (setq o (make-overlay start end))
    (push o litable-overlays)
    (litable--set-overlay-priority o)
    (overlay-put o 'display
                 (propertize
                  ;; TODO: extract this format into customize
                  ;; TODO: customize max-length
                  ;; for the subexpression, then
                  ;; cut off and replace with
                  ;; "bla..."
                  (concat ms "{"
                          (prin1-to-string value) "}")
                  'face face))))


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
  "Restult overlay priority"
  :type 'integer
  :group 'litable)

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
  (run-hooks 'litable-mode-hook))

(defun litable-stop ()
  "Stop litable in the buffer."
  (remove-hook 'after-change-functions 'litable-update-defs t)
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
