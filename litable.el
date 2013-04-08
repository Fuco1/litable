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

(defun litable--annotate-let-form (&optional point)
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
         (cvars (litable--extract-variables (cadr let-form))) ; vars defined in current form
         (pvars (litable-get-let-bound-variables point t)) ; vars defined in the very previous form
         (nvars (litable--merge-variables ; merged vars
                 (litable--overlays-at point)
                 cvars))
         ov)
    (setq ov (make-overlay (car bounds) (cdr bounds)))
    (push ov litable-overlays)
    (overlay-put ov 'litable-let-form-type t)
    (overlay-put ov 'litable-let-form nvars)
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

(defun litable--merge-variables (overlays varlist)
  "Merge the varlist with the variables stored in overlays."
  (let ((varlists (--map (overlay-get it 'litable-let-form) overlays)))
    (-distinct (-union (-flatten (-concat varlists)) varlist))))

(defun litable-get-let-bound-variables (&optional point symbols)
  "Get a list of let-bound variables at POINT."
  (let ((active (litable--get-active-overlay point)))
    (when active
      (--map (if symbols it (symbol-name it)) (overlay-get active 'litable-let-form)))))

(defun litable-get-let-bound-parent-variables (&optional point)
  "Get a list of let-bound variables in the parent form at POINT."
  (let ((active (litable--get-active-overlay point)))
    (when active
     (--map (symbol-name it) (overlay-get active 'litable-let-form-prev)))))

(defun litable-annotate-let-forms (&optional point)
  "Annotate all let and let* forms in the defun at point."
  (setq point (or point (point)))
  (save-excursion
    (save-restriction
      (widen)
      (narrow-to-defun)
      (remove-overlays (point-min) (point-max) 'litable-let-form-type t)
      (goto-char (point-min))
      (while (re-search-forward "(let\\*?" nil t)
        (save-excursion
          (goto-char (match-beginning 0))
          (litable--annotate-let-form))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; argument propagation in the defuns

(defun litable--make-subs-list (arg-names values)
  "Return the list of cons pairs with symbol name in car and value in cdr."
  (let (r)
    (--each (-zip arg-names values)
      ;; do we want to eval here? TODO: Make it a customizable option!
      (!cons (cons (symbol-name (car it)) (eval (cdr it))) r))
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

;; - maybe add different colors for different arguments that get
;;   substituted. This might result in rainbows sometimes, maybe
;;   undersirable
(defun litable-find-function-subs-arguments (form &optional depth)
  "Find the definition of \"form\" and substitute the arguments.

If depth = 0, also evaluate the current form and print the result."
  (setq depth (or depth 0))
  (let* ((symbol (and (listp form) (car form)))
         (name (and (symbolp symbol) (symbol-name symbol)))
         subs args needle)
    (when (and symbol
               (symbolp symbol))
      ;; recursively evaluate the arguments first
      (--each (cdr form) (litable-find-function-subs-arguments it (1+ depth)))
      (when (not (subrp (symbol-function symbol)))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char 1)
            (when (re-search-forward (regexp-quote (concat "(defun " name)) nil t)
              (forward-list) (backward-list)
              (setq args (->> (sexp-at-point)
                           (delete '&optional)
                           (delete '&rest)))
              (setq subs (litable--make-subs-list args (cdr form)))
              (save-restriction
                (narrow-to-defun)
                (litable-annotate-let-forms)
                (setq needle
                      (concat "\\_<"
                              (regexp-opt (--map (regexp-quote (symbol-name it)) args))
                              "\\_>"))
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
                    (let ((bound (litable-get-let-bound-variables))
                          (bound-p (litable-get-let-bound-parent-variables))
                          (in-var-form (litable--in-var-form-p)))
                      (when (and (member ms bound)
                                 (not (and (not (member ms bound-p))
                                           in-var-form
                                           ;; we can still be at the "definition"
                                           ;; instance, that is: (>x< (blabla x)). This
                                           ;; should not get replaced.
                                           (/= me (save-excursion
                                                    (litable-backward-up-list)
                                                    (down-list)
                                                    (forward-sexp)
                                                    (point))))))
                        (setq ignore t)))
                    (unless ignore
                      (let (o)
                        (setq o (make-overlay mb me))
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
                                              (prin1-to-string (cdr (assoc ms subs))) "}")
                                      'face
                                      ;; TODO: make the face customizable
                                      'font-lock-type-face))))
                    (setq ignore nil)))
                ;; if depth > 0 means we're updating a defun, print the
                ;; end result after the end of the defun
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
      (let ((ostart (save-excursion (end-of-line) (point))))
        ;; TODO: make the face customizable
        (litable--print-result (eval form) ostart 'font-lock-warning-face)))))

;; TODO: shorten the result if too long? Add customize limit for
;; cut-off. Maybe echo the full thing in the echo area/print in msg
;; log <- maybe not a good idea, it will produce tons of spam.
(defun litable--print-result (result pos face)
  "Print the RESULT of evaluating form at POS.
Fontify the result using FACE."
  (let ((o (make-overlay pos pos)))
    (push o litable-overlays)
    (litable--set-result-overlay-priority o)
    (overlay-put o
                 'before-string
                 (propertize
                  ;; TODO: extract this format into customize
                  (format " => %s" result)
                  'face face))))

(defun litable--print-input (input pos face)
  "Print the INPUT for the evaluated form at POS.
Fontify the input using FACE."
  (let ((o (make-overlay pos pos)))
    (push o litable-overlays)
    (litable--set-result-overlay-priority o)
    (overlay-put o
                 'before-string
                 (propertize
                  ;; TODO: extract this format into customize
                  (format " <= %s" (mapconcat 'prin1-to-string input ", "))
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

(defun litable-update-defs (&optional a b c)
  (litable-remove-overlays)
  (when a
    (ignore-errors
      (let ((form (save-excursion
                    (while (/= (car (syntax-ppss)) 0) (litable-backward-up-list))
                    (sexp-at-point))))
        (litable-find-function-subs-arguments form)))))

;; TODO: if the same function is eval'd twice, also all the overlays
;; are created twice. Maybe we should keep an alist (defun . overlays
;; in defun) and reuse/update them.  But, until we hit performance
;; issues, doesn't matter -- for now fixed with priorities.
(defvar litable-overlays nil)

(defvar litable-overlay-priority 0)

(defvar litable-result-overlay-priority 0)

(defun litable--set-overlay-priority (overlay)
  (setq litable-overlay-priority (1+ litable-overlay-priority))
  (overlay-put overlay 'priority litable-overlay-priority))

(defun litable--set-result-overlay-priority (overlay)
  (setq litable-result-overlay-priority (1+ litable-result-overlay-priority))
  (overlay-put overlay 'priority litable-result-overlay-priority))

(defun litable-remove-overlays ()
  (--each litable-overlays (delete-overlay it))
  (setq litable-overlays nil)
  (setq litable-overlay-priority 0)
  (setq litable-result-overlay-priority 0))

;; TODO: make into minor-mode
(defun litable-init ()
  "Initialize litable in the buffer."
  (interactive)
  (add-hook 'after-change-functions 'litable-update-defs nil t))

(defun litable-stop ()
  "Stop litable in the buffer."
  (interactive)
  (remove-hook 'after-change-functions 'litable-update-defs t))


(provide 'litable)

;;; litable.el ends here
