;;; litable.el --- dynamic evaluation replacement with emacs

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: lisp
;; Version: 0.0.20130407
;; Created: 8th April 2013
;; Package-requires: ((dash "1.1.0") (letcheck "0.2"))

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
(require 'letcheck)
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
         (cvars (letcheck-extract-variables (cadr let-form)))
         (nvars (litable--merge-variables
                 (litable--overlays-at point)
                 cvars))
         ov)
    (setq ov (make-overlay (car bounds) (cdr bounds)))
    (push ov litable-overlays)
    (overlay-put ov 'litable-let-form nvars)))

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

(defun litable--merge-variables (overlays varlist)
  "Merge the varlist with the variables stored in overlays."
  (let ((varlists (--map (overlay-get it 'litable-let-form) overlays)))
    (-distinct (-union (-flatten (-concat varlists)) varlist))))

(defun litable-get-let-bound-variables (&optional point)
  "Get a list of let-bound variables at POINT."
  (let ((active (litable--get-active-overlay point)))
    (when active
     (--map (symbol-name it) (overlay-get active 'litable-let-form)))))

(defun litable-annotate-let-forms (&optional point)
  "Annotate all let and let* forms in the defun at point."
  (setq point (or point (point)))
  (save-excursion
    (save-restriction
      (widen)
      (narrow-to-defun)
      (goto-char (point-min))
      (while (re-search-forward "(let\\*?" nil t)
        (save-excursion
          (goto-char (match-beginning 0))
          (litable--annotate-let-form))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; argument propagation in the defuns

(defun litable--make-subs-list (arg-names values)
  (let (r)
    (--each (-zip arg-names values)
      ;; do we want to eval here? Make it a customizable option!
      (!cons (cons (symbol-name (car it)) (eval (cdr it))) r))
    r))

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
              ;; build the symbol-name <-> value alist
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
                    (save-excursion
                      (litable-backward-up-list)
                      (let* ((s (sexp-at-point))
                             (ex-form (assq (car s) litable-exceptions)))
                        (when ex-form
                          (down-list)
                          (forward-sexp (cdr ex-form))
                          (when (>= (point) me)
                            (setq ignore t)))))
                    ;; test the let form
                    (let ((bound (litable-get-let-bound-variables)))
                      (when (member ms bound)
                        (setq ignore t)))
                    (unless ignore
                      (let (o)
                        (setq o (make-overlay mb me))
                        (push o litable-overlays)
                        (overlay-put o 'display
                                     (propertize
                                      ;; TODO: extract this format into customize
                                      (concat ms "{"
                                              (prin1-to-string (cdr (assoc ms subs))) "}")
                                      'face
                                      'font-lock-type-face))))
                    (setq ignore nil)))
                ;; if depth > 0 means we're updating a defun, print the
                ;; end result after the end of the defun
                (when (> depth 0)
                  (save-excursion
                    (end-of-defun)
                    (backward-char)
                    (let* ((ostart (point))
                           (o (make-overlay ostart ostart)))
                      (push o litable-overlays)
                      (overlay-put o
                                   'after-string
                                   (propertize
                                    ;; TODO: extract this format into customize
                                    (format " => %s" (eval form))
                                    'face 'font-lock-constant-face)))))))))))
    (when (and (= depth 0)
               (nth 1 (syntax-ppss)))
      (let* ((ostart (save-excursion (end-of-line) (point)))
             (o (make-overlay ostart ostart)))
        (push o litable-overlays)
        (overlay-put o
                     'after-string
                     (propertize
                      ;; TODO: extract this format into customize
                      (format " => %s" (eval form))
                      'face 'font-lock-warning-face))))))

;; UNUSED -- figure out a better way to do this
(defun litable-do-let-form-substitution (let-form)
  "Replace stuff in let form (let* does not work yet)."
  (save-excursion
    (save-restriction
      (widen)
      (narrow-to-region (point) (save-excursion (forward-sexp) (point)))
      (let ((varlist (letcheck-extract-variables (cadr let-form))))
        (while varlist
          (let* ((var (symbol-name (car varlist)))
                 (needle (concat "\\_<" (regexp-quote var) "\\_>"))
                 val sep)
            (re-search-forward needle nil t)
            (save-excursion
              (when (save-excursion
                      (my-backward-up-list)
                      (down-list)
                      (looking-at needle))
                (litable--next-sexp)
                (setq sep (sexp-at-point))
                ;; TODO: in this sexp, a value from above can be
                ;; technically substituted -- this is the let
                ;; definition
                (forward-sexp)
                ;;(setq val (eval sep))
                (setq val sep)
                (let (o ignore mb me)
                  (while (re-search-forward needle nil t)
                    (setq mb (match-beginning 0))
                    (setq me (match-end 0))
                    ;; figure out the context here. If the sexp we're in is
                    ;; on the exception list, move along. Maybe we shouldn't
                    ;; censor some results though. TODO: Meditate on this
                    (save-excursion
                      (my-backward-up-list)
                      (let* ((s (sexp-at-point))
                             (ex-form (assq (car s) litable-exceptions)))
                        (when ex-form
                          (down-list)
                          (forward-sexp (cdr ex-form))
                          (when (>= (point) me)
                            (setq ignore t)))))
                    (when (not ignore)
                      (setq o (make-overlay mb me))
                      (push o litable-overlays)
                      (overlay-put o 'display
                                   (propertize
                                    ;; extract this format into customize
                                    (concat var "{"
                                            (prin1-to-string val) "}")
                                    'face
                                    'font-lock-type-face)))
                    (setq ignore nil)))))
            (!cdr varlist)))))))


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

(defvar litable-overlays nil)

(defun litable-remove-overlays ()
  (--each litable-overlays (delete-overlay it))
  (setq litable-overlays nil))

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
