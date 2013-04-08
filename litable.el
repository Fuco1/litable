;;; litable.el --- dynamic evaluation replacement with emacs

;; Copyright (C) 2013  Fuco

;; Author: Fuco
;; Keywords: lisp
;; Version: 0.0.20130407
;; Created: 8th April 2013
;; Package-requires: ((dash "1.0.3"))

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

(defun litable-find-function-subs-arguments (form)
  ;; first thing in form is the function name
  (let ((name (symbol-name (car form)))
        (cur-param-index 1)
        args cur-param-name)
    (save-excursion
      (goto-char 1)
      (re-search-forward (concat "(defun " name))

      ;; replace the arguments. The next form is the argument form
      (forward-list) (backward-list)
      (setq args (sexp-at-point))
      (save-restriction
        (widen)
        (narrow-to-defun)
        (while args
          (goto-char (point-min))
          (setq cur-param-name (symbol-name (car args)))
          (when (equal cur-param-name "&optional")
            (!cdr args)
            (setq cur-param-name (symbol-name (car args))))
          (let (o)
            (while (re-search-forward (concat "\\_<" cur-param-name "\\_>") nil t)
              (setq o (make-overlay (match-beginning 0) (match-end 0)))
              (push o dyneval-overlays)
              (overlay-put o 'display (prin1-to-string (nth cur-param-index form)))))
          (!cdr args)
          (setq cur-param-index (1+ cur-param-index)))))
    (let ((ostart (save-excursion (end-of-line) (point))))
      (setq dyneval-result-overlay (make-overlay ostart ostart))
      (overlay-put dyneval-result-overlay
                   'after-string
                   (propertize
                    (format " => %s" (eval form))
                    'face 'font-lock-warning-face)))))

(defun litable-update-defs (&optional a b c)
  (litable-remove-overlays)
  (when a
    (ignore-errors
      (let ((form (save-excursion
                    (while (/= (car (syntax-ppss)) 0) (litable-backward-up-list))
                    (sexp-at-point))))
        (litable-find-function-subs-arguments form)))))

;; stolen from mastering emacs comments
(defun litable-backward-up-list ()
  "Stupid backward-up-list doesn't work from inside a string and
I got tired of having to move outside the string to use it."
  (interactive)
  (if (in-string-p)
      (while (in-string-p)
        (backward-char))
    (backward-up-list)))

(defvar dyneval-overlays nil)

(defvar dyneval-result-overlay nil)

(defun litable-remove-overlays ()
  (--each dyneval-overlays (delete-overlay it))
  (when dyneval-result-overlay
    (delete-overlay dyneval-result-overlay)
    (setq dyneval-result-overlay nil)))

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
