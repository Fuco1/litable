;; WIP!!

(require 'dash)

(defvar litable-exceptions '(
                             (setq . 2)
                             )
  "A list of cons pairs (form-name . nth argument) where the
substitution should not occur.  The number includes the first
item, counting starts at 1.

For example:

  (setq . 2) ;; first argument is target name, do not substitute.")

;; TODO: add better recursive evaluation
(defun my-find-function-subs-arguments (form &optional depth)
  ;; first thing in form is the function name
  (setq depth (or depth 0))
  (let ((name (symbol-name (and (listp form) (car form))))
        (cur-param-index 1)
        args cur-param-name)
    (when name
      (save-excursion
        (save-restriction
          (widen)
          (goto-char 1)
          (when (re-search-forward (regexp-quote (concat "(defun " name)) nil t)
            ;; replace the arguments. The next form is the argument form
            (forward-list) (backward-list)
            (setq args (sexp-at-point))
            (save-restriction
              (widen)
              (narrow-to-defun)
              (while args
                (goto-char (point-min))
                (setq cur-param-name (symbol-name (car args)))
                ;; add support for &rest too. What about keywords?
                (when (equal cur-param-name "&optional")
                  (!cdr args)
                  (setq cur-param-name (symbol-name (car args))))
                ;; also augument the current form?
                (my-find-function-subs-arguments (nth cur-param-index form) (1+ depth))
                (let (o ignore mb me)
                  (while (re-search-forward (concat "\\_<" (regexp-quote cur-param-name) "\\_>") nil t)
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
                      (push o dyneval-overlays)
                      (overlay-put o 'display
                                   (propertize
                                    (prin1-to-string (nth cur-param-index form))
                                    'face
                                    'font-lock-type-face)))
                    (setq ignore nil)))
                (!cdr args)
                (setq cur-param-index (1+ cur-param-index))))))))
    (when (and (= depth 0)
               (nth 1 (syntax-ppss)))
      (let ((ostart (save-excursion (end-of-line) (point))))
        (setq dyneval-result-overlay (make-overlay ostart ostart))
        (overlay-put dyneval-result-overlay
                     'after-string
                     (propertize
                      (format " => %s" (eval form))
                      'face 'font-lock-warning-face))))))

(defun my-update-defs (&optional a b c)
  (my-remove-overlays)
  (when a
    (ignore-errors
      (let ((form (save-excursion
                    (while (/= (car (syntax-ppss)) 0) (my-backward-up-list))
                    (sexp-at-point))))
        (my-find-function-subs-arguments form)))))

;; stolen from mastering emacs comments
(defun my-backward-up-list ()
  "Stupid backward-up-list doesn't work from inside a string and
I got tired of having to move outside the string to use it."
  (interactive)
  (when (in-string-p)
    (while (in-string-p)
      (backward-char)))
  (backward-up-list))

(defvar dyneval-overlays nil)

(defvar dyneval-result-overlay nil)

(defun my-remove-overlays ()
  (--each dyneval-overlays (delete-overlay it))
  (setq dyneval-overlays nil)
  (when dyneval-result-overlay
    (delete-overlay dyneval-result-overlay)
    (setq dyneval-result-overlay nil)))

;; run this on the buffer where you want this functionality.
;; TODO: make it into a function/minor mode
;;
;; (add-hook 'after-change-functions 'my-update-defs nil t)
;; (remove-hook 'after-change-functions 'my-update-defs t)
