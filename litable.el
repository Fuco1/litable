;; WIP!! someone clean this up (= me :/)

;; https://github.com/magnars/dash.el
;; is on melpa/marmalade already
(require 'dash)

;; https://github.com/Fuco1/letcheck
;; we should package it on melpa/marmalade
(require 'letcheck)

(defvar litable-exceptions '(
                             (setq . 2)
                             )
  "A list of cons pairs (form-name . nth argument) where the
substitution should not occur.  The number includes the first
item, counting starts at 1.

For example:

  (setq . 2) ;; first argument is target name, do not substitute.")

;; TODO:
;; - split this monster into managable pieces!
;;
;; - maybe add different colors for different arguments that get
;;   substituted. This might result in rainbows sometimes, maybe
;;   undersirable
(defun my-find-function-subs-arguments (form &optional depth)
  ;; first thing in form is the function name
  (setq depth (or depth 0))
  (let* ((symbol (and (listp form) (car form)))
         (name (symbol-name symbol))
        (cur-param-index 1)
        args cur-param-name)
    (when symbol
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
                    ;; are we inside a let form? If so, we need to get
                    ;; the correct value for the argument. For now,
                    ;; just substitute the values inside the let if
                    ;; the variable is the same as the argument. Do we
                    ;; want to resolve the let forms always? Might be
                    ;; useful, but adds confision and the definitions
                    ;; can't be eval'd anyway, so it'd just copy forms
                    ;; = ugly. Maybe don't even do anything (add a customize?)
                    ;; depends on `letcheck' library (see top of the file)
                    (save-excursion
                      ;; this jumps in front
                      (let* ((let-form (letcheck-get-let-form))
                             (varlist (and let-form (letcheck-extract-variables (cadr let-form)))))
                        (when (and let-form
                                   (member cur-param-name (mapcar 'symbol-name varlist)))
                          (setq ignore t)
                          ;; instead do the let-form substitution.
                          (litable-do-let-form-substitution let-form)
                          (forward-sexp))))
                    (when (not ignore)
                      (setq o (make-overlay mb me))
                      (push o dyneval-overlays)
                      (overlay-put o 'display
                                   (propertize
                                    ;; TODO: extract this format into customize
                                    (concat cur-param-name "{"
                                            (prin1-to-string (nth cur-param-index form)) "}")
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
                      ;; TODO: extract this format into customize
                      (format " => %s" (eval form))
                      'face 'font-lock-warning-face))))))

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
                      (push o dyneval-overlays)
                      (overlay-put o 'display
                                   (propertize
                                    ;; extract this format into customize
                                    (concat var "{"
                                            (prin1-to-string val) "}")
                                    'face
                                    'font-lock-type-face)))
                    (setq ignore nil)))))
            (!cdr varlist)))))))

(defun litable--next-sexp ()
  (ignore-errors
    (forward-sexp))
  (ignore-errors
    (forward-sexp))
  (ignore-errors
    (backward-sexp)))

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
