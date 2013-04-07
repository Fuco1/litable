;; WIP!!

(require 'dash)

(defun my-find-function-subs-arguments (form)
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
  (if (in-string-p)
      (while (in-string-p)
        (backward-char))
    (backward-up-list)))

(defvar dyneval-overlays nil)

(defvar dyneval-result-overlay nil)

(defun my-remove-overlays ()
  (--each dyneval-overlays (delete-overlay it))
  (when dyneval-result-overlay
    (delete-overlay dyneval-result-overlay)
    (setq dyneval-result-overlay nil)))

;; run this on the buffer where you want this functionality.
;; TODO: make it into a function/minor mode
;;
;; (add-hook 'after-change-functions 'my-update-defs nil t)
;; (remove-hook 'after-change-functions 'my-update-defs t)
