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

(defcustom litable-print-function 'pp-to-string
  "Function used to print results and inputs"
  :type '(choice
	  (function-item :tag "pp-to-string" :value  pp-to-string)
	  (function-item :tag "prin1-to-string"
			 :value  prin1-to-string)
	  (function :tag "Your own function"))
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
        ;; TODO: make the face customizable
        (litable--print-result (eval form) ostart 'font-lock-warning-face)))))

(defun litable--safe-eval (form)
  "Check if FORM contains only known pure functions and eval it.

If it doesn't, don't eval and return a warning."
  ;; We'll keep track of whether an impure function was found.
  (let ((litable--impure-found nil))
    (litable--deep-search-for-impures form)
    ;; If it was, we report
    (if litable--impure-found
        (format "Unsafe functions: %S" litable--impure-found)
      ;; If it wasn't, we evaluate as expected
      (eval form))))

;;; TODO: Maybe this should be a hash table? It needs to be looked up
;;; a lot, so might be a good idea.
(defvar litable-pure-functions-list '(- --each -zip LaTeX-back-to-indentation LaTeX-current-environment LaTeX-default-environment LaTeX-default-style LaTeX-find-matching-begin LaTeX-mark-environment TeX-active-master TeX-check-files TeX-fold-mode TeX-normal-mode TeX-output-extension abbreviate-file-name abs activate-mark add-text-properties alist and append aref assoc assq back-to-indentation backward-char backward-list backward-sexp backward-up-list backward-word beginning-of-buffer beginning-of-defun beginning-of-line beginning-of-thing boundp bounds-of-thing-at-point browse-url-encode-url buffer-end buffer-file-name buffer-list buffer-live-p buffer-modified-p buffer-name buffer-read-only buffer-string buffer-substring buffer-substring-no-properties c-end-of-defun caar cadr called-interactively-p capitalize car car-safe case catch cdar cddr cdr cdr-safe ceiling char-displayable-p char-to-string check-parens cl-copy-list cl-find cl-loop cl-member cl-remove-if cl-signum comment-region compare-strings compilation-buffer-internal-p completing-read concat concatenate cond condition-case cons consp copy-sequence count count-if current-buffer current-column current-prefix-arg current-time current-time-string date-to-time decf default-directory directory-file-name directory-files dired-get-filename dired-next-line display-graphic-p dolist dotimes down-list downcase elt emacs-uptime end-of-defun end-of-line end-of-thing eobp eolp eq equal error error-message-string executable-find expand-file-name fboundp file-attributes file-directory-p file-exists-p file-expand-wildcards file-name file-name-as-directory file-name-base file-name-directory file-name-extension file-name-nondirectory file-name-sans-extension file-relative-name find-if first float-time floor for format format-mode-line format-time-string forward-char forward-line forward-list forward-sexp frame-first-window frame-parameter frame-width fresets functionp get get-buffer-process get-buffer-window get-buffer-window-list get-char-property getenv gethash goto-char goto-line grep-command grep-find grep-find-command grep-find-template grep-find-use-xargs grep-highlight-matches grep-template grep-use-null-device group gtalk gtalk-once gtt--format-and-hexify guess-class-name gz h hText hash-table have-timeout header heading height help-window helpString here hide-subtree hungry-delete-forward i i\.e\. id ido-completing-read ido-switch-buffer if ignore ignore-errors imap-queue-folder imap-server imap-size imap-ssl imap-ssl-port imap-trash-folder imap-user immediately implicit in in-math in-var-form inAgenda inc-guard-base incf include-guard including indent indent-for-comment indent-for-tab-command indent-region info-path init/notify initial-buffer input insert insert-file-contents insert-file-contents-literally insert-kbd-macro insert-snippet-by-name instead int-to-string integerp interactive intern intern-soft interprogram-cut-function interprogram-paste isInBlock isearch-exit it item j jab/activity-make-string jab/chat-camila jab/focus-next-chat jabber-activity-clean jabber-activity-find-buffer-name jabber-activity-make-string-default jabber-chat-buffer-show-avatar jabber-chat-error jabber-chat-prompt-foreign jabber-chat-prompt-local jabber-chat-text-foreign jabber-chat-text-local jabber-chat-with jabber-connect-all jabber-fix-status jabber-jid-displayname jabber-jid-resource jabber-jid-symbol jabber-jid-user jabber-message-wave jabber-post-connect-hooks jabber-propertize jabber-read-account jabber-title-large jabber-title-medium java/find-id-definition java/previous-end-of-statement jde-global-classpath jde-import-all jde-import-kill-extra-imports jde-import-organize jde-jdk-registry jid jids json-encode json-encode-alist json-encode-string json-join jump-to-register k kadr kar kbd kdr keep key key-binding key-chord-define key-chord-define-global keyboard-translate keymap keywordp kill-buffer kill-buffer-if-not-modified kill-line kill-new kill-process kill-region kill-ring kill-ring-save kill-ring-yank-pointer kmacro-exec-ring-item known-package-names kw l l1 l2 lambda lang latex latex-extra latex-functions latex//find-nth-section-with-predicate latex//forward-arguments latex//found-undesired-string latex//impl-previous-section latex/command-default latex/end-of-environment latex/forward-environment latex/next-section latex/next-section-same-level latex/section-regexp latex/setup-keybinds le left len length let let* let-form lets level li like line line-beginning-position line-end-position line-move-visual line-number-at-pos lisa lisa--backward-up-sexp lisa--find-in-buffer lisa--format lisa--global-replace lisa--insert-or-generate-package-template lisa--original-template-file lisa--should-template lisa--success lisa--umcomment-block lisa-define-package-variables lisa-insert-full-change-log lisa-insert-template list list-packages list-symbol list-system-processes listed listify-key-sequence listp lm-commentary lm-header lm-keywords-list lm-synopsis ln load load-file load-once load-path load-theme loc local-key-binding log lol longname looking-at looking-back loop lsp/define-package-variables lsp/replace-regexp lst m magit-diff-add magit-diff-del magit-diff-file-header magit-diff-hunk-header magit-diff-none magit-dont-ignore-whitespace magit-ignore-whitespace magit-key-mode-command magit-refresh magit-status mail-domain mail/create-buffer mail/switch-to-buffer mailbox-type main major make-backup-files make-directory make-hash-table make-local-variable make-marker make-overlay make-sparse-keymap make-string make-symbol make-variable-buffer-local map mapc mapcar mapconcat maphash mark mark-marker mark-sexp mark-whole-buffer marker master-file match match-beginning match-data match-data-list match-end match-string match-string-no-properties matches max mb mc--all-equal mc--executing-command-for-fake-cursor mc--kill-ring-entries mc--maybe-set-killed-rectangle mc--reset-read-prompts mc/add-fake-cursor-to-undo-list mc/all-fake-cursors mc/create-cursor-id mc/create-fake-cursor-at-point mc/cursor-with-id mc/delete-region-overlay mc/dump-list mc/edit-lines mc/enable-temporarily-disabled-minor-modes mc/execute-command mc/execute-command-for-all-fake-cursors mc/execute-command-for-fake-cursor mc/execute-this-command-for-all-cursors-1 mc/for-each-cursor-ordered mc/for-each-fake-cursor mc/keyboard-quit mc/make-cursor-overlay-at-eol mc/make-cursor-overlay-at-point mc/make-cursor-overlay-inline mc/make-region-overlay-between-point-and-mark mc/num-cursors mc/pop-state-from-overlay mc/prompt-for-inclusion-in-whitelist mc/remove-fake-cursor mc/remove-fake-cursors mc/restore-state-from-overlay mc/save-excursion mc/save-lists mc/save-window-scroll mc/store-current-state-in-overlay mc/temporarily-disable-unsupported-minor-modes md me member memq merged message message-string mew mew-biff-bark mew-case-set mew-decode-syntax-delete mew-face-mark-refile mew-face-mark-unread mew-refile-guess-by-alist mew-summary-display mew-summary-down mew-summary-exec mew-summary-form-date mew-summary-form-time mew-summary-goto-message mew-summary-ls mew-summary-up mew-summary-visit-folder mew-time-mon-str-to-int mew-time-rfc-day mew-time-rfc-mon mew/bristolp mew/gmail-allmail mew/gmail-inbox mew/goto-all mew/goto-inbox mew/inboxp min minibufferp minor-mode-key-binding misc ml mml mode mode-io-correlate mode-line mode-line-eol-desc modify-syntax-entry mouse-1 mouse-yank-primary mousing move-beginning-of-line move-end-of-line move-overlay movement-function mp msc/browse-url-best-browser msc/browser-decide msc/find-important-files msc/forward-paragraph msc/kill-and-close msc/open-line-and-indent msc/super-switch-to-buffer msc/unfill-paragraph mt/equation-break-line mt/forward-to-matching-right my-\.* my-c-mode-cedet-hook my-cedet-hook myFileList n name name-last-kbd-macro nameList narrow-to-defun nconc needle needsQuerying new-content-hash new-name new-timestamp newdir newline newline-and-indent newsticker-date-face newsticker-default-face newsticker-extra-face newsticker-feed-face newsticker-immortal-item-face newsticker-new-item-face newsticker-old-item-face newsticker-statistics-face next-command next-error-find-buffer next-line nfile nfo nil nint no noback nodir noflet noflet|expand not now nreverse nth null null-device num num-matches number-to-string numberp nv nvars nxml-sexp-element-flag nyan-create o o1 old old-archive-entry old-last-url old-value on once open open-externally open-line opening opoint or order org-agenda-files org-agenda-filter-apply org-agenda-list org-agenda-previous-line org-agenda-property-create-string org-agenda-switch-to org-auto-repeat-maybe org-beginning-of-line org-capture-templates org-ctrl-c-ctrl-c org-deadline org-edit-special org-edit-src-code org-end-of-line org-entry-get org-export-as-latex-to-buffer org-export-latex-classes org-get-at-bol org-goto-marker-or-bmk org-icompleting-read org-insert-heading-respect-content org-insert-todo-heading org-insert-todo-heading-respect-content org-last-state org-log-repeat org-property-values org-read-property-value org-refile-targets org-schedule org-set-property org-special-ctrl-a/e org-table-next-field org-table-recalculate org-table-sort-lines org-tag-faces org-tags-sparse-tree org-time-stamp org/-decide-heading-name org/-find-org-file org/agenda-position org/auction-house-table-sort org/extract-bibtex-property org/extract-doi org/insert-location org/new-todo org/paper-pdf-url org/refile-row org/retrieve-bibtex org/title-to-filename org/week-agenda orig-point original-command os ostart other other-buffer other-window out outline-forward-same-level outline-next-visible-heading outline-up-heading output output-pdf ov overlay overlay-end overlay-get overlay-put overlay-start overlays overlays-at overlays-in p package package-archives package-buffer-info package-build-archive package-build-archive-alist package-build-archive-alist-add package-build-archive-alist-remove package-build-archive-ignore-errors package-build-cleanup package-build-recipe-alist package-build-reinitialize package-def package-desc-doc package-desc-name package-desc-reqs package-desc-summary package-desc-vers package-desc-version package-initialize package-install-file package-install-from-buffer package-list package-menu--generate package-menu--new-package-list package-menu-execute package-menu-mark-upgrades package-refresh-contents package-strip-rcs-id package-version-join pair paredit-backward-up path patterns pb pb/add-to-archive-contents pb/archive-file-name pb/bzr-expand-repo pb/bzr-repo pb/checkout pb/checkout-git pb/copy-file pb/copy-package-files pb/create-tar pb/cvs-repo pb/darcs-repo pb/debugger-return pb/dump pb/dump-archive-contents pb/ensure-ends-here-line pb/expand-config-file-list pb/expand-file-specs pb/expand-source-file-list pb/find-package-commentary pb/find-package-file pb/find-parse-time pb/find-parse-time-latest pb/find-source-file pb/generate-dir-file pb/generate-info-files pb/get-package-info pb/get-pkg-file-info pb/git-head-branch pb/git-repo pb/hg-repo pb/merge-package-info pb/package-buffer-info-vec pb/package-name-completing-read pb/parse-time pb/princ-checkout pb/princ-exists pb/read-from-file pb/read-recipe pb/read-recipes-ignore-errors pb/readme-file-name pb/remove-archive pb/run-process pb/run-process-match pb/slurp-file pb/string-match-all pb/string-rtrim pb/svn-repo pb/trim pb/update-or-insert-version pb/with-wiki-rate-limit pb/write-pkg-file pb/write-pkg-readme pdf persp-selected-face pg-show-mark php+-mode-setup pkg pkg-cwd pkg-dir pkg-file pkg-file-source pkg-info pkg-name pkg-source pkg-target pkgfile-info pl play-sound-file plist plist-get plist-put po point point-max point-min pop pop-mark pop-to-mark-command pos pp-to-string prefer-coding-system prefix prefixes prev-content-hash previous previous-line princ print print-length print-level print-quoted private proc process-attributes process-connection-type process-environment process-get process-send-string process-status prog-ssl progn prompt prop propertize propertized-full-mode-string propertized-shorten-mode-string protected proto provide pt push push-mark put put-text-property puthash pvars query-string quit-window quote r ra rainbow-delimiters-depth-1-face rainbow-delimiters-depth-2-face rainbow-delimiters-depth-3-face rainbow-delimiters-depth-4-face rainbow-delimiters-depth-5-face rainbow-delimiters-depth-6-face rainbow-delimiters-depth-7-face rainbow-delimiters-depth-8-face rainbow-delimiters-depth-9-face random rassoc rci re re-search-backward re-search-forward read-buffer-to-switch read-file-name read-from-string read-kbd-macro read-number read-only read-string readme-file real recenter recipe-file refile-guess-alist reftex-get-bibfile-list reftex-kill-emacs-hook reftex-pop-to-bibtex-entry reftex-reference reg regex regexp regexp-opt regexp-quote region-beginning region-end remove remove-hook remove-if remove-if-not remove-overlays rename-buffer rename-file repeat replace-match replace-regexp-everywhere replace-regexp-in-string replace-string replacements replacer-match-data repo repo-type require requires res result return reverse revert-buffer ri right root run run-hooks s s--aget s--mapcar-head s--truthy\? s-chop-prefix s-chop-suffix s-chop-suffixes s-format s-join s-lex-fmt|expand s-lex-format s-match-strings-all s-slice-at s-split s-split-words s-trim-left s-trim-right s-with s1 safe-local-variable-values sap satisfying save-buffer save-current-buffer save-excursion save-match-data save-restriction save-some-buffers saved-func-namev saved-match-data saves sc scala-mode-feature:tags-command scala-mode-feature:tags-option scala-mode:api-url screen-cast search-backward search-backward-regexp search-forward search-forward-regexp search-length seconds section secure-hash see select-and-evaluate select-frame select-window selected-window selfile separator sequence seriously server-done server-start set set-buffer set-buffer-file-coding-system set-clipboard-coding-system set-default set-default-coding-systems set-face-attribute set-face-background set-face-foreground set-fill-column set-fontset-font set-frame-parameter set-keyboard-coding-system set-mark set-mark-command-repeat-pop set-marker set-match-data set-process-query-on-exit-flag set-selective-display set-terminal-coding-system set-text-properties set-visited-file-name set-window-hscroll set-window-start setcdr setf setq setq-default sexp-at-point sh-indent-comment shell-command shell-command-to-string shift-insert should show-subtree signal single$ size skip-chars-backward skip-chars-forward sleep-for smaller smart-mode-line sml-modeline-create sml/buffer-name sml/check-hidden-modes sml/extract-minor-modes sml/fill-width-available sml/get-directory sml/get-prefix sml/mode-list-to-string-list sml/nyan-support sml/propertize-prefix sml/regexp-composer sml/replacer sml/revert sml/set-battery-font sml/set-face-color sml/set-shortener-func sml/setup sml/shorten-directory sml/show-client sml/show-encoding sml/show-eol sml/show-time sml/simplified sml/simplified-extract-minor-modes sml/sml-modeline-support sml/strip-prefix smooth-scroll-margin smtp-auth-list smtp-server smtp-ssl smtp-ssl-port smtp-user sn so sort sorted source-file sources sp spec split-string split-window-below split-window-right src ss st stack stale-archives stamp-file stamp-info start start-pos start-process start-time status str string string-equal string-lessp string-match string-to-char string-to-list string-to-number string< string= stringp strings sub subdir subrp subs substitute-command-keys substring substring-no-properties such suffix suffixes suspend-frame switch-to-buffer switch-to-buffer-other-window sy sym symbol symbol-function symbol-macrolet symbol-name symbol-regexp symbol-value symbolp synctex-output syntax-ppss t t-or-nil-or-color table tabulated-list-init-header target target-dir tc temp- template term term-send-raw term-set-escape-char tex-main-file texBuffer text text-properties-at textrm the the-command thing-at-point thing-at-point-looking-at this this-command-keys this-file this-fn this-marker throw thus time time-since time-to-seconds times tipically title to toggle-text-mode-auto-fill toggle-truncate-lines top trailer treated truncates tw tw/other-window twit twittering-goto-next-status twittering-goto-previous-status twittering-goto-previous-thing twittering-kill-buffer txtfile type type-of uc ufile uncomment-region unless unsupported-cmd unwind-protect upcase url url-copy-file url-hexify-string url-link url-retrieve url-retrieve-synchronously use used user user-full-name using usually v va val value var var-form-bounds variable-at-point variables varlist vars vc-backend vector verify-visited-file-modtime version version-to-list very vn vo void-function vs w w32-shell-execute wait warn wasActive wasChanged wasInChat wasInJabber wasJab weekfile when which whichever while widen width wiki-url win windmove-default-keybindings windmove-find-other-window window-body-width window-buffer window-configuration-to-register window-hscroll window-list window-normalize-window window-start window-width with with-current-buffer with-output-to-string with-temp-buffer with-temp-file wl word-count words working-dir workspaces-adt write-file write-region ww x x-send-client-message x86 yank yas--snippets-at-point yas-expand yas-expand-snippet yas-field-highlight-face yas-insert-snippet yas-next-field yas-prompt-functions yas-reload-all yas/fetch yas/get-snippet-tables yas/insert-by-name yas/template-content yasnippet yet you yr zap-to-char zerop)
  
  "List of symbols of function considered pure (and thus safe) by litable.

Litable won't evaluate code that contains a function not listed here.")

(defun litable--deep-search-for-impures (form)
  "Check whether all car's inside FORM are pure.

If any isn't a pure function, reports in the variable `litable--impure-found'."
  (if (not (listp form))
      ;; If it's not a list, it is the function name
      (unless (member form litable-pure-functions-list)
        (add-to-list 'litable--impure-found form))
    ;; If it's a list, it is the entire function call. Check the name,
    ;; and search the arguments for more function calls. Plain
    ;; arguments don't get checked.
    (litable--deep-search-for-impures (car form))
    (dolist (cur (cdr form))
      (when (listp cur) (litable--deep-search-for-impures cur)))))

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
         (s (format " => %s" (funcall litable-print-function result))))
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
                  (format " <= %s" (mapconcat litable-print-function input ", "))
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
                          (funcall litable-print-function value) "}")
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
