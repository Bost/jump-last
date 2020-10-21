(require 'goto-chg)

(setq my=unwanted-modes
      '(magit-status-mode
        magit-log-mode
        magit-diff-mode
        magit-revision-mode
        magit-stash-mode
        magit-process-mode
        bs-mode ; *buffer-selection*
        ;; in fundamenatal-mode:
        ;; *package-build-checkout*
        ;; *cider-refresh-log*
        ;; *edn*
        ;; *Backtrace*
        ;; *Help*
        cider-browse-ns-mode  ; for *cider-ns-browser*
        cider-stacktrace-mode ; for *cider-error*
        cider-docview-mode    ; for *cider-doc*
        cider-inspector-mode  ; for *cider-inspect*
        help-mode             ; for *Help*
        dired-mode
        ediff-meta-mode       ; for *Ediff Registry*
        Info-mode             ; for *info*
        spacemacs-buffer-mode ; for *spacemacs*
        compilation-mode      ; for *Compile-Log*
        minibuffer-inactive-mode ; for *Minibuf-1*
        ))

;; The https://stackoverflow.com/a/8627634 doesn't work and its home page
;; http://shenfeng.me/emacs-last-edit-location.html doesn't exit anymore anyway

(setq my=last-edited-buffer nil)

(defun my=current-buffer ()
  "See also `window-buffer' and `buffer-file-name', however the
  `buffer-file-name' returns nil when in the *scratch* buffer."
  (current-buffer))

(defun my=last-edited-buffer-changed ()
  (not (equal (my=current-buffer) my=last-edited-buffer)))

(defun my=save-last-edited-buffer (n &optional c)
  (let* (
         (changed (my=last-edited-buffer-changed))
         (ignored (if (find major-mode my=unwanted-modes) t))
         )
    ;; (message "%s changed: %s; ignored: %s"
    ;;          "[my=save-last-edited-buffer]" changed ignored)
    (when (and changed (not ignored))
        (setq my=last-edited-buffer (my=current-buffer)))))

(defun my=goto-last-edited-place ()
  (interactive)
  (let* (


         (changed (my=last-edited-buffer-changed))
         (was-killed (if (not (buffer-name my=last-edited-buffer)) t))
         (undefined (if my=last-edited-buffer nil t))
         (ignored (if (find major-mode my=unwanted-modes) t))
         )
    ;; (message "%s undefined %s; was-killed: %s; ignored %s; changed: %s;"
    ;;          "[my=goto-last-edited-place]"
    ;;          undefined was-killed ignored changed)
    (when (and changed (not (or undefined was-killed ignored)))
      (pop-to-buffer my=last-edited-buffer))
    (goto-last-change nil)))

(dolist (symb '(
                self-insert-command
                delete-char
                ;; advising evil functions doesn't work
                ;; evil-insert evil-delete evil-append
                ))
  (advice-add symb :after #'my=save-last-edited-buffer)
  ;; (advice-remove symb #'my=save-last-edited-buffer)
  )

;; TODO better approach: define a list of hooks for major or minor modes.
;; See also https://www.emacswiki.org/emacs/List_Of_Major_And_Minor_Modes
;; (dolist (symb '(text-mode-hook emacs-lisp-mode-hook))
;;   (add-hook symb 'my=save-last-edited-buffer)
;;   ;; (remove-hook symb 'my=save-last-edited-buffer)
;;   )

;; FYI tinkering with `after-change-functions' doesn't work. Its value is buffer
;; local.
;; (setq after-change-functions
;;       (add-to-list 'after-change-functions 'my=save-last-edited-place))

;; Map it to `WinKey + F10':
;;   (global-set-key (kbd "<s-f10>") 'my=goto-last-edited-place)
;; or remap it completely if you're brave enough:
;;   (global-set-key [remap goto-last-change] 'my=goto-last-edited-place)

(provide 'jump-last)
