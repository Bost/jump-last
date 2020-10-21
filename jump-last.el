;;; jump-last.el --- Jump to the Last Edit Location, regardless of the file.

;; Copyright (C) 2020 Rostislav Svoboda

;; Authors: Rostislav Svoboda <Rostislav.Svoboda@gmail.com>
;; Version: 0.1
;; Package-Requires: ((goto-chg "1.7.3"))
;; Keywords:
;; URL: https://github.com/Bost/jump-last

;;; Installation:
;; In the `dotspacemacs/user-config', add there:
;;   (use-package jump-last)
;; then, in the `dotspacemacs-additional-packages', add there:
;;   (jump-last :location
;;              (recipe :fetcher github :repo "Bost/jump-last"))
;; or:
;;   $ git clone https://github.com/Bost/jump-last
;; and then
;;   (jump-last :location "<path/to/the/cloned-repo>")

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

(defun my=buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer.
Thanks to https://stackoverflow.com/a/2238589"
  (with-current-buffer buffer-or-string
    major-mode))

;; (message "%s" ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")

;; (dolist (mode (buffer-list))
;;   (message "%s; relevant %s"
;;            mode
;;            (if (provided-mode-derived-p (my=buffer-mode mode) 'prog-mode 'text-mode)
;;                t)))

;; The https://stackoverflow.com/a/8627634 doesn't work and its home page
;; http://shenfeng.me/emacs-last-edit-location.html doesn't exit anymore anyway

(setq my=last-edited-buffer nil)

(defun my=current-buffer ()
  "See also `window-buffer' and `buffer-file-name', however the
  `buffer-file-name' returns nil when in the *scratch* buffer."
  (current-buffer))

(defun my=last-edited-buffer-changed-p ()
  (if (not (equal (my=current-buffer) my=last-edited-buffer))
      t))

(defun my=buffer-relevant-p (buffer-or-name)
  (let* (
         (buffer-major-mode (my=buffer-mode buffer-or-name))
         (my=buffer-relevant
         ;; `provided-mode-derived-p' is available starting from Emacs 26, so we
         ;; need to check if the is defined.
         ;;
         ;; Note:
         ;; Spacemacs (master) requires Emacs 24.4 or above.
         ;; Spacemacs (develop) requires Emacs 25.1 or above See
         ;; https://github.com/syl20bnr/spacemacs#prerequisites
         (if (fboundp 'provided-mode-derived-p)
             (provided-mode-derived-p buffer-major-mode 'prog-mode 'text-mode)
           (not (find buffer-major-mode my=unwanted-modes)))))
    (if my=buffer-relevant t)))

(defun my=save-last-edited-buffer (n &optional c)
  (let* (
         (curr-buff (my=current-buffer))
         )
    (message "%s changed: %s; ignored: %s"
             "[my=save-last-edited-buffer]"
             (my=last-edited-buffer-changed-p) (my=buffer-relevant-p curr-buff))
    (when (and (my=last-edited-buffer-changed-p)
               (my=buffer-relevant-p curr-buff))
      (setq my=last-edited-buffer curr-buff))))

(defun my=goto-last-edited-place ()
  (interactive)
  (message "Running %s" "my=jump-last-edited-place")
  (let* (
         (was-not-killed (if (buffer-name my=last-edited-buffer) t))
         )
    (message "%s defined %s; was-killed: %s; relevant %s; changed: %s;"
             "[my=goto-last-edited-place]"
             (if my=last-edited-buffer t)
             was-not-killed
             (my=buffer-relevant-p my=last-edited-buffer)
             (my=last-edited-buffer-changed-p))
    (when (and (my=last-edited-buffer-changed-p)
               (my=buffer-relevant-p my=last-edited-buffer)
               ;; has been set, i.e. does it have a non-nil value?
               my=last-edited-buffer
               was-not-killed)
      (pop-to-buffer my=last-edited-buffer))
    (goto-last-change nil)))

(dolist (symb '(
                self-insert-command
                delete-char
                ;; advising following evil functions doesn't work:
                ;; evil-insert evil-delete evil-append
                ))
  (advice-add symb :after #'my=save-last-edited-buffer)
  ;; (advice-remove symb #'my=save-last-edited-buffer)
  )

;; TODO try another approach: define a list of hooks for major or minor modes.
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
