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

(defun my=buffer-major-mode (buffer-or-string)
  "Returns the major mode associated with a buffer.
Thanks to https://stackoverflow.com/a/2238589"
  (when buffer-or-string
    (with-current-buffer buffer-or-string
      major-mode)))

;; TODO Convert my=last-edited-buffer to a list and access its elements with
;; push/pop then add-hook on the kill-buffer-hook which removes current buffer
;; from this list
(setq my=last-edited-buffer nil)

(defun my=current-buffer ()
  "See also `window-buffer' and `buffer-file-name', however the
  `buffer-file-name' returns nil when in the *scratch* buffer."
  (current-buffer))

(defun my=last-edited-buffer-changed-p ()
  (if (not (equal (my=current-buffer) my=last-edited-buffer))
      t))

(defun my=save-last-edited-buffer (beg end length)
  (when (my=last-edited-buffer-changed-p)
    (setq my=last-edited-buffer (my=current-buffer))))

(defun my=jump-last-edited-place ()
  (interactive)
  (when (and
         ;; TODO compare my=last-edited-buffer with the active buffer
         (my=last-edited-buffer-changed-p)
         ;; has been set, i.e. does it have a non-nil value?
         my=last-edited-buffer
         ;; ... as has not been killed
         (buffer-name my=last-edited-buffer))
    (pop-to-buffer my=last-edited-buffer))
  (goto-last-change nil))

(defun my=register ()
  (interactive)
  (when (derived-mode-p 'text-mode 'prog-mode)
    ;; (message "[my=register] major-mode: %s; adding hook)"
    ;;          major-mode)
    (add-hook 'after-change-functions 'my=save-last-edited-buffer 0 t)))

(dolist (hook '(text-mode-hook prog-mode-hook))
  (add-hook hook 'my=register)
  ;; (remove-hook hook 'my=register)
  )

;; Map it to `WinKey + F10':
;;   (global-set-key (kbd "<s-f10>") 'my=jump-last-edited-place)
;; or remap it completely if you're brave enough:
;;   (global-set-key [remap goto-last-change] 'my=jump-last-edited-place)

(provide 'jump-last)
