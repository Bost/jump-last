;;; jump-last.el --- Jump to the Last Edit Location, regardless of the file.

;; Copyright (C) 2020 Rostislav Svoboda

;; Authors: Rostislav Svoboda <Rostislav.Svoboda@gmail.com>
;;          Daniel Nicolai <dalanicolai@gmail.com>
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

;; TODO Convert my=last-edited-buffer to a list and access its elements with
;; push/pop then add-hook on the kill-buffer-hook which removes current buffer
;; from this list
(setq my=last-edited-buffer nil)

(defun my=last-edited-buffer-changed-p ()
  "See also `window-buffer', `buffer-name' and
  `buffer-file-name', however the `buffer-file-name' returns nil
  when in the *scratch* buffer."
  (and
   (not (equal (current-buffer) my=last-edited-buffer))
   (not (string= " markdown-code-fontification:clojure-mode" (buffer-name))))
  )

(defun my=save-last-edited-buffer (beg end length)
  "See also `window-buffer', `buffer-name' and
  `buffer-file-name', however the `buffer-file-name' returns nil
  when in the *scratch* buffer."
  (when (my=last-edited-buffer-changed-p)
    (setq my=last-edited-buffer (current-buffer))))

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
  (goto-last-change nil)
  (recenter-top-bottom))

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

;; Map it to e.g. `WinKey + F10':
;;   (global-set-key (kbd "<s-f10>") 'my=jump-last-edited-place)
;; or remap it completely if you're brave enough:
;;   (global-set-key [remap goto-last-change] 'my=jump-last-edited-place)

(provide 'jump-last)
