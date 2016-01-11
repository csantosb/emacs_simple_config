;;; csb-backup --- Summary
;;
;;; Commentary:
;;
;; This file contains ...

(defvar autosave-dir
  (if running-os-is-linux
      (expand-file-name ".autosaves/" user-emacs-directory)
    (expand-file-name (expand-file-name ".autosaves/"
                                        user-emacs-directory))))
(setq auto-save-default t
      auto-save-list-file-prefix autosave-dir
      ;; auto-save-file-name-transforms `((".*" ,autosave-dir t))
      ;; auto-save-file-name-transforms
      ;;  `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat autosave-dir "\\2") t))
      delete-auto-save-files t
      auto-save-timeout 30
      auto-save-interval 300)

(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))
;; (setq mylist '(emacs-lisp-mode org-mode))
;; (add-hook 'focus-out-hook (lambda () (when (member major-mode mylist) (save-buffer))))

(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))

(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

;; (ad-unadvise 'desktop-save-buffer-p)
;; (defadvice switch-to-buffer (before set-font activate)
;;   (if  (member major-mode '(org-mode))
;;    (set-frame-font "Symbola" t)
;;  (set-frame-font "Inconsolata" t)))
;; (add-to-list 'after-change-major-mode-hook (lambda () (setq titi major-mode) (message "current major mode is %s" toto)))
;; (set-frame-font "Inconsolata" t)
;; (ad-unadvise 'switch-to-buffer)
;; buffer-face-mode-face

;; (setq desktop-modes-not-to-save '(python-mode))
(require 'desktop)
(desktop-save-mode -1)
(setq desktop-auto-save-timeout 10)
(setq desktop-path (list (concat user-emacs-directory)))
(setq desktop-dirname (list (concat user-emacs-directory)))
(setq desktop-base-file-name ".emacs.desktop")
(setq desktop-load-locked-desktop t)
(setq desktop-restore-frames t)
(setq desktop-restore-in-current-display t)
(setq desktop-restore-reuses-frames t)
(setq desktop-restore-forces-onscreen t)
;; (setq desktop-globals-to-save nil)
;; (setq desktop-locals-to-save nil)
;; (defadvice desktop-save-buffer-p (before desktop-save-buffer-p-test (filename bufname mode &rest _dummy) activate)
;;   (setq CurrentPerspBufferNames_ (mapcar (lambda (l) (buffer-name l) ) (persp-buffers persp-curr)))
;;   (setq CurrentPerspBufferNames (remove-if-not 'identity CurrentPerspBufferNames_)) ;; remove nils
;;   ;; if buffer is not a member of the list, change its name to nil
;;   (if (not (member (symbol-name (make-symbol bufname)) CurrentPerspBufferNames))
;;    (setq filename nil)))
;; (ad-unadvise 'desktop-save-buffer-p)

(savehist-mode t)
(stante-after savehist
              (add-to-list 'savehist-additional-variables 'kill-ring)
              (setq savehist-file (expand-file-name "history" user-emacs-directory)))

(require 'saveplace)
(save-place-mode 1)
(stante-after saveplace
              (setq-default save-place t)
              (setq-default save-place-file
                            (expand-file-name ".saved-places" user-emacs-directory)))

(setq-default make-backup-files t
              ;; store all backup in local backups dir
              backup-directory-alist '((".*" . ".backups"))
              delete-old-versions t
              vc-make-backup-files t
              kept-new-versions 10
              kept-old-versions 10
              ;; bqackup-by-copying t
              version-control t)

(provide 'csb-backup)
;;; csb-backup.el ends here
