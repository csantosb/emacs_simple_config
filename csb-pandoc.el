;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(with-eval-after-load 'pandoc-mode
    (defun find-pandoc-config-file-other ()
      "Edit the pandoc-mode config file, in another window."
      (interactive)
      (find-file-other-window (concat user-emacs-directory "elisp/my_pandoc.org")))

    (defun find-pandoc-config-file ()
      "Edit the pandoc-mode config file."
      (interactive)
      (find-file (concat user-emacs-directory "elisp/my_pandoc.org"))))

(with-eval-after-load 'pandoc-mode
    (define-key pandoc-mode-map "\C-c/w" 'pandoc-set-write)
    (define-key pandoc-mode-map "\C-c/r" 'pandoc-run-pandoc)
    (define-key pandoc-mode-map "\C-c/p" 'pandoc-convert-to-pdf)

    (define-key pandoc-mode-map "\C-c/S" 'pandoc-view-settings)
    (define-key pandoc-mode-map "\C-c/V" 'pandoc-view-output)
    ;;
    (define-key pandoc-mode-map "\C-c/c" 'pandoc-insert-@)
    (define-key pandoc-mode-map "\C-c/C" 'pandoc-select-@)
    (define-key pandoc-mode-map "\C-c/m" 'pandoc-set-metadata)
    (define-key pandoc-mode-map "\C-c/v" 'pandoc-set-variable)
    ;;
    (define-key pandoc-mode-map "\C-c/d" 'pandoc-set-default-format)
    ;; save settings
    (define-key pandoc-mode-map "\C-c/s" 'pandoc-save-settings-file)
    (define-key pandoc-mode-map "\C-c/P" 'pandoc-save-project-file)
    (define-key pandoc-mode-map "\C-c/g" 'pandoc-save-global-settings-file)
    ;;
    (define-key pandoc-mode-map "\C-c/I" 'find-pandoc-config-file)
    (define-key pandoc-mode-map "\C-c/i" 'find-pandoc-config-file-other))

(with-eval-after-load 'pandoc-mode
     (define-key pandoc-@-mode-map "q" 'pandoc-quit-@-select)
     (define-key pandoc-@-mode-map "j" 'pandoc-next-@)
     (define-key pandoc-@-mode-map "n" 'pandoc-next-@)
     (define-key pandoc-@-mode-map [down] 'pandoc-next-@)
     (define-key pandoc-@-mode-map "k" 'pandoc-prev-@)
     (define-key pandoc-@-mode-map "p" 'pandoc-prev-@)
     (define-key pandoc-@-mode-map [up] 'pandoc-prev-@)
     (define-key pandoc-@-mode-map [return] 'pandoc-select-current-@)
     (define-key pandoc-@-mode-map [home] 'pandoc-goto-first-@)
     (define-key pandoc-@-mode-map [prior] 'pandoc-goto-first-@)
     (define-key pandoc-@-mode-map [end] 'pandoc-goto-last-@)
     (define-key pandoc-@-mode-map [next] 'pandoc-goto-first-@))

;; (add-hook 'pandoc-mode-hook (lambda ()
;;                               ;; Edit pandoc config mile
;;                               (local-set-key (kbd "C-c g I") 'find-pandoc-config-file)
;;                               (local-set-key (kbd "C-c g i") 'find-pandoc-config-file-other)))

(provide 'csb-pandoc)

;;; csb-pandoc.el ends here
