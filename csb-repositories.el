;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(when (> emacs-major-version 23)
  (require 'package)
  (require 'cl))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path (concat user-emacs-directory "el-get-user/recipes"))
(el-get 'sync)

;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(add-to-list 'package-archives '("user42" . "http://download.tuxfamily.org/user42/elpa/packages/"))

(when running-os-is-linux
  (run-with-timer (* 60 60 6) (* 60 60 12) 'paradox-upgrade-packages))

(with-eval-after-load 'paradox
  (setq paradox-execute-asynchronously t
        paradox-column-width-version 15
        paradox-column-width-package 30
        paradox-column-width-download 7
        paradox-column-width-status 13
        paradox-automatically-star nil
        paradox-github-token t)
  (add-hook 'paradox-menu-mode-hook
            (lambda ()
              (indent-guide-mode -1)  ;; too slow otherwise
              (define-key paradox-menu-mode-map "Q"
                (lambda()
                  (interactive)
                  (kill-this-buffer)
                  (persp-kill "paradox"))))))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((org-plus-contrib . "org"))))

(provide 'csb-repositories)

;;; csb-repositories.el ends here
