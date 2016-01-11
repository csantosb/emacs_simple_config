;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(fa-config-default)
(with-eval-after-load 'c-mode
  (define-key c-mode-map (kbd "C-x c I") 'moo-jump-local))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              ;; find . -type f -name "*.c" -print > gtags.file
              (ggtags-mode 1)
              (helm-gtags-mode 1)
              (flyspell-prog-mode)
              (when running-os-is-linux
                (semantic-mode 1))
              (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
              ;; (setq-local orgstruct-heading-prefix-regexp " */// ")
              ;; (add-to-list 'imenu-generic-expression '(nil  "^/// *\\(.*\\)" 1))
              )))

(add-hook 'c-mode-hook (lambda ()
                         ;; derived from prog-mode
                         (message "Entering c mode")))

(add-hook 'c++-mode-hook (lambda ()
                           ;; derived from prog-mode
                           (message "Entering c++ mode")
                           (when running-os-is-linux
                             (semantic-show-parser-state-mode 1))))

(provide 'csb-c-mode)

;;; csb-c-mode.el ends here
