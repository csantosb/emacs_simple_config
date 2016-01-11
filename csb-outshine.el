;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(with-eval-after-load 'outshine
  ;; (setq outline-minor-mode-prefix "\M-#")
  (setq outshine-use-speed-commands t
        outshine-startup-folded-p t))
;; (defvar outline-minor-mode-prefix "\M-#")

;; retrieve usual orgmode S-TAB behaviour
(stante-after outshine
              (define-key outline-minor-mode-map (kbd "<backtab>") 'outshine-cycle-buffer)

              (outshine-define-key-with-fallback
               outline-minor-mode-map (kbd "M-<up>")
               (outline-move-subtree-up) (outline-on-heading-p))

              (outshine-define-key-with-fallback
               outline-minor-mode-map (kbd "M-<down>")
               (outline-move-subtree-down) (outline-on-heading-p))

              (outshine-define-key-with-fallback
               outline-minor-mode-map (kbd "M-<left>")
               (outline-demote) (outline-on-heading-p))

              (outshine-define-key-with-fallback
               outline-minor-mode-map (kbd "M-<right>")
               (outline-promote) (outline-on-heading-p)))
;; (add-hook 'outshine-hook (lambda () (define-key outline-minor-mode-map (kbd "<backtab>") 'outshine-cycle-buffer)))

(provide 'csb-outshine)

;;; csb-outshine.el ends here
