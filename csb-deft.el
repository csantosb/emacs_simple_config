;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(stante-after deft
              (setq deft-use-filename-as-title t
                    deft-incremental-search t
                    deft-text-mode 'org-mode
                    deft-separator "  "))

(stante-after deft
              (defadvice deft (after deft-helm-perso-wiki activate)
                (set (make-local-variable 'header-line-format)
                     (format "    %s%s%s" "Use C-c C- ... d-delete  n-new"
                             "  r-rename a-archive  g-refresh  s-toggle_sort"
                             "t-toggle_inc_search  q-quit  Q-close"))))

(provide 'csb-deft)

;;; csb-deft.el ends here
