;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(add-hook 'after-init-hook 'global-company-mode)
;; (global-company-mode 1)

(stante-after company
              (company-quickhelp-mode 1)
              (setq company-quickhelp-delay 0.5))

(stante-after company
  (define-key company-active-map (kbd "TAB") 'company-complete-common)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map (kbd "<f1>") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)
  (define-key company-active-map "\C-g" 'company-abort)
  (define-key company-active-map (kbd "M-n") 'company-select-next)
  (define-key company-active-map (kbd "M-p") 'company-select-previous)
  (define-key company-active-map "\C-w" 'company-show-location)
  (define-key company-active-map "\C-s" 'company-search-candidates)
  (define-key company-active-map "\C-\M-s" 'company-filter-candidates))

(stante-after company
              ;; (setq company-begin-commands '(self-insert-command org-self-insert-command))
              (setq company-idle-delay 0.01
                    company-echo-delay 0.01
                    company-minimum-prefix-length 2))

;; yas-minor-mode in hooks in a per-buffer basis
(when running-os-is-linux
  (yas-global-mode 1)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets" yas-installed-snippets-dir)))

;; (add-to-list 'company-backends 'company-yasnippet)
(with-eval-after-load 'yasnippet
  ;; (define-key yas-keymap [(tab)]       'yas-next-field-or-maybe-expand)
  ;; (define-key yas-keymap (kbd "TAB")   'yas-next-field-or-maybe-expand)
  ;; (define-key yas-keymap [(shift tab)] 'yas-prev-field)
  ;; (define-key yas-keymap [backtab]     'yas-prev-field)
  ;; (define-key yas-keymap (kbd "C-g")   'yas-abort-snippet)
  ;; (define-key yas-keymap (kbd "C-d")   'yas-skip-and-clear-or-delete-char)
  (define-key yas-minor-mode-map [(tab)]     nil) ;; 'yas-expand
  (define-key yas-minor-mode-map (kbd "TAB") nil) ;; 'yas-expand
  ;; maybe C-c y ?? to expand
  (define-key yas-minor-mode-map (kbd "C-c yy") 'company-yasnippet)
  (define-key yas-minor-mode-map (kbd "C-c yn") 'yas-new-snippet)
  (define-key yas-minor-mode-map (kbd "C-c ye") 'yas-expand)
  (define-key yas-minor-mode-map (kbd "C-c yi") 'yas-insert-snippet)
  (define-key yas-minor-mode-map (kbd "C-c yv") 'yas-visit-snippet-file))

(add-hook 'after-init-hook 'abbrev-mode)
(abbrev-mode 1)
;; (with-eval-after-load 'abbrev (add-to-list 'company-backends 'company-abbrev))
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)
(setq abbrev-file-name (concat user-emacs-directory ".abbrev_defs"))
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;; I am not using helm's dabbrev 'C-x c c', as I prefer to add the backend to company
(global-unset-key (kbd "M-/"))   ;; dabrev-expand
(global-unset-key (kbd "C-M-/")) ;; dabrev-complete
;; (with-eval-after-load 'abbrev (add-to-list 'company-backends 'company-dabbrev-code)) ;; auto done
;; (add-to-list 'company-backends 'company-dabbrev)

(provide 'csb-completions)
;;; csb-completions.el ends here
