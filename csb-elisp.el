;;; csb-elisp --- Summary
;;
;;; Commentary:
;;
;; This file contains defaults for variables as well as global keystrokes

(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-prog-mode))))

;;; Navigation

  (with-eval-after-load 'elisp-mode
    (define-key emacs-lisp-mode-map (kbd "M-j") 'forward-sexp)
    (define-key emacs-lisp-mode-map (kbd "M-k") 'backward-sexp))

;;; Navigation - Slime

  (with-eval-after-load 'elisp-slime-nav-mode-map
    (define-key elisp-slime-nav-mode-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)
    (define-key elips-slime-nav-mode-map (kbd "M-,") 'pop-tag-mark)
    (global-set-key (kbd "C-c C-d C-d") nil))

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-n")
    #'(lambda()(interactive)
        (re-search-forward outline-regexp)
        (beginning-of-line)))
  (define-key emacs-lisp-mode-map (kbd "C-c C-p")
    #'(lambda()(interactive)
        (re-search-backward outline-regexp)
        (beginning-of-line))))

;;; Hook

(add-hook 'emacs-lisp-mode-hook
          (lambda ()

            (outline-minor-mode 1) ;; get acces to outshine
            ;; (nameless-mode-from-hook)
            ;; (setq-local orgstruct-heading-prefix-regexp "^ *;; *\\(\\**.*\\)")
            ;; (add-to-list 'imenu-generic-expression '(" "  " *;; *\\(.*\\)" 1))
            ;; (add-to-list 'imenu-generic-expression '(nil "^\\(?:[*]+\\).*$" 0))
            ;; (add-to-list 'imenu-generic-expression '(nil "^ *;; *\\(\\*.*\\)" 1))
            ;; (add-to-list 'imenu-generic-expression '(nil "^ *;;; +\\([*]+ .*\\)" 1))
            (setq-local helm-imenu-delimiter " ")
            (setq-local imenu-generic-expression
                        '(("" ";;[;]\\{1,8\\} \\(.*$\\)" 1)
                          ;; ("" "^\\s-*;; +\\([*]+ .*\\)" 1)
                          ("" "^\\s-*(\\(cl-def\\(?:ine-compiler-macro\\|macro\\|subst\\|un\\)\\|def\\(?:advice\\|generic\\|ine-\\(?:compil\\(?:ation-mode\\|er-macro\\)\\|derived-mode\\|g\\(?:\\(?:eneric\\|lobal\\(?:\\(?:ized\\)?-minor\\)\\)-mode\\)\\|m\\(?:ethod-combination\\|inor-mode\\|odify-macro\\)\\|s\\(?:etf-expander\\|keleton\\)\\)\\|m\\(?:acro\\|ethod\\)\\|s\\(?:etf\\|ubst\\)\\|un\\*?\\)\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)
                          ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\)\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)
                          ("Variables" "^\\s-*(defvar\\s-+\\(\\(\\sw\\|\\s_\\)+\\)[[:space:]\n]+[^)]" 1)
                          ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(\\sw\\|\\s_\\)+\\)" 2)))
            (elisp-slime-nav-mode)
            (enable-paredit-mode)
            (rename-modeline "lisp-mode" emacs-lisp-mode "elisp")
            ;; Dash
            (require 'helm-dash)
            (setq-local helm-dash-common-docsets '("Emacs Lisp"))
            (setq-local helm-dash-docsets '("Emacs Lisp"))
            (helm-dash-activate-docset "Emacs Lisp")
            ;; Company
            (require 'company-elisp)
            ;; make `company-backends' local is critcal
            ;; or else, you will have completion in every major mode, that's very annoying !
            (make-local-variable 'company-backends)
            ;; company-elisp is the plugin to complete words
            (add-to-list 'company-backends 'company-elisp)))

  (add-hook 'lisp-interaction-mode-hook
            (lambda ()
              (enable-paredit-mode)))

  (add-hook 'ielm-mode-hook 'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

(provide 'csb-elisp)

;;; csb-elisp.el ends here
