;;; csb-tools.el --- Summary
;;
;;; Commentary:
;;
;; This file contains ...

(with-eval-after-load 'vhdl-mode
  (require 'vhdl-tools)
  (setq vhdl-tools-use-outshine t))

(with-eval-after-load 'vhdl-mode
   (define-key vhdl-mode-map "\C-c\C-h" nil) ;; vhdl-doc-mode
   (define-key vhdl-mode-map (kbd "C-c ?") 'vhdl-doc-mode))

(with-eval-after-load 'vhdl-mode

  (define-key vhdl-mode-map (kbd "C-c v")
    (defhydra csb/hydra-vhdl (:color blue)
      "
  C-_i_ _i_ndent   C-_t_ template    C-_f_ fill   _b_eautify  C-_p_ port  _c_ompose   C-_x_ fix  _m_odel   C-_a_ align  _?_
    "
      ("i" (with-initial-minibuffer "vhdl-indent") nil)
      ("t" (with-initial-minibuffer "vhdl-template") nil)
      ("f" (with-initial-minibuffer "vhdl-fill") nil)
      ("b" (with-initial-minibuffer "vhdl-beautify") nil)
      ("a" (with-initial-minibuffer "vhdl-align") nil)
      ("p" (with-initial-minibuffer "vhdl-port") nil)
      ("c" (with-initial-minibuffer "vhdl-compose") nil)
      ("x" (with-initial-minibuffer "vhdl-fix") nil)
      ("m" (with-initial-minibuffer "vhdl-model") nil)
      ("?" vhdl-doc-mode nil)
      ("q" nil nil))))

(with-eval-after-load 'vhdl-mode
  (define-key vhdl-mode-map (kbd "C-M-\\") nil) ;; 'vhdl-indent-region
  (define-key vhdl-mode-map (kbd "C-c C-i C-r") 'vhdl-indent-region)
  ;;
  (define-key vhdl-mode-map (kbd "C-M-q") nil) ;; 'vhdl-indent-sexp
  (define-key vhdl-mode-map (kbd "C-c C-i s") 'vhdl-indent-sexp)
  ;;
  (define-key vhdl-mode-map (kbd "C-c C-i C-l") 'vhdl-indent-line))

(with-eval-after-load 'vhdl-mode
   (define-key vhdl-template-map (kbd "ic") #'vhdl-template-insert-construct)
   (define-key vhdl-template-map (kbd "id") #'vhdl-template-insert-directive)
   (define-key vhdl-template-map (kbd "ip") #'vhdl-template-insert-package)
   (define-key vhdl-template-map "\C-h" nil)) ;; default to vhdl-template-header

(advice-add 'vhdl-beautify-region :before #'delete-trailing-whitespace)
;; (advice-remove 'vhdl-beautify-buffer #'delete-trailing-whitespace)

(with-eval-after-load 'vhdl-mode
  (when (and (not csb/simple-config) (executable-find "ghdl"))
    (flycheck-define-checker vhdl-ghdl
                             "A VHDL syntax checker using ghdl."
                             :command ("ghdl" "-s" "--std=93" "--ieee=synopsys" "-fexplicit" source)
                             :error-patterns
                             ((error line-start (file-name) ":" line ":" column
                                     ": " (message) line-end))
                             :modes vhdl-mode)))

  (add-hook 'vhdl-mode-hook (lambda () (flycheck-select-checker 'vhdl-ghdl)))

(with-eval-after-load 'vhdl-mode

  (defun csb/vhdl-update-doxygen ()
    (interactive)
    (async-shell-command "doxygen Doxyfile")
    (other-window 1)
    (delete-window))

  (define-key vhdl-mode-map (kbd "C-c M-ç") #'csb/vhdl-update-doxygen))

(with-eval-after-load 'vhdl-mode
  (setq vhdl-basic-offset 3)
  (setq vhdl-forbidden-words '("toto"))
  (setq vhdl-highlight-forbidden-words t)
  (setq vhdl-highlight-special-words t)
  (setq vhdl-highlight-translate-off t))

(add-hook 'vhdl-mode-hook
          (lambda ()
            ;; (font-lock-add-keywords  nil '(("\\`.*\\@.*\\`" 1 font-lock-warning-face t)))
            (highlight-indentation-mode -1)  ;; ||
            (font-lock-comment-annotations)
            (font-lock-doc-annotations)
            (auto-fill-mode)
            (turn-on-auto-fill)
            (vhdl-tools-mode 1)
            (flyspell-prog-mode)
            ;; (csb/mode-line-off)
            (vhdl-electric-mode 1)
            (setq-local show-trailing-whitespace t)
            ;; make `company-backends' local is critcal
            ;; or else, you will have completion in every major mode, that's very annoying !
            ;;(make-local-variable 'company-backends)
            ;;(add-to-list 'company-backends 'company-gtags)
            ))
;; (turn-on-orgstruct++)
;; (setq-local orgstruct-heading-prefix-regexp "^ *--! ")
;; (setq-local orgstruct-heading-prefix-regexp "^ *-- *\\(\\*.*\\)")
;; (ispell-minor-mode 1)
;; *
;; (add-to-list 'imenu-generic-expression '("@" "^ *--! *\\(\\*.*\\)" 1))
;; (add-to-list 'imenu-generic-expression '("@" "^\\s-*--!\\s-+\\(\\**\\)\\s-+@brief\\s-+\\(.+\\)" 1))
;; (setq imenu-generic-expression '(("@" "^\\s-*--!\\s-+\\(\\**\\)\\s-+@brief\\s-+\\(.+\\)" 2)))
;; instances
;; (add-to-list 'imenu-generic-expression '("@" "^\\s-*\\(\\(\\w\\|\\s_\\)+\\s-*:\\(\\s-\\|\n\\)*\\(entity\\s-+\\(\\w\\|\\s_\\)+\\.\\)?\\(\\w\\|\\s_\\)+\\)\\(\\s-\\|\n\\)+\\(generic\\|port\\)\\s-+map\\>" 1))
;; processes
;; (add-to-list 'imenu-generic-expression '("@"  "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(\\(postponed\\s-+\\|\\)process\\)" 1))
;; (setq imenu-generic-expression (delq (assoc "Process" imenu-generic-expression) imenu-generic-expression))
;; (setq imenu-generic-expression (delq (assoc "Instance" imenu-generic-expression) imenu-generic-expression))

(provide 'csb-vhdl)

;;; csb-vhdl.el ends here
