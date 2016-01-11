;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

;; (define-prefix-command 'endless/toggle-map)
(define-key ctl-x-map "t" #'csb/hydra-toggle/body)

(defvar whitespace-mode nil)
(defvar auto-fill-mode nil)
(defvar-local csb/outline-minor-mode nil)
(defvar read-only-mode nil)

(defhydra csb/hydra-toggle (:color blue :timeout 5)
  "
_q_   _A_ auto-fill-mode  %`auto-fill-mode     _m_ mode-line           nil     _i_ auto-insert  %`auto-insert-mode     _j_ changes:     %`highlight-changes-mode
    _r_ read-only-mode  %`buffer-read-only     _f_ flycheck-mode       %`flycheck-mode       _O_ org++        %`orgstruct-mode
    _d_ debug-on-error  %`debug-on-error       ___ modal-mode          %`modal-mode     _h_ high-indent  %`highlight-indentation-mode
    _t_ truncate-lines  %`truncate-lines       _c_ column-number-mode  %`column-number-mode       _b_ menu-bar     %`menu-bar-mode
    _w_ whitespace-mode %`whitespace-mode     _o_ outline-mode        %`outline-minor-mode     _l_ hl-diff:     %`diff-hl-mode "
  ("A" (lambda()(interactive)
         (if auto-fill-function
             (progn
               (message "auto fill disabled")
               (setq auto-fill-mode nil)
               (auto-fill-mode -1))
           (progn
             (message "auto fill enabled")
             (setq auto-fill-mode t)
             (auto-fill-mode)))) nil)
  ("e" csb/term-toggle-eshell nil)
  ("a" csb/term-toggle-ansi-term nil)
  ("s" nil nil :exit t)
  ("i" auto-insert-mode nil)
  ("r" read-only-mode nil :exit t)
  ("d" toggle-debug-on-error nil)
  ("t" toggle-truncate-lines nil)
  ("w" whitespace-mode nil)
  ("m" csb/toggle-mode-line nil)
  ("f" flycheck-mode nil)
  ("_" toggle-modal-mode nil)
  ("c" column-number-mode nil)
  ("b" menu-bar-mode nil)
  ("l" csb/toggle-diff-hl-mode nil)
  ("j" highlight-changes-mode nil)
  ("o" (lambda()(interactive)
         (if csb/outline-minor-mode
             (progn
               (message "outline minor disabled")
               (setq csb/outline-minor-mode nil)
               (outline-minor-mode -1))
           (progn
             (message "outline minor enabled")
             (setq csb/outline-minor-mode t)
             (outline-minor-mode t)))) nil)
  ("O" orgstruct++-mode nil)
  ("h" highlight-indentation-mode nil)
  ("q" nil nil))

(define-key csb/hydra-toggle/keymap (kbd "r")
  (lambda()(interactive)
    (cond
     ((string= major-mode "grep-mode") (wgrep-change-to-wgrep-mode))
     ((string= major-mode "occur-mode") (occur-edit-mode))
     (t (dired-toggle-read-only)))))

(provide 'csb-global-togglemap)
;;; csb-global-togglemap.el ends here
