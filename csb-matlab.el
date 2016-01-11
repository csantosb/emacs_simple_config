;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...

(require 'matlab-load) ;; all autoloads are here
;; (with-eval-after-load 'matlab (add-to-list 'company-backends 'company-matlab-shell))

(with-eval-after-load 'matlab
  (setq matlab-comment-column 50                             ;; Goal column for on-line comments
        matlab-comment-region-s "% "
        matlab-indent-function t
        ;; semanticdb-matlab-include-paths ("/programs/MATLAB/R2014b")
        semantic-matlab-root-directory  "/programs/MATLAB/R2014b"
        matlab-indent-level 4                                ;; Level to indent blocks.
        matlab-return-function 'matlab-indent-end-before-ret ;; customize RET handling with this function
        matlab-fill-code t                                   ;; auto-fill code
        matlab-fill-strings t                                ;; auto-fill strings
        matlab-highlight-block-match-flag t                  ;; enable matching block begin/end keywords
        matlab-vers-on-startup t
        matlab-handle-simulink nil                           ;; enable simulink keyword highlighting
        matlab-auto-fill t                                   ;; do auto-fill at startup
        matlab-functions-have-end nil                        ;; functions terminate with end
        matlab-cont-level 4                                  ;; level to indent continuation lines
        matlab-verify-on-save-flag t                         ;; enable code checks on save
        matlab-cont-requires-ellipsis t                      ;; does your MATLAB support implied elipsis
        matlab-vers-on-startup nil))

(with-eval-after-load 'matlab
  ;; (define-key km [(control h) (control m)] matlab-help-map)
  (define-key matlab-mode-map (kbd "C-h RET")  nil))

(with-eval-after-load 'matlab
  (define-key matlab-help-map "r" nil)       ;; run-command doesn't belong here
  (define-key matlab-mode-map (kbd "C-c ?") matlab-help-map)

  (defadvice matlab-shell-describe-command (around matlab-shell-describe-command-toto activate)
    (window-configuration-to-register :matlab-fullscreen)
    ad-do-it
    (jump-to-register :matlab-fullscreen)))

(with-eval-after-load 'matlab
  ;; (define-key km [(control c) (control c)] matlab-insert-map)
  (define-key matlab-mode-map (kbd "C-c C-c")  nil))

(with-eval-after-load 'matlab
  (define-key matlab-insert-map "'" nil)    ;; stringgify-region doesn't belong here
  (define-key matlab-insert-map "\C-s" nil) ;; matlab-ispell-strings doesn't belong here
  (define-key matlab-insert-map "\C-c" nil) ;; matlab-ispell-comments doesn't belong here
  (define-key matlab-mode-map (kbd "C-c C-t") matlab-insert-map))

(with-eval-after-load 'matlab

  (defvar matlab-ispell-map
    (let ((km (make-sparse-keymap)))
      (define-key km "\C-s" 'matlab-ispell-strings)
      (define-key km "\C-c" 'matlab-ispell-comments)
      (define-key km "\C-r" 'matlab-stringgify-region)
      km))

  (define-key matlab-mode-map (kbd "C-c i") matlab-ispell-map))

(with-eval-after-load 'matlab

  (defun matlab-shell-run-region-or-buffer ()
    "Run region from BEG to END and display result in MATLAB shell.
    If region is not active run the current buffer.
    This command requires an active MATLAB shell."
    (interactive)
    (if (and transient-mark-mode mark-active)
        (matlab-shell-run-region (mark) (point))
      (window-configuration-to-register :matlab-shell-tmp)
      (matlab-shell-save-and-go)
      (jump-to-register :matlab-shell-tmp)))

  (define-key matlab-mode-map (kbd "C-c C-l") #'matlab-shell-run-region-or-line)    ;; send region or line
  (define-key matlab-mode-map (kbd "C-c C-m") #'matlab-shell-run-command)           ;; run COMMAND and display result in a buffer
  (define-key matlab-mode-map (kbd "C-c C-c") #'matlab-shell-run-region-or-buffer)  ;; save this M file, and evaluate it in a MATLAB shell
  (define-key matlab-mode-map (kbd "C-c C-e") #'matlab-shell-run-cell)              ;; run the cell the cursor is in
  (define-key matlab-mode-map [(meta control return)] nil))                         ;; matlab-shell-run-cell

(with-eval-after-load 'matlab
  )

(with-eval-after-load 'matlab
  ;; (define-key matlab-mode-map (kbd "C-M-i")  'matlab-complete-symbol)
)
  ;; (define-key matlab-mode-map (kbd "TAB")    'matlab-complete-symbol)

(with-eval-after-load 'matlab)
;; Miscellaneous
;; (define-key matlab-mode-map (kbd "C-c C-f p") 'matlab-fill-paragraph)
;; (define-key matlab-mode-map (kbd "C-c C-f r") 'matlab-fill-region)

(with-eval-after-load 'matlab
  (add-hook 'matlab-mode-hook
            (lambda ()
              (run-hooks 'prog-mode-hook)  ;; not done by default
              (ggtags-mode 1)
              ;; (helm-gtags-mode 1)
              (csb/debug-mode 1)
              (auto-fill-mode 0)
              (flycheck-mode -1)
              (setq-local matlab-indent-function-body t )                       ;; if you want function bodies indented
              ;; (make-local-variable 'company-backends)
              ;; (add-to-list 'company-backends 'company-matlab-shell)
              (matlab-cedet-setup)  ;; Enable CEDET feature support for MATLAB code
              (semantic-mode 1)
              (when (not (string= (system-name) "vb-arch"))(mlint-minor-mode 1)) ;; Mlint
              (when (not (string= (system-name) "vb-arch"))(matlab-toggle-show-mlint-warnings)) ;; MLint ON by default
              ;; (setq-local orgstruct-heading-prefix-regexp " *%% ")
              ;; IMenu
              ;; (add-to-list 'imenu-generic-expression '(nil  "^%% *\\(.*\\)" 1))
              ;; matlab-imenu-generic-expression is defined in matlab.el
              ;; (add-to-list 'imenu-generic-expression '(nil "^ *%% +\\([*]+ .*\\)" 0))
              ;; (add-to-list 'imenu-generic-expression '(nil "^ *%% *\\(\\*.*\\)" 1))
              ;; (add-to-list 'imenu-generic-expression '("Class" "^\\s-*classdef\\>[ \t\n.]*\\(\\(\\[[^]]*\\]\\|\\sw+\\)[ \t\n.]*\< =\[ \t\n.]*\\)?\\([a-zA-Z0-9_]+\\)" 3))
              ;; (eval-after-load 'matlab '(diminish 'mlint-minor-mode))
              ;; (ecb-activate)
              (setq-local show-trailing-whitespace t)
              ;; imenu-default-create-index-function is better than gtags
              ;; (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
              ;; Helm Dash
              (require 'helm-dash)
              (setq-local helm-dash-common-docsets '("MATLAB"))
              (setq-local helm-dash-docsets '("MATLAB"))
              (helm-dash-activate-docset "MATLAB"))))

(with-eval-after-load 'matlab
  (csb/toggle-terminals "MATLAB")
  ;; (define-key csb/hydra-toggle/keymap (kbd "s") #'csb/term-toggle-MATLAB)
  ;;(define-key matlab-mode-map (kbd "C-x t s") #'csb/term-toggle-MATLAB)
  )

(with-eval-after-load 'matlab
  (define-key matlab-mode-map (kbd "C-c C-s") 'matlab-show-matlab-shell-buffer)))

(with-eval-after-load 'matlab
  (setq matlab-shell-command-switches (if running-os-is-linux
                                          '("-nodesktop -nosplash -nosoftwareopengl")
                                        '("-nodesktop"))
        matlab-shell-command (if running-os-is-linux
                                 "/programs/MATLAB/R2014b/bin/matlab"
                               "/drives/c/MATLAB_R2013a/bin/matlab.exe")))

(with-eval-after-load 'matlab
  (add-hook 'matlab-shell-mode-hook
            (lambda ()
              ;; doesn't work as usual, that's why I put this here
              (define-key matlab-shell-mode-map (kbd "C-x t s") #'csb/toggle-matlab-shell)
              (csb/debug-mode 1)
              (setq-local show-trailing-whitespace nil)
              (smartscan-mode -1)
              ;; (make-local-variable 'company-backends)
              ;; (add-to-list 'company-backends 'company-matlab-shell)
              (setq-local smartscan-mode nil))))

(with-eval-after-load 'matlab
    (setq mlint-programs
        (if running-os-is-linux
            '("/programs/MATLAB/R2014b/bin/glnxa64/mlint")
          '("/drives/c/MATLAB_R2013a/bin/win32/mlint.exe"))
        mlint-program
        (if running-os-is-linux
            "/programs/MATLAB/R2014b/bin/glnxa64/mlint"
          "/drives/c/MATLAB_R2013a/bin/win32/mlint.exe")))

(with-eval-after-load 'mlint
  ;; (setq-default mlint-verbose t)
  (setq mlint-verbose t)
  ;; mlint-calculate-cyclic-complexity-flag
  ;; mlint-error-id-fix-alist
  ;; mlint-flags
  ;; mlint-lm-delete-focus
  ;; mlint-lm-entry
  ;; mlint-lm-entry-depricated
  ;; mlint-lm-entry-logicals
  ;; mlint-lm-entry-unused-argument
  ;; mlint-lm-group
  ;; mlint-lm-quiet
  ;; mlint-lm-replace-focus
  ;; mlint-lm-str2num
  ;; mlint-mark-group
  ;; mlint-minor-menu
  ;; mlint-minor-mode
  ;; mlint-minor-mode-map
  ;; mlint-minor-mode-was-enabled-before
  ;; mlint-output-regex
  ;; mlint-overlay-map
  ;; mlint-platform
  ;; mlint-program-selection-fcn
  ;; mlint-programs
  ;; mlint-scan-for-fixes-flag
  ;; mlint-symtab-info
  ;; mlint-symtab-line-regex
  ;; mlint-version
  )

(with-eval-after-load 'mlint
  (define-key mlint-minor-mode-map (kbd "C-c ! b") 'mlint-buffer)
  (define-key mlint-minor-mode-map (kbd "C-c ! l") 'mlint-emacs-popup-kludge)
  (define-key mlint-minor-mode-map (kbd "C-c ! o") 'mlint-mark-ok)
  (define-key mlint-minor-mode-map (kbd "C-c ! m") 'mlint-minor-menu)
  ;; Next
  (define-key mlint-minor-mode-map (kbd "C-c ! n") 'mlint-next-buffer)
  (define-key mlint-minor-mode-map (kbd "C-c ! m") 'mlint-next-buffer-new)
  ;; Previous
  (define-key mlint-minor-mode-map (kbd "C-c ! p") 'mlint-prev-buffer)
  (define-key mlint-minor-mode-map (kbd "C-c ! ,") 'mlint-prev-buffer-new)
  ;; Warnings
  (define-key mlint-minor-mode-map (kbd "C-c ! s") 'mlint-show-warning)
  (define-key mlint-minor-mode-map (kbd "C-c ! c") 'mlint-clear-warnings)
  (define-key mlint-minor-mode-map (kbd "C-c ! f") 'mlint-fix-warning))

(with-eval-after-load 'matlab
  (define-key matlab-mode-map (kbd "C-x t f") 'matlab-toggle-show-mlint-warnings))

(provide 'csb-matlab)

;;; csb-matlab.el ends here
