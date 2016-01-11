;;; csb-julia --- Summary
;;
;;; Commentary:
;;
;; This file contains ...

(with-eval-after-load 'ess-julia
  ;; (setq matlab-comment-column 50 )
  )

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

(with-eval-after-load 'ess-julia

  (defun csb/ess-inf-run-region-or-buffer ()
    "Run region from BEG to END and display result in MATLAB shell.
        If region is not active run the current buffer.
        This command requires an active MATLAB shell."
    (interactive)
    (if (and transient-mark-mode mark-active)
        (ess-eval-region (mark) (point) nil)
      (progn
        (window-configuration-to-register :matlab-shell-tmp)
        (save-buffer)
        (call-interactively 'ess-load-file)
        (jump-to-register :matlab-shell-tmp))))

  (defun csb/ess-inf-run-line ()
    "Run region from BEG to END and display result in MATLAB shell.
  pIf region is not active run the current line.
  This command requires an active MATLAB shell."
    (interactive)
    (ess-eval-region (line-beginning-position) (line-end-position) nil))

  (define-key ess-julia-mode-map (kbd "C-c C-l") #'csb/ess-inf-run-line)    ;; send region or line
  (define-key ess-julia-mode-map (kbd "C-c C-c") #'csb/ess-inf-run-region-or-buffer)  ;; save this M file, and evaluate it in a MATLAB shell
  )

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

(with-eval-after-load 'ess-julia
  (add-hook 'ess-julia-mode-hook
            (lambda ()
              (make-local-variable 'csb/hydra-toggle/keymap)
              (define-key csb/hydra-toggle/keymap (kbd "s") #'csb/term-toggle-julia))))

(with-eval-after-load 'ess-julia
  (csb/toggle-terminals "julia"))

(with-eval-after-load 'ess-julia
  (define-key ess-julia-mode-map (kbd "C-c C-s") #'ess-switch-to-inferior-or-script-buffer))

(with-eval-after-load 'ess-julia
  (add-hook 'inferior-ess-mode-hook
            (lambda ()
              (make-local-variable 'csb/hydra-toggle/keymap)
              (define-key csb/hydra-toggle/keymap (kbd "s") #'csb/term-toggle-julia)
              (setq-local show-trailing-whitespace nil)
              (smartscan-mode -1))))

(provide 'csb-julia)

;;; csb-julia.el ends here
