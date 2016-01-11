;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(define-minor-mode csb/debug-mode
  "Get your foos in the right places."
  :lighter " csb/dbg"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-u u") #'gud-until)   ;; Continue execution to the current line
            (define-key map (kbd "C-c C-u <up>") #'gud-up)
            (define-key map (kbd "C-c C-u <down>") #'gud-down)
            (define-key map (kbd "C-c C-u d") #'gud-remove)  ;; Delete the breakpoint(s) on the current source line
            (define-key map (kbd "C-c C-u g") #'gud-refresh)
            (define-key map (kbd "C-c C-u b") #'gud-break)   ;; set breakpoint at current line.
            (define-key map (kbd "C-c C-u c") #'gud-cont)
            (define-key map (kbd "<f5>") #'gud-finish)
            (define-key map (kbd "<f6>") #'gud-next)
            (define-key map (kbd "<f7>") #'gud-step)
            (define-key map (kbd "<f8>") #'gud-cont)
            (define-key map (kbd "C-c C-u i") #'gud-step)    ;; equiv matlab step in
            (define-key map (kbd "C-c C-u s") #'gud-next)    ;; equiv matlab step 1
            (define-key map (kbd "C-c C-u f") #'gud-finish)  ;; equiv matlab step out
            map)
  :global nil )

(provide 'csb-debug)
;;; csb-debug.el ends here
