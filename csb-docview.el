;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(with-eval-after-load 'doc-view

  ;; Exit
  (define-key doc-view-mode-map (kbd "Q") 'doc-view-kill-proc-and-buffer)
  (define-key doc-view-mode-map (kbd "q") 'quit-window)
  (define-key doc-view-mode-map (kbd "q") 'doc-view-kill-proc)
  ;;
  (define-key doc-view-mode-map (kbd "k") nil)
  (define-key doc-view-mode-map (kbd "K") nil)
  (define-key doc-view-mode-map (kbd "H") nil)

  (define-key doc-view-mode-map (kbd "C-c C-c") 'doc-view-toggle-display)
  (define-key doc-view-mode-map (kbd "C-c C-t") 'doc-view-open-text)
  (define-key doc-view-mode-map (kbd "r") 'doc-view-revert-buffer)

  ;; navigation
  (define-key doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page)
  (define-key doc-view-mode-map (kbd "k") 'doc-view-previous-line-or-previous-page)
  (define-key doc-view-mode-map (kbd "n") 'doc-view-next-page)
  (define-key doc-view-mode-map (kbd "p") 'doc-view-previous-page)
  (define-key doc-view-mode-map (kbd "g") 'doc-view-first-page)
  (define-key doc-view-mode-map (kbd "G") 'doc-view-last-page)

  ;; Scrolling
  (define-key doc-view-mode-map (kbd "SPC") 'doc-view-scroll-up-or-next-page)
  (define-key doc-view-mode-map (kbd "S-SPC") 'doc-view-scroll-down-or-previous-page)
  (define-key doc-view-mode-map (kbd "C-x G") 'doc-view-goto-page)

  ;; Zoom
  (define-key doc-view-mode-map (kbd "+") 'doc-view-enlarge)
  (define-key doc-view-mode-map (kbd "-") 'doc-view-shrink)
  (define-key doc-view-mode-map (kbd "w") 'doc-view-fit-width-to-window)
  ;; (define-key doc-view-mode-map (kbd "H") 'doc-view-fit-width-to-window) ;; taken by pdf-tools
  (define-key doc-view-mode-map (kbd "h") 'doc-view-fit-page-to-window)

  ;; Help
  (define-key doc-view-mode-map (kbd "?") 'describe-mode)

  ;; Search  [[info:emacs#DocView%20Searching][info:emacs#DocView Searching]]
  ;; C-r             doc-view-search-backward
  ;; C-s             doc-view-search
  ;; C-t             doc-view-show-tooltip

  ;; Slicing
  ;; s b             doc-view-set-slice-from-bounding-box
  ;; s m         doc-view-set-slice-using-mouse
  ;; s r             doc-view-reset-slice
  ;; s s             doc-view-set-slice
  )

(with-eval-after-load 'doc-view
  (setq doc-view-shrink-factor 1.01)
  (setq show-trailing-whitespace nil)
  (setq doc-view-continuous t)
  (setq doc-view-resolution 200))

(add-hook 'doc-view-mode-hook (lambda ()
                                ;; (doc-view-fit-page-to-window)
                                (csb/mode-line-off)
                                (redraw-display)))

(provide 'csb-docview)

;;; csb-docview.el ends here
