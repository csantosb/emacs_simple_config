;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(pdf-tools-install)
(setq auto-mode-alist (rassq-delete-all 'pdf-view-mode auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'". csb/open-pdf))

(defun csb/open-pdf()
  "Select between pdftotext or pdf-view mode depending on the window mode"
  (if (not (eq (window-system) 'x))
      (my-open-pdf-terminal)
    (pdf-view-mode)))

(defun my-open-pdf-terminal ()
  "Run pdftotext on the entire buffer."
  (interactive)
  (let ((modified (buffer-modified-p)))
    (erase-buffer)
    (shell-command
     (concat "pdftotext " (buffer-file-name) " -")
     (current-buffer)
     t)
    (set-buffer-modified-p modified)
    ;; (text-mode)
    ;; (setq-local show-trailing-whitespace nil)
    )
  )

(with-eval-after-load 'pdf-annot
  (setq pdf-annot-minor-mode-map-prefix "C-c M-a"))

(with-eval-after-load 'pdf-view
  (setq pdf-view-display-size 'fit-width))

(with-eval-after-load 'image-mode
  (define-key image-mode-map (kbd "C-f") 'image-forward-hscroll)
  (define-key image-mode-map (kbd "C-b") 'image-backward-hscroll)
  (define-key image-mode-map (kbd "C-n") 'image-scroll-up)
  (define-key image-mode-map (kbd "C-p") 'image-scroll-down)
  (define-key image-mode-map (kbd "RET") 'image-next-line))

(with-eval-after-load 'pdf-view

  ;; Navigation
  (define-key pdf-view-mode-map (kbd "n")         'pdf-view-next-page-command)
  (define-key pdf-view-mode-map (kbd "p")         'pdf-view-previous-page-command)

  (define-key pdf-view-mode-map (kbd "SPC")       'pdf-view-scroll-up-or-next-page)
  (define-key pdf-view-mode-map (kbd "S-SPC")     'pdf-view-scroll-down-or-previous-page)
  (define-key pdf-view-mode-map (kbd "DEL")       'pdf-view-scroll-down-or-previous-page)

  (define-key pdf-view-mode-map (kbd "j")         'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k")         'pdf-view-previous-line-or-previous-page)

  ;; navigation
  ;; (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  ;; (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
  ;; (define-key pdf-view-mode-map (kbd "n") 'pdf-view-next-page)
  ;; (define-key pdf-view-mode-map (kbd "p") 'pdf-view-previous-page)

  ;; (define-key pdf-view-mode-map (kbd "g") 'pdf-view-first-page)
  ;; (define-key pdf-view-mode-map (kbd "G") 'pdf-view-last-page)

  ;; Scrolling
  ;; (define-key pdf-view-mode-map (kbd "SPC") 'pdf-view-scroll-up-or-next-page)
  ;; (define-key pdf-view-mode-map (kbd "S-SPC") 'pdf-view-scroll-down-or-previous-page)
  ;; (define-key pdf-view-mode-map (kbd "C-x G") 'pdf-view-goto-page)

  ;; scrolling
  ;; (define-key pdf-view-mode-map (kbd "SPC") 'scroll-up-line)
  ;; (define-key pdf-view-mode-map (kbd "S-SPC") 'scroll-down-line)
  ;; (define-key pdf-view-mode-map (kbd "C-SPC") '(lambda()(interactive)(scroll-up-command 8)))
  ;; (define-key pdf-view-mode-map (kbd "C-S-SPC") '(lambda()(interactive)(scroll-down-command 8)))


  (define-key pdf-view-mode-map (kbd "M-<")       'pdf-view-first-page)
  (define-key pdf-view-mode-map (kbd "M->")       'pdf-view-last-page)
  (define-key pdf-view-mode-map [remap goto-line] 'pdf-view-goto-page)
  (define-key pdf-view-mode-map (kbd "RET")       'image-next-line)

  ;; Zoom in/out.
  (define-key pdf-view-mode-map "+"               'pdf-view-enlarge)
  (define-key pdf-view-mode-map "="               'pdf-view-enlarge)
  (define-key pdf-view-mode-map "-"               'pdf-view-shrink)
  (define-key pdf-view-mode-map "0"               'pdf-view-scale-reset)

  ;; Fit the image to the window
  (define-key pdf-view-mode-map "w"               'pdf-view-fit-width-to-window)
  (define-key pdf-view-mode-map "h"               'pdf-view-fit-height-to-window)
  (define-key pdf-view-mode-map "P"               'pdf-view-fit-page-to-window)

  ;; Slicing the image
  (define-key pdf-view-mode-map (kbd "s m")       'pdf-view-set-slice-using-mouse)
  (define-key pdf-view-mode-map (kbd "s b")       'pdf-view-set-slice-from-bounding-box)
  (define-key pdf-view-mode-map (kbd "s r")       'pdf-view-reset-slice)

  ;; Reconvert
  (define-key pdf-view-mode-map (kbd "C-c C-c")   'doc-view-mode)
  (define-key pdf-view-mode-map (kbd "g")         'pdf-view-revert-buffer)
  (define-key pdf-view-mode-map (kbd "r")         'pdf-view-revert-buffer)

  ;; Region
  (define-key pdf-view-mode-map [down-mouse-1] 'pdf-view-mouse-set-region)
  (define-key pdf-view-mode-map [C-down-mouse-1] 'pdf-view-mouse-extend-region)
  (define-key pdf-view-mode-map [remap kill-region] 'pdf-view-kill-ring-save)
  (define-key pdf-view-mode-map [remap kill-ring-save] 'pdf-view-kill-ring-save)
  (define-key pdf-view-mode-map [remap mark-whole-buffer] 'pdf-view-mark-whole-page)

  ;; Exit
  ;; (define-key pdf-view-mode-map (kbd "Q") 'pdf-view-kill-proc-and-buffer)
  ;; (define-key pdf-view-mode-map (kbd "q") 'quit-window)
  ;; (define-key pdf-view-mode-map (kbd "q") 'pdf-view-kill-proc)
  (define-key pdf-view-mode-map (kbd "Q")         'kill-this-buffer)

  ;; Other
  (define-key pdf-view-mode-map (kbd "C-c C-d") 'pdf-view-dark-minor-mode)

  ;; (define-key pdf-view-mode-map (kbd "C-c C-c") 'pdf-view-toggle-display)
  ;; (define-key pdf-view-mode-map (kbd "C-c C-t") 'pdf-view-open-text)
  ;; (define-key pdf-view-mode-map (kbd "r") 'pdf-view-revert-buffer)

  ;; Help
  (define-key pdf-view-mode-map (kbd "?") 'describe-mode)

  ;; remove defaults
  (define-key pdf-view-mode-map (kbd "<next>") nil)
  (define-key pdf-view-mode-map (kbd "<prior>") nil)
  (define-key pdf-view-mode-map (kbd "<down>") nil)
  (define-key pdf-view-mode-map (kbd "<up>") nil)
  (define-key pdf-view-mode-map (kbd "H") nil)
  (define-key pdf-view-mode-map (kbd "W") nil)
  (define-key pdf-view-mode-map (kbd "C-n") nil)
  (define-key pdf-view-mode-map (kbd "C-p") nil)
  )

(with-eval-after-load 'pdf-history
  (define-key pdf-history-minor-mode-map (kbd "L") 'pdf-history-forward)
  (define-key pdf-history-minor-mode-map (kbd "H") 'pdf-history-backward)
  ;; nil
  (define-key pdf-history-minor-mode-map (kbd "B") 'nil)
  (define-key pdf-history-minor-mode-map (kbd "N") 'nil))

(with-eval-after-load 'pdf-isearch
     (define-key pdf-isearch-minor-mode-map (kbd "C-s") 'isearch-forward)
     (define-key pdf-isearch-minor-mode-map (kbd "C-r") 'isearch-backward)
     (define-key pdf-isearch-minor-mode-map (kbd "C-M-s") 'doc-view-search)
     (define-key pdf-isearch-minor-mode-map (kbd "C-M-r") 'doc-view-search-backward)
     (define-key pdf-isearch-minor-mode-map (kbd "M-s o") 'pdf-occur))

(with-eval-after-load 'pdf-isearch
  (define-key pdf-isearch-active-mode-map (kbd "C-d") 'pdf-misc-dark-mode)
  (define-key pdf-isearch-active-mode-map (kbd "C-b") 'pdf-isearch-batch-mode)
  (define-key pdf-isearch-active-mode-map (kbd "C-v") 'doc-view-scroll-up-or-next-page)
  (define-key pdf-isearch-active-mode-map (kbd "M-v") 'doc-view-scroll-down-or-previous-page))

(with-eval-after-load 'pdf-links
  (define-key pdf-links-minor-mode-map (kbd "f") 'pdf-links-isearch-link)
  (define-key pdf-links-minor-mode-map (kbd "F") 'pdf-links-do-action))

(with-eval-after-load 'pdf-occur
  (define-key pdf-occur-buffer-mode-map (kbd "h") 'pdf-occur-goto-occurrence)
  (define-key pdf-occur-buffer-mode-map (kbd "l") 'pdf-occur-view-occurrence)
  (define-key pdf-occur-buffer-mode-map (kbd "F") 'next-error-follow-minor-mode)
  ;;
  (define-key pdf-occur-buffer-mode-map (kbd "RET") nil)
  (define-key pdf-occur-buffer-mode-map (kbd "C-o") nil)
  (define-key pdf-occur-buffer-mode-map (kbd "C-c C-f") nil))

(with-eval-after-load 'pdf-misc
  (define-key pdf-misc-minor-mode-map (kbd "C-w") 'pdf-misc-copy-region)
  (define-key pdf-misc-minor-mode-map (kbd "M-w") nil)
  (define-key pdf-misc-minor-mode-map (kbd "C-c C-d") 'pdf-misc-dark-mode)
  (define-key pdf-misc-minor-mode-map (kbd "I") 'pdf-misc-display-metadata)
  (define-key pdf-misc-minor-mode-map (kbd "s w") 'pdf-misc-crop-to-window)
  (define-key pdf-misc-minor-mode-map (kbd "s p") 'pdf-misc-crop-to-page))

(with-eval-after-load 'pdf-annot
  (define-key pdf-annot-list-mode-map (kbd "F") 'pdf-annot-list-follow-minor-mode)
  (define-key pdf-annot-list-mode-map (kbd "C-c C-f") nil)
  (define-key pdf-annot-list-mode-map (kbd "SPC") 'pdf-annot-display-annotation)
  (define-key pdf-annot-list-mode-map (kbd "RET") 'pdf-annot-display-annotation))

(with-eval-after-load 'pdf-annot
  (define-key pdf-annot-edit-contents-minor-mode-map (kbd "C-c C-c") 'pdf-annot-edit-contents-commit)
  (define-key pdf-annot-edit-contents-minor-mode-map (kbd "C-c C-k") 'pdf-annot-edit-contents-quit)
  (define-key pdf-annot-edit-contents-minor-mode-map (kbd "C-c C-q") nil))

(with-eval-after-load 'pdf-annot
    (define-key pdf-annot-minor-mode-map (kbd "C-c M-a l") 'pdf-annot-list-annotations)
    (define-key pdf-annot-minor-mode-map (kbd "C-c M-a t") 'pdf-annot-toggle-display-annotations)
    (define-key pdf-annot-minor-mode-map (kbd "C-c M-a a") 'pdf-annot-add-text-annot)
    (define-key pdf-annot-minor-mode-map (kbd "C-c M-a r") 'pdf-annot-revert-page)
    (define-key pdf-annot-minor-mode-map (kbd "C-c M-a R") 'pdf-annot-revert-document)
    (define-key pdf-annot-minor-mode-map (kbd "C-x s") 'pdf-annot-save-document)
    ;; pdf-annot-attach-dired
    ;;
    (define-key pdf-annot-minor-mode-map (kbd "C-c M-a C-a") 'nil)
    (define-key pdf-annot-minor-mode-map (kbd "C-c M-a C-d") 'nil)
    (define-key pdf-annot-minor-mode-map (kbd "C-c M-a C-l") 'nil)
    (define-key pdf-annot-minor-mode-map (kbd "C-c M-a C-r") 'nil)
    (define-key pdf-annot-minor-mode-map (kbd "C-c M-a d") 'nil))

(with-eval-after-load 'pdf-outline
  (define-key pdf-outline-minor-mode-map (kbd "C-c M-t") #'pdf-outline)
  (define-key pdf-outline-minor-mode-map (kbd "TAB")
    #'(lambda()(interactive)
        (pdf-outline)
        (pdf-outline-follow-mode t)
        (outline-hide-sublevels 1)))
  (define-key pdf-outline-minor-mode-map (kbd "o") nil))

(with-eval-after-load 'pdf-outline

  (define-key pdf-outline-buffer-mode-map "-" 'negative-argument)

  ;; Navigation
  (define-key pdf-outline-buffer-mode-map (kbd "p") 'previous-line)
  (define-key pdf-outline-buffer-mode-map (kbd "n") 'next-line)
  (define-key pdf-outline-buffer-mode-map (kbd "k") 'previous-line)
  (define-key pdf-outline-buffer-mode-map (kbd "j") 'next-line)
  (define-key pdf-outline-buffer-mode-map (kbd "M-<") 'beginning-of-buffer)
  (define-key pdf-outline-buffer-mode-map (kbd "M->") 'pdf-outline-end-of-buffer)
  (define-key pdf-outline-buffer-mode-map (kbd "<") 'beginning-of-buffer)
  (define-key pdf-outline-buffer-mode-map (kbd ">") 'pdf-outline-end-of-buffer)

  ;; Tree
  (define-key pdf-outline-buffer-mode-map (kbd "a") 'show-all)
  (define-key pdf-outline-buffer-mode-map (kbd "1") 'outline-hide-sublevels)
  (define-key pdf-outline-buffer-mode-map (kbd "l") #'show-subtree)
  (define-key pdf-outline-buffer-mode-map (kbd "h") #'(lambda()(interactive)(pdf-outline-up-heading 1)(hide-subtree)))

  (define-key pdf-outline-buffer-mode-map (kbd "u") 'pdf-outline-up-heading)
  (define-key pdf-outline-buffer-mode-map (kbd "f") 'outline-forward-same-level)
  (define-key pdf-outline-buffer-mode-map (kbd "b") 'outline-backward-same-level)

  ;; Follow

  ;; outline-toggle-children
  ;; pdf-outline-follow-link - go to the location and move point to the original buffer
  ;; pdf-outline-display-link - stay in toc

  ;; get ther
  (define-key pdf-outline-buffer-mode-map (kbd "SPC") #'pdf-outline-follow-link-and-quit)
  (define-key pdf-outline-buffer-mode-map (kbd "RET") #'pdf-outline-follow-link-and-quit)
  (define-key pdf-outline-buffer-mode-map (kbd ".") 'pdf-outline-move-to-current-page) ;; move point in toc to current pdf page

  ;; Follow mode: document is updated as point moves in toc
  (define-key pdf-outline-buffer-mode-map (kbd "F") 'pdf-outline-follow-mode)
  (define-key pdf-outline-buffer-mode-map (kbd "C-c C-f") nil)

  (define-key pdf-outline-buffer-mode-map (kbd "o") 'pdf-outline-select-pdf-window)

  ;; Quit
  (define-key pdf-outline-buffer-mode-map (kbd "q") #'pdf-outline-quit)
  (define-key pdf-outline-buffer-mode-map (kbd "TAB") #'pdf-outline-quit-and-kill)
  (define-key pdf-outline-buffer-mode-map (kbd "Q") #'pdf-outline-quit-and-kill)

  ;; Nil - remove defaults
  (define-key pdf-outline-buffer-mode-map [mouse-1] 'pdf-outline-mouse-display-link))

(add-hook 'pdf-view-mode-hook
          #'(lambda ()
              (csb/mode-line-off)
              (setq-local debug-on-error nil)
              (set-window-fringes (selected-window) 60 60 nil)))

(provide 'csb-pdf)

;;; csb-pdf.el ends here
