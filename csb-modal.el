;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(defun csb/modal-next (arg)
  (interactive "p")
  (cond ((string= major-mode "elfeed-search-mode")
         (forward-line 1))
        ((string= major-mode "mu4e-headers-mode")
         (mu4e-headers-next))
        (t (if (bolp)
               (progn
                 (forward-paragraph arg)
                 (forward-line 1))
             (line-move arg)))))

(defun csb/modal-prev (arg)
  (interactive "p")
  (cond ((string= major-mode "elfeed-search-mode")
         (forward-line -1))
        ((string= major-mode "mu4e-headers-mode")
         (mu4e-headers-prev))
        (t (if (bolp)
               (progn
                 (forward-line -1)
                 (backward-paragraph arg)
                 (forward-line 1))
             (line-move (- arg))))))

(defun csb/modal-scroll-up (arg)
  (interactive "p")
  (cond ((string= major-mode "elfeed-search-mode")
         (next-line))
        ((string= major-mode "mu4e-headers-mode")
         (mu4e-headers-next))
        (t (scroll-up-line))))

(defun csb/modal-scroll-down (arg)
  (interactive "p")
  (cond ((string= major-mode "elfeed-search-mode")
         (previous-line))
        ((string= major-mode "mu4e-headers-mode")
         (mu4e-headers-prev))
        (t (scroll-down-line))))

(define-minor-mode modal-mode
  "Get your foos in the right places."
  :lighter " modal"
  :keymap (let ((map (make-sparse-keymap)))
            ;;
            (define-key map (kbd "n") #'csb/modal-next)
            (define-key map (kbd "p") #'csb/modal-prev)
            ;;
            (define-key map (kbd "j") #'csb/modal-scroll-up)
            (define-key map (kbd "k") #'csb/modal-scroll-down)
            ;;
            (define-key map (kbd "C-n") #'next-line)
            (define-key map (kbd "C-p") #'previous-line)
            ;;
            (define-key map (kbd "SPC") #'(lambda()(interactive)(scroll-up-command 8)))
            (define-key map (kbd "S-SPC") #'(lambda()(interactive)(scroll-down-command 8)))
            ;;
            map)
  :global nil
  (if modal-mode
      (progn
        (message "modal mode enabled")
        (with-eval-after-load "info"
          (define-key Info-mode-map (kbd "S-<backspace>") 'ace-link-info))
        (with-eval-after-load "help-mode"
          (define-key help-mode-map (kbd "S-<backspace>") 'ace-link-help))
        (with-eval-after-load "eww"
          (define-key eww-link-keymap (kbd "S-<backspace>") 'ace-link-eww)
          (define-key eww-mode-map (kbd "S-<backspace>") 'ace-link-eww)))
    (message "modal mode disabled")))

(defun toggle-modal-mode()
(interactive)
 (if modal-mode
     (modal-mode -1)
   (modal-mode t)))

(ace-link-setup-default (kbd "S-<backspace>"))

(provide 'csb-modal)
;;; csb-modal.el ends here
