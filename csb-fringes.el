;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide)) fringe-bitmaps)

(defvar csb/window-fringes-enable t "Bla")

(defun csb/window-fringes ()
  (when csb/window-fringes-enable
    (let ((csb/fringe (cond ((eq (display-pixel-width) 1680) 250)
                            ((eq (display-pixel-width) 1920) 280)
                            (t 0)))
                            (csb/fringe-small 18))
      (cond
       ((not csb/window-fringes-enable)
        )
       ;; pdf mode
       ((and (string= major-mode 'pdf-view-mode)
             (= (count-windows ) 1))
        (set-window-fringes (selected-window) 60 60 nil))
       ;; calfw mode
       ((and (string= major-mode 'cfw:calendar-mode)
             (= (count-windows ) 1))
        (set-window-fringes (selected-window) 10 10 nil))
       ;; two or more windows vertical
       ((and (>= (count-windows ) 2) (window-full-height-p))
        (dolist (win (window-list))
          (set-window-fringes win csb/fringe-small csb/fringe-small nil)))
       ;; two or more windows horizontal
       ((and (>= (count-windows ) 2) (window-full-width-p))
        (dolist (win (window-list))
          (set-window-fringes win csb/fringe csb/fringe nil)))
       ;; three or more windows vertical
       ((and (>= (count-windows ) 3) (window-full-height-p))
        (dolist (win (window-list))
          (set-window-fringes win csb/fringe-small csb/fringe-small nil)))
       ;; three or more windows horizontal
       ((and (>= (count-windows ) 3) (window-full-width-p))
        (dolist (win (window-list))
          (set-window-fringes win csb/fringe csb/fringe nil)))
       ;; more than two windows
       ((> (count-windows ) 2)
        (dolist (win (window-list))
          (set-window-fringes win csb/fringe-small csb/fringe-small nil)))
       ;; one window
       ((and (= (count-windows ) 1) (window-full-height-p))
        (dolist (win (window-list))
          (set-window-fringes win csb/fringe csb/fringe nil))))
      (redraw-display))))

;; (set-fringe-style '(50 . 50))
;; (fringe-mode '( 50 . 50))
;; (set-window-fringes nil 0 0)

(defun csb/elfeed-fringes ()
  (let ((csb/fringe (cond ((eq (display-pixel-width) 1680) 250)
                          ((eq (display-pixel-width) 1920) 185)
                          (t 0))))
    (set-window-fringes (selected-window) csb/fringe csb/fringe nil))
  (redraw-display))

(defun csb/fringes-sx ()
  (when csb/window-fringes-enable
    (let ((csb/fringe-small 10)
          (csb/fringe (cond ((eq (display-pixel-width) 1680) 250)
                            ((eq (display-pixel-width) 1920) 280)
                            (t 0))))
      (cond
       ((not csb/window-fringes-enable)
        )
       ;; two or more windows vertical
       ((and (>= (count-windows ) 2) (window-full-height-p))
        (dolist (win (window-list))
          (set-window-fringes win csb/fringe-small csb/fringe-small nil)))
       ;; two or more windows horizontal
       ((and (>= (count-windows ) 2) (window-full-width-p))
        (dolist (win (window-list))
          (set-window-fringes win csb/fringe csb/fringe nil)))
       ;; three or more windows vertical
       ((and (>= (count-windows ) 3) (window-full-height-p))
        (dolist (win (window-list))
          (set-window-fringes win csb/fringe-small csb/fringe-small nil)))
       ;; three or more windows horizontal
       ((and (>= (count-windows ) 3) (window-full-width-p))
        (dolist (win (window-list))
          (set-window-fringes win csb/fringe csb/fringe nil)))
       ;; more than two windows
       ((> (count-windows ) 2)
        (dolist (win (window-list))
          (set-window-fringes win csb/fringe-small csb/fringe-small nil)))
       ;; one window
       ((and (= (count-windows ) 1) (window-full-height-p))
        (dolist (win (window-list))
          (set-window-fringes win csb/fringe csb/fringe nil))))
      (redraw-display))))

(add-hook 'window-configuration-change-hook 'csb/window-fringes)

(add-hook 'persp-switch-hook
          (lambda()
            (cond
             ;; elfeed mode
             ((string= (persp-name persp-curr) "elfeed")
              (progn
                (remove-hook 'window-configuration-change-hook 'csb/window-fringes)
                (csb/elfeed-fringes)
                (add-hook 'window-configuration-change-hook 'csb/elfeed-fringes)
                (load-theme 'darktooth t)))
             ;; out of elfeed mode
             ((string= (when persp-last (persp-name persp-last)) "elfeed")
              (progn
                (remove-hook 'window-configuration-change-hook 'csb/elfeed-fringes)
                (csb/window-fringes)
                (add-hook 'window-configuration-change-hook 'csb/window-fringes)
                (load-theme 'hc-zenburn t)))
             ;; sx mode
             ((string= (persp-name persp-curr) "sx")
              (progn
                (remove-hook 'window-configuration-change-hook 'csb/window-fringes)
                (csb/fringes-sx)
                (add-hook 'window-configuration-change-hook 'csb/fringes-sx)))
             ;; out of sx mode
             ((string= (when persp-last (persp-name persp-last)) "sx")
              (progn
                (remove-hook 'window-configuration-change-hook 'csb/fringes-sx)
                (csb/window-fringes)
                (add-hook 'window-configuration-change-hook 'csb/window-fringes))))))

(add-hook 'persp-switch-hook
          (lambda()
            (cond
             ;; calendar mode
             ((string= (persp-name persp-curr) "calendar")
              (blink-cursor-mode 1))
             ;; out of calendar mode
             ((string= (when persp-last (persp-name persp-last)) "calendar")
              (blink-cursor-mode -1)))))

(provide 'csb-fringes)

;;; csb-fringes.el ends here

;; (defvar bzg-big-fringe-mode nil)

;; (define-minor-mode bzg-big-fringe-mode
;;   "Minor mode to hide the mode-line in the current buffer."
;;   :init-value nil
;;   :global t
;;   :variable bzg-big-fringe-mode
;;   :group 'editing-basics
;;   (if (not bzg-big-fringe-mode)
;;       (set-fringe-style nil)
;;     (set-fringe-mode
;;      (/ (- (frame-pixel-width)
;;           (* 200 (frame-char-width)))
;;       2)
;;    )))

;; (defun bzg-big-fringe-mode ()
;;  "Minor mode to hide the mode-line in the current buffer."
;;  (if (not bzg-big-fringe-mode)
;;      (set-fringe-style nil)
;;    (set-fringe-mode
;;     (/ (- (frame-pixel-width)
;;          (* 100 (frame-char-width)))
;;      2)
;;   )))
;; (window-body-width)

;; Now activate this global minor mode
;; (bzg-big-fringe-mode 1)

;; Set the color of the fringe
;; (custom-set-faces '(fringe ((t (:background "white")))))
