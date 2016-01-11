;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(require 'paradox)
(paradox-require 'hydra)

(paradox-require 'ace-window)
(setq aw-keys '(?d ?f ?g ?h ?j ?k ?l ?r ?t ?y ?u ?i))
(setq aw-leading-char-style 'path)
(custom-set-faces
 '(aw-leading-char-face
   ((t (:inherit ace-jump-face-foreground :height 2.0)))))

(paradox-require 'buffer-move)

;;(require 'windcycle)

(paradox-require 'window-numbering)
(window-numbering-mode 1)
(setq window-numbering-auto-assign-0-to-minibuffer t)
(eval-after-load 'window-numbering
  '(progn
    ;; (define-key window-numbering-keymap (kbd "C-o 1") 'select-window-1)
    ;; (define-key window-numbering-keymap (kbd "C-o 2") 'select-window-2)
    ;; (define-key window-numbering-keymap (kbd "C-o 3") 'select-window-3)
    ;; (define-key window-numbering-keymap (kbd "C-o 4") 'select-window-4)
    )
  )

(require 'tiling)

(require 'winner)
(winner-mode 1)

(paradox-require 'switch-window)

(global-set-key (kbd "C-o")
                (defhydra csb/hydra-window
                  (:color red
                          :pre (setq csb/window-fringes-enable nil)
                          :timeout 2
                          :post (progn
                                  (setq csb/window-fringes-enable t)
                                  (csb/window-fringes)))
                  "
      ^^Point^^  ^^Window^^   ^^Buffer^^  ^Winner^   ^Resize^     ^Split^       ^Misc^       ^Tiling^         ^Resize^         ^Kill^
    ---------------------------------------------------------------------------------------------------------------------------------
      ^^^^        ^^    ^^     ^^    ^^     ^ ^    shr_i_nk    ^  ^            r_O_tate    _4_ 2x2
        ^_k_^      ^_wk_^       ^_bk_^      _U_    _=_balance  _\__ vertical    _s_elect    _c_ cycle         _rk_   ↑     _q_  kill other buffer
       _h_ _l_   _wh_  _wl_   _bh_  _bl_    _D_    _a_rea      _\|_ horizontal   _o_ther    _e_ master left   _rj_   ↓     _Q_  kill other buffer - delete window
        ^_j_^      ^_wj_^       ^_bj_^      ^ ^    _m_inimize  ^  ^             _n_ext     _u_ master up     _rl_ ←| |→   _0_  delete window
      ^^^^        ^^    ^^     ^^    ^^     ^ ^    _M_aximize  ^  ^             _p_rev     _-_ horizontal    _rh_ |↦ ↤|   _1_  delete other windows
      ^^^^        ^^    ^^     ^^    ^^     ^ ^    _f_it          ^^           ^\'^ ace     _v_ vertical     _SPC_ swap"
                  ;; point
                  ("j"  windmove-down nil)
                  ("k"  windmove-up nil)
                  ("h"  windmove-left nil)
                  ("l"  windmove-right nil)
                  ;;
                  ("bj" buf-move-down nil)
                  ("bk" buf-move-up nil)
                  ("bh" buf-move-left nil)
                  ("bl" buf-move-right nil)
                  ;;
                  ("wj" tiling-tile-down nil)
                  ("wk" tiling-tile-up nil)
                  ("wh" tiling-tile-left nil)
                  ("wl" tiling-tile-right nil)
                  ;;
                  ("c" tiling-cycle nil)
                  ("e" (csb/set-tile 0) nil)
                  ("u" (csb/set-tile 1) nil)
                  ("-" (csb/set-tile 2) nil)
                  ("v" (csb/set-tile 3) nil)
                  ("4" (csb/set-tile 4) nil)
                  ;;
                  ("U" (progn
                         (winner-undo)
                         (setq this-command 'winner-undo)) nil)
                  ("D" winner-redo nil)
                  ("s" switch-window nil :exit t)
                  ("'" ace-window nil :exit t)
                  ("SPC" ace-swap-window nil)
                  ;; "window resize"
                  ("=" balance-windows nil)
                  ("a" balance-windows-area nil)
                  ("m" minimize-window nil)
                  ("M" maximize-window nil)
                  ;;
                  ("n" select-next-window nil)
                  ("p" select-previous-window nil)
                  ("o" other-window nil :exit t)
                  ;;
                  ("rk" my-enlarge-window-vertically nil)
                  ("rl" my-enlarge-window-horizontally nil)
                  ("rj" my-shrink-window-vertically nil)
                  ("rh" my-shrink-window-horizontally nil)
                  ;;
                  ("i" shrink-window-if-larger-than-buffer nil)
                  ("f" fit-window-to-buffer nil)
                  ("O" rotate-windows nil)
                  ;; splitting
                  ("_"  (lambda ()
                          (interactive)
                          (split-window-below)
                          (windmove-down)) nil)
                  ("|"  (lambda ()
                          (interactive)
                          (split-window-right)
                          (windmove-right)) nil)
                  ;;
                  ("q" kill-other-buffer nil)
                  ("Q" kill-other-buffer-and-close nil :exit t)
                  ("0" delete-window nil  :exit t)
                  ("1" delete-other-windows nil  :exit t)))

(global-unset-key (kbd "C-)"))
(define-prefix-command 'window-map)
(global-set-key (kbd "C-)") 'window-map)

(define-key window-map (kbd "z t") 'transpose-frame)            ;; Swap x-direction and y-direction
(define-key window-map (kbd "z t") 'flip-frame)                 ;; flip vertically
(define-key window-map (kbd "z t") 'flop-frame)                 ;; flip horizontally
(define-key window-map (kbd "z t") 'rotate-frame)               ;; Rotate 180 degrees
(define-key window-map (kbd "z t") 'rotate-frame-clockwise)     ;; Rotate 90 degrees clockwise
(define-key window-map (kbd "z t") 'rotate-frame-anticlockwise) ;; Rotate 90 degrees anti-clockwise

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond
   ((not (> (count-windows) 1))
    (message "You can't rotate a single window!"))
   (t
    (let ((i 0)
          (num-windows (count-windows)))
      (while  (< i (- num-windows 1))
        (let* ((w1 (elt (window-list) i))
               (w2 (elt (window-list) (% (+ i 1) num-windows)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (setq i (1+ i))))))))

(define-key window-map (kbd "<return>") 'rotate-windows)
(global-set-key (kbd "C-<return>") 'rotate-windows)

(define-key window-map (kbd "'") 'ace-window)
(global-set-key (kbd "C-'") 'ace-window)

(define-key window-map (kbd "j") 'windmove-down)
(define-key window-map (kbd "k") 'windmove-up)
(define-key window-map (kbd "h") 'windmove-left)
(define-key window-map (kbd "l") 'windmove-right)
(setq windmove-wrap-around nil)

(define-key window-map "m"
  (defhydra hydra-win-buf-move ()
    "buffer move"
    ("k" buf-move-up "up")
    ("j" buf-move-down "down")
    ("h" buf-move-left "left")
    ("l" buf-move-right "right")))

(setq buffer-move-behavior 'swap)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun kill-other-buffer ()
  "Kill other buffer."
  (interactive)
  (save-excursion
    (other-window 1)
    (kill-this-buffer)
    (other-window 1)))

(defun kill-other-buffer-and-close ()
  "Kill other buffer."
  (interactive)
  (save-excursion
    (other-window 1)
    ;;        (quit-window)
    (kill-this-buffer)
    (delete-window)
    (other-window 1)))

(defun kill-buffer-and-close ()
  "Kill other buffer."
  (interactive)
  (save-excursion
    (kill-this-buffer)
    (delete-window)))

(define-key window-map (kbd "q") 'kill-other-buffer)
(define-key window-map (kbd "Q") (lambda()(interactive)(kill-other-buffer-and-close)))
(define-key window-map (kbd "0") (lambda()(interactive)(delete-window)))
(define-key window-map (kbd "1") (lambda()(interactive)(delete-other-windows)))

(global-set-key (kbd "C-x q") 'kill-this-buffer)
;; save-buffers-kill-terminal)
(global-set-key (kbd "C-x Q") (lambda()(interactive)(kill-buffer-and-close)))

;;    (define-key window-map (kbd "s k") 'buffer-up-swap)
;;    (define-key window-map (kbd "s j") 'buffer-down-swap)
;;    (define-key window-map (kbd "s l") 'buffer-right-swap)
;;    (define-key window-map (kbd "s h") 'buffer-left-swap)
  ;; (global-set-key (kbd "C-x <up>") 'windmove-up-cycle)
  ;; (global-set-key (kbd "C-x <down>") 'windmove-down-cycle)
  ;; (global-set-key (kbd "C-x <right>") 'windmove-right-cycle)
  ;; (global-set-key (kbd "C-x <left>") 'windmove-left-cycle)
  ;; (global-set-key (kbd "M-<up>") 'windmove-up-cycle)
  ;; (global-set-key (kbd "M-<down>") 'windmove-down-cycle)
  ;; (global-set-key (kbd "M-<right>") 'windmove-right-cycle)
  ;; (global-set-key (kbd "M-<left>") 'windmove-left-cycle)

(defhydra hydra-win-tiling (window-map "t")
  "tiling"
  ("k" tiling-tile-up "up")
  ("j" tiling-tile-down "down")
  ("h" tiling-tile-left "left")
  ("l" tiling-tile-right "right"))

(define-key window-map (kbd "SPC") 'tiling-cycle)

(defun csb/set-tile (&optional  arg)
  (interactive "P")
  (let ((bufs (mapcar 'window-buffer (window-list nil -1 nil))))
    (if (> (length bufs) 1)
        (cond ((= arg 0)
               (funcall 'tiling-master-left bufs))
              ((= arg 1)
               (funcall 'tiling-master-top bufs))
              ((= arg 2)
               (funcall 'tiling-even-horizontal bufs))
              ((= arg 3)
               (funcall 'tiling-even-vertical bufs))
              ((and (= arg 4) (= (length bufs) 4))
               (funcall 'tiling-tile-4 bufs)))
      (message "wrong number of windows for setting up a layout"))))

(define-key window-map (kbd "t t") 'csb/set-tile)

(define-key window-map (kbd "u") #'winner-undo)
(define-key window-map (kbd "d") #'winner-redo)

(define-key window-map (kbd "s") 'switch-window)

(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))

(define-key window-map (kbd "n") 'select-next-window)
(define-key window-map (kbd "p") 'select-previous-window)
(define-key window-map (kbd "o") 'other-window)
;;
;; move point to : conflits with org mode
;;
;; (global-set-key (kbd "M-k") 'windmove-up-cycle)
;; (global-set-key (kbd "M-j") 'windmove-down-cycle)
;; (global-set-key (kbd "M-l") 'windmove-right-cycle)
;; (global-set-key (kbd "M-h") 'windmove-left-cycle)

(define-key window-map "r"
 (defhydra hydra-win-resizing ()
   "window resize"
   ("=" balance-windows "balance")
   ("'" balance-windows-area "balance area")
   ("n" minimize-window "minimize")
   ("m" maximize-window "maximize")
   ("s" shrink-window-if-larger-than-buffer "shrink")
   ("f" fit-window-to-buffer "fit")))

;; remove key
(global-set-key (kbd "C-x +") nil) ;; default key for balance-windows

(defun my-enlarge-window-vertically ()
  (interactive)
  (enlarge-window 3))

(defun my-enlarge-window-horizontally ()
  (interactive)
  (enlarge-window-horizontally 3))

(defun my-shrink-window-vertically ()
  (interactive)
  (shrink-window 3))

(defun my-shrink-window-horizontally ()
  (interactive)
  (shrink-window-horizontally 3))

(define-key window-map (kbd "<up>")    (make-repeatable-command 'my-enlarge-window-vertically))
(define-key window-map (kbd "<right>") (make-repeatable-command 'my-enlarge-window-horizontally))
(define-key window-map (kbd "<down>")  (make-repeatable-command 'my-shrink-window-vertically))
(define-key window-map (kbd "<left>")  (make-repeatable-command 'my-shrink-window-horizontally))

(define-key window-map "\M-[1;5A" (make-repeatable-command 'my-enlarge-window-vertically))
(define-key window-map "\M-[1;5C" (make-repeatable-command 'my-enlarge-window-horizontally))
(define-key window-map "\M-[1;5B" (make-repeatable-command 'my-shrink-window-vertically))
(define-key window-map "\M-[1;5D" (make-repeatable-command 'my-shrink-window-horizontally))

(define-key window-map (kbd "_") #'split-window-below)
(define-key window-map (kbd "|") #'split-window-right)

(global-unset-key (kbd "C-x 2") )
(global-unset-key (kbd "C-x 3") )
(global-unset-key (kbd "C-x 0") )
(global-unset-key (kbd "C-x 1") )
(global-unset-key (kbd "C-x o") )
(global-unset-key (kbd "C-x |"))
(global-unset-key (kbd "C-x _"))

(defun split-window-multiple-ways (x y)
  "Split the current frame into a grid of X columns and Y rows."
  (interactive "nColumns: \nnRows: ")
  ;; one window
  (delete-other-windows)
  (dotimes (i (1- x))
      (split-window-horizontally)
      (dotimes (j (1- y))
        (split-window-vertically))
      (other-window y))
  (dotimes (j (1- y))
    (split-window-vertically))
  (balance-windows))

(defun get-window-in-frame (x y &optional frame)
  "Find Xth horizontal and Yth vertical window from top-left of FRAME."
  (let ((orig-x x) (orig-y y)
        (w (frame-first-window frame)))
    (while (and (windowp w) (> x 0))
      (setq w (windmove-find-other-window 'right 1 w)
            x (1- x)))
    (while (and (windowp w) (> y 0))
      (setq w (windmove-find-other-window 'down 1 w)
            y (1- y)))
    (unless (windowp w)
      (error "No window at (%d, %d)" orig-x orig-y))
    w))

(defun set-window-buffer-in-frame (x y buffer &optional frame)
  "Set Xth horizontal and Yth vertical window to BUFFER from top-left of FRAME."
  (set-window-buffer (get-window-in-frame x y frame) buffer))

(defun show-buffers-with-major-mode (mode)
  "Fill all windows of the current frame with buffers using major-mode MODE."
  (interactive
   (let* ((modes (loop for buf being the buffers
                              collect (symbol-name (with-current-buffer buf
                                                           major-mode)))))
     (list (intern (completing-read "Mode: " modes)))))
  (let ((buffers (loop for buf being the buffers
                              when (eq mode (with-current-buffer buf
                                                     major-mode))
                                     collect buf)))
    (dolist (win (window-list))
      (when buffers
        ;; (show-buffer win (car buffers))
        (set-window-buffer win (car buffers))
        (setq buffers (cdr buffers))))))

(with-eval-after-load 'csb-windows

  (defun find-windows-config-file-other ()
    "Edit the windows-mode config file, in another window."
    (interactive)
    (find-file-other-window (concat org-config-path "csb-windows.page")))

  (defun find-windows-config-file ()
    "Edit the windows-mode config file."
    (interactive)
    (find-file (concat org-config-path "csb-windows.page"))))

(provide 'csb-windows)
;;; csb-windows.el ends here
