;;(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x\C-d" 'dired-jump)
(define-key global-map "\C-xd" 'dired)
(define-key global-map "\C-x4\C-d" 'dired-jump-other-window)

(with-eval-after-load 'dired
              (defun my_dired_find_file ()
                " does a dired-find-file + a close dired
                    wrap around the original dired function"
                (interactive)
                ;; save current dired buffer
                (let ((dired-buffer (current-buffer)))
                  ;; call the original function
                  (dired-find-file)
                  ;; close direx
                  (kill-buffer dired-buffer))))

(with-eval-after-load 'dired
  (defun my-dired-up-directory ()
    "Take dired up one directory, but behave like dired-find-alternate-file"
    (interactive)
    (let ((old (current-buffer)))
      (dired-up-directory)
      (kill-buffer old))))

(with-eval-after-load 'dired (lambda()
                               (require 'dired-x)
                               (setq diredp-hide-details-initially-flag nil)
                               (require 'dired+)))

(with-eval-after-load 'dired

  ;; close
  (define-key dired-mode-map (kbd "Q") 'kill-this-buffer)
  (define-key dired-mode-map "Q" 'kill-this-buffer)

  ;; help
  (define-key dired-mode-map "?" 'dired-summary)
  (define-key dired-mode-map (kbd "C-h m") 'describe-mode)

  ;; (define-key dired-mode-map "n" 'evil-search-next)
  ;; (define-key dired-mode-map "N" 'evil-search-previous)
  ;; (define-key dired-mode-map "o" 'dired-sort-toggle-or-edit)
  ;; (define-key dired-mode-map "v" 'dired-toggle-marks)
  ;; (define-key dired-mode-map "m" 'dired-mark)
  ;; (define-key dired-mode-map "u" 'dired-unmark)
  ;; (define-key dired-mode-map "U" 'dired-unmark-all-marks)
  ;; (define-key dired-mkill-otherode-map "c" 'dired-create-directory)

  ;; navigation

  ;; hjkl keys as usual
  (define-key dired-mode-map "h" 'my-dired-up-directory)
  (define-key dired-mode-map "l" 'dired-find-alternate-file)
  (define-key dired-mode-map "j" 'dired-next-line)
  (define-key dired-mode-map "k" 'dired-previous-line)

  ;; jump
  (define-key dired-mode-map "J" 'dired-goto-file)
  (define-key dired-mode-map (kbd "C-j") 'my_dired_find_file)

  ;; disable defaults
  (define-key dired-mode-map "^" nil)
  (define-key dired-mode-map "a" nil)
  (define-key dired-mode-map (kbd "C-o")  nil))

(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

(add-hook 'dired-mode-hook
          (lambda ()
            ;; (dired-omit-mode 1)
            (require 'dired-imenu)
            (dired-setup-imenu)
            (csb/mode-line-off)))

   (add-hook 'dired-load-hook
             (lambda ()
               (load "dired-x")
               ;; Set dired-x global variables here.  For example:
               ;; (setq dired-guess-shell-gnutar "gtar")
               ;; (setq dired-x-hands-off-my-keys nil)
               ))

(stante-after wdired
  (define-key wdired-mode-map "\C-x\C-s" 'wdired-finish-edit)
  (define-key wdired-mode-map "\C-c\C-c" 'wdired-finish-edit)
  (define-key wdired-mode-map "\C-c\C-k" 'wdired-abort-changes)
  (define-key wdired-mode-map "\C-c\C-[" 'wdired-abort-changes)
  (define-key wdired-mode-map "\C-x\C-q" 'wdired-exit)
  (define-key wdired-mode-map "\C-m"     'ignore)
  (define-key wdired-mode-map "\C-j"     'ignore)
  (define-key wdired-mode-map "\C-o"     'ignore)
  (define-key wdired-mode-map [up]       'wdired-previous-line)
  (define-key wdired-mode-map "\C-p"     'wdired-previous-line)
  (define-key wdired-mode-map [down]     'wdired-next-line)
  (define-key wdired-mode-map "\C-n"     'wdired-next-line))

(add-hook 'direx:direx-mode-hook
            (lambda ()
              (defun my_maybe_find_item (&optional item)
                " does a maybe-find-item + a close direx
                  wrap around the original direx function"
                (interactive)
                ;; save current direx buffer
                (let ((direx-buffer (current-buffer)))
                  ;; call the original function
                  (direx:maybe-find-item)
                  ;; close direx
                  (kill-buffer direx-buffer)))

              ;; open direx buffer and press ? for keybinds
              (define-key direx:direx-mode-map (kbd "C-j") 'my_maybe_find_item)
              (define-key direx:direx-mode-map (kbd "j") 'direx:next-item)
              (define-key direx:direx-mode-map (kbd "k") 'direx:previous-item)
              (define-key direx:direx-mode-map (kbd "l") 'direx:maybe-find-item)
              (define-key direx:direx-mode-map (kbd "h") 'direx_toggle-item)
              (define-key direx:direx-mode-map (kbd "u") 'direx:up-item)

              (define-key direx:direx-mode-map (kbd "^") nil)
              ;; (define-key direx:direx-mode-map (kbd "C-n") nil)
              ;; (define-key direx:direx-mode-map (kbd "C-p") nil)
              ;; (define-key direx:direx-mode-map (kbd "n") nil)
              ;; (define-key direx:direx-mode-map (kbd "p") nil)
              (define-key direx:direx-mode-map (kbd "g") nil)
              (define-key direx:direx-mode-map (kbd "r") 'direx:refresh-whole-tree)
              ;; (my_windows_local)
              )
            )



;; (push '(direx:direx-mode :position left :width 50 :dedicated t)
;;      popwin:special-display-config)

(global-set-key "\C-x4\C-j" 'direx:jump-to-directory-other-window)
;; (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)
;; (global-set-key (kbd "C-x C-j") nil)

(provide 'csb-dired)

;;; csb-dired.el ends here
