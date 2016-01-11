;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(setq explicit-shell-file-name "/bin/sh")
(setq shell-file-name "/bin/sh")
;; (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))

(defmacro csb/toggle-terminals(term-type)
  `(defun ,(intern (format "csb/term-toggle-%s" term-type)) (arg)
     (interactive "P")
     (if (string=
          (cond ((string= ,term-type "ansi-term")
                 "term-mode")
                ((string= ,term-type "julia")
                 "inferior-ess-mode")
                ((string= ,term-type "MATLAB")
                 "matlab-shell-mode")
                ((string= ,term-type "eshell")
                 (format "%s-mode" ,term-type))
                (t ""))
          major-mode)
         ;; recover window config
         (jump-to-register :term-fullscreen)
       (progn
         ;; store current window config
         (window-configuration-to-register :term-fullscreen)
         (let ((height (/ (window-total-height) 3))
               (parent (if (buffer-file-name)
                           (file-name-directory (buffer-file-name))
                         default-directory)))
           (split-window-vertically (- height))
           (other-window 1)
           (if ;; check if an eshell already open in current persp
               (let ((term-exists))
                 (catch 'error
                   (dolist (thisbuffer (persp-buffers persp-curr))
                     (when (and (buffer-name thisbuffer)
                                (string-prefix-p (format "*%s*" ,term-type) (buffer-name thisbuffer)))
                       (setq term-exists t)
                       (throw 'error t))))
                 term-exists)
               (switch-to-buffer (format "*%s*" ,term-type))
             (cond ((string= ,term-type "ansi-term")
                    (ansi-term "/bin/zsh"))
                   ((string= ,term-type "julia")
                    (julia))
                   ((string= ,term-type "MATLAB")
                    (matlab-shell))
                   ((string= ,term-type "eshell")
                    (eshell))
                   (t "")))
           ;; cd to current dir
           (cond ((string= ,term-type "ansi-term")
                  (term-send-raw-string (format "cd %s \n" parent)))
                 ((string= ,term-type "MATLAB")
                  (matlab-shell-send-string (format "cd %s \n" parent)))
                 ((string= ,term-type "eshell")
                  (insert (format "cd %s" parent))
                  (eshell-send-input))
                 (t nil)))
         (cond ((equal arg '(4))
                (tiling-tile-up))
               ((equal arg '(16))
                (delete-other-windows))
               (t ))))))

;; (setq eshell-login-script (concat user-emacs-directory "eshell/login"))
;; (setq eshell-rc-script (concat user-emacs-directory "eshell/profile"))
(setq explicit-shell-file-name "/bin/sh")
(setq shell-file-name "/bin/sh")
(setq eshell-buffer-name "*eshell*")
(setq eshell-prefer-lisp-functions t)  ;; prefer Lisp functions to external commands.
;; (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))

(setq eshell-aliases-file "/home/csantos/.emacs.d/eshell/alias")

(defun eshell-update-aliases()
  "Copy zsh aliases file to eshell and perform modifs to make it usable"
  (interactive)
  (message "updating eshell alias from zsh")
  ;; get file
  (copy-file "~/Dropbox/config/zsh/aliases.zsh" eshell-aliases-file t t t)
  ;; fix aliases
  (with-current-buffer (find-file-noselect eshell-aliases-file)
    ;; comment out this line to avoid shading eshell/ls
    (goto-char (point-min))
    (while (re-search-forward "alias ls='ls" nil t)
      (replace-match "# alias ls='ls"))
    ;; chage string "='" for " "
    (goto-char (point-min))
    (while (re-search-forward "='" nil t)
      (replace-match " "))
    ;; chage string "'" for " "
    (goto-char (point-min))
    (while (re-search-forward "'" nil t)
      (replace-match " "))
    (save-buffer)
    (kill-this-buffer)))

(add-hook 'after-init-hook 'eshell-update-aliases)

;; (defun csb/toggle-eshell(arg)
;;   (interactive "P")
;;   (if (string= "eshell-mode" major-mode)
;;       ;; recover window config
;;       (jump-to-register :eshell-fullscreen)
;;     (progn
;;       ;; store current window config
;;       (window-configuration-to-register :eshell-fullscreen)
;;       (let ((height (/ (window-total-height) 3))
;;             (parent (if (buffer-file-name)
;;                         (file-name-directory (buffer-file-name))
;;                       default-directory)))
;;         (split-window-vertically (- height))
;;         (other-window 1)
;;         (if ;; check if an eshell already open in current persp
;;             (let ((eshell-exists))
;;               (catch 'error
;;                 (dolist (thisbuffer (persp-buffers persp-curr))
;;                   (when (and (buffer-name thisbuffer)
;;                              (string-prefix-p "*eshell" (buffer-name thisbuffer)))
;;                     (setq eshell-exists t)
;;                     (throw 'error t))))
;;               eshell-exists)
;;             (switch-to-buffer "*eshell*")
;;           (eshell))
;;         ;; cd to current dir
;;         (insert (format "cd %s" parent))
;;         (eshell-send-input))
;;       (cond ((equal arg '(4))
;;              (tiling-tile-up))
;;             ((equal arg '(16))
;;              (delete-other-windows))
;;             (t )))))
(csb/toggle-terminals "eshell")

;; (define-key launcher-map "s" #'th-shell-popup)
;; (define-key launcher-map "S" #'eshell-here)
;; (define-key endless/toggle-map (kbd "s") 'toggle-shell-full-screen)

(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
  (add-hook 'eshell-mode-hook #'(lambda ()
                                  (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
                                  (define-key eshell-mode-map (kbd "C-x C-h") 'helm-eshell-history)
                                  (smartscan-mode -1)
                                  (ansi-color-for-comint-mode-on)
                                  (setq-local show-trailing-whitespace nil)))
(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))
  (add-hook 'term-exec-hook 'oleh-term-exec-hook)

(add-hook 'shell-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))

;; (defun csb/toggle-ansi-term(arg)
;;   (interactive "P")
;;   (if (string= "term-mode" major-mode)
;;       ;; recover window config
;;       (jump-to-register :ansi-term-fullscreen)
;;     (progn
;;       ;; store current window config
;;       (window-configuration-to-register :ansi-term-fullscreen)
;;       (let ((height (/ (window-total-height) 3))
;;             (parent (if (buffer-file-name)
;;                         (file-name-directory (buffer-file-name))
;;                       default-directory)))
;;         (split-window-vertically (- height))
;;         (other-window 1)
;;         (if ;; check if an ansi-term already open in current persp
;;             (let ((ansi-term-exists))
;;               (catch 'error
;;                 (dolist (thisbuffer (persp-buffers persp-curr))
;;                   (when (and (buffer-name thisbuffer)
;;                              (string-prefix-p "*ansi-term" (buffer-name thisbuffer)))
;;                     (setq ansi-term-exists t)
;;                     (throw 'error t))))
;;               ansi-term-exists)
;;             (switch-to-buffer "*ansi-term*")
;;           (ansi-term "/bin/zsh"))
;;         ;; cd to current dir
;;         (term-send-raw-string (format "cd %s \n" parent)))
;;       (cond ((equal arg '(4))
;;              (tiling-tile-up))
;;             ((equal arg '(16))
;;              (delete-other-windows))
;;             (t )))))
(csb/toggle-terminals "ansi-term")

(add-hook 'term-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))

;;(require 'multi-term)
;; (setq multi-term-program "/bin/zsh")

(provide 'csb-terminal)

;;; csb-terminal.el ends here
