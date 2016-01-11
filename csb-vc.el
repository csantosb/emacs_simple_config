;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(autoload 'magit-get-top-dir "magit" nil t)

(with-eval-after-load 'magit

  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  ;; (setq magit-turn-on-auto-revert-mode nil)
  (define-key magit-status-mode-map (kbd "q")
    (lambda()
      "Restores the previous window configuration and bury magit related buffers."
      (interactive)
      (bury-buffer)
      (jump-to-register :magit-fullscreen)))

  (define-key magit-status-mode-map (kbd "Q")
    (lambda()
      "Restores the previous window configuration and kills all magit related buffers"
      (interactive)
      (kill-this-buffer)
      (jump-to-register :magit-fullscreen)
      ;; kill all magit buffers
      (mapc (lambda(arg) (when (string-prefix-p "*magit" (buffer-name arg))
                      (kill-buffer arg)))
            (persp-buffers persp-curr)))))

;; (require 'f)
;; (eq (f-size "~/Syncthing/MobileOrg/mobileorg.org") 0)
(add-hook 'git-commit-mode-hook (lambda()(smartscan-mode -1)))

(defun csb/magit-status()
  (interactive)
  (require 'magit)
  (magit-status (projectile-project-root))
  (csb/mode-line-off))

(define-key launcher-map "m" #'csb/magit-status)

(defun endless/add-PR-fetch ()
  "If refs/pull is not defined on a GH repo, define it."
  (let ((fetch-address
         "+refs/pull/*/head:refs/pull/origin/*")
        (magit-remotes
         (magit-get-all "remote" "upstream" "fetch")))
    (unless (or (not magit-remotes)
                (member fetch-address magit-remotes))
      (when (string-match
             "github" (magit-get "remote" "origin" "url"))
        (magit-git-string
         "config" "--add" "remote.origin.fetch"
         fetch-address)))))

(add-hook 'magit-mode-hook #'endless/add-PR-fetch)

(defun csb/git-timemachine()
  (interactive)
  (require 'git-timemachine)
  (git-timemachine))
(define-key launcher-map "M" #'csb/git-timemachine)
(with-eval-after-load 'git-timemachine
  (setq git-timemachine-abbreviation-length 12)
  ;; details of the commit will be shown in the minibuffer
  (setq git-timemachine-show-minibuffer-details t))

(defun in-git-p () (not (string-match "^fatal" (shell-command-to-string "git rev-parse
  --git-dir"))))

(defun git-parse-status ()
  (interactive)
  (let ((U 0)   ; untracked files
        (M 0)   ; modified files
        (O 0)   ; other files
        (U-files "")
        (M-files "")
        (O-files ""))
    (dolist (line (split-string
                   (shell-command-to-string "git status --porcelain")
                   "\n"))
      (cond
       ;; ignore empty line at end
       ((string= "" line) nil)

       ((string-match "^\\?\\?" line)
        (setq U (+ 1 U))
        (setq U-files (concat U-files "\n" line)))

       ((string-match "^ M" line)
        (setq M (+ 1 M))
        (setq M-files (concat M-files "\n" line))
        )

       (t
        (message "detected other in %s" line)
        (setq O (+ 1 O))
        (setq O-files (concat O-files "\n" line)))))

    ;; construct propertized string
    (concat
     "("
     (propertize
      (format "M:%d" M)
      'face (list ':foreground (if (> M 0)
                                   "red"
                                 "forest green"))
      'help-echo M-files)
     "|"
     (propertize
      (format "U:%d" U)
      'face (list ':foreground (if (> U 0)
                                   "red"
                                 "forest green"))
      'help-echo U-files)
     "|"
     (propertize
      (format "O:%d" O)
      'face (list ':foreground (if (> O 0)
                                   "red"
                                 "forest green"))
      'help-echo O-files)
     ") ")))

(defun git-remote-status ()
  (interactive)
  (let* (;; get the branch we are on.
         (branch (s-trim
                  (shell-command-to-string
                   "git rev-parse --abbrev-ref HEAD")))
         ;; get the remote the branch points to.
         (remote (s-trim
                  (shell-command-to-string
                   (format "git config branch.%s.remote" branch))))
         (remote-branch (s-trim
                         (shell-command-to-string
                          "git for-each-ref --format='%(upstream:short)' $(git symbolic-ref -q HEAD)")))
         (commits (split-string
                   (s-trim
                    (shell-command-to-string
                     (format
                      "git rev-list --count --left-right HEAD...%s"
                      remote-branch)))))
         (local (nth 0 commits))
         (remotes (nth 1 commits)))
    (concat
     "["
     (propertize
      (format "%s" branch)
      'face (list :foreground "magenta"))
     "|"
     (format "↑%s|↓%s" local remotes)
     "]")))

(defvar git-modeline-last-update (float-time) "Last time we updated")
(defvar git-modeline-update-interval 15 "Minimum time between update in seconds")
(defvar git-modeline "" "Last value of the modeline")

(define-minor-mode git-mode
  "minor mode to put git repo status in modeline"
  nil nil nil
  (let ((git-modeline '(:eval (if
                                  (> (- (float-time) git-modeline-last-update)
                                     git-modeline-update-interval)
                                  ;; we are updating
                                  (setq git-modeline
                                        (if (not (in-git-p))
                                            ""
                                          (setq  git-modeline-last-update (float-time))
                                          (concat
                                           (git-remote-status)
                                           (git-parse-status))))

                                ;; use last value of the modeline
                                git-modeline))))
    (if git-mode
        ;; put in modeline
        (push git-modeline mode-line-format)
      ;; remove from modeline
      (setq mode-line-format
            (-remove (lambda (x)
                       (equal x git-modeline))
                     mode-line-format)))))

;; (define-key endless/toggle-map "g" 'git-mode)

(defvar csb/ediff-map (make-sparse-keymap "Ediff "))
(define-key csb/ediff-map (kbd "e") (cons "ediff" #'ediff))
(define-key csb/ediff-map (kbd "b") (cons "buffers" #'ediff-buffers))
(define-key csb/ediff-map (kbd "d") (cons "directories" #'ediff-directories))
(define-key csb/ediff-map (kbd "f") (cons "files" #'ediff-files))
(define-key csb/ediff-map (kbd "w") (cons "dwim" #'ediff-dwim))
(define-key csb/ediff-map (kbd "3") (cons "ediff3" #'ediff3))
(define-key csb/ediff-map (kbd "4") (cons "ediff-buffers3" #'ediff-buffers3))
(define-key csb/ediff-map (kbd "5") (cons "ediff-directories3" #'ediff-directories3))
(define-key launcher-map "e" csb/ediff-map)

(stante-after ediff-util
              (winner-mode 1)
              (ediff-setup-keymap)
              (setq ediff-window-setup-function 'ediff-setup-windows-plain
                    ediff-split-window-function 'split-window-horizontally
                    ediff-diff-options "-w")) ;; ignore whitespace

(add-hook 'diff-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))
(add-hook 'ediff-before-setup-hook (lambda()(window-configuration-to-register :ediff-register)))
(add-hook 'ediff-quit-hook (lambda()(jump-to-register :ediff-register)))

(defun ediff-dwim ()
  "Do ediff as I mean.

If a region is active when this command is called, call `ediff-regions-wordwise'.

Else if the current frame has 2 windows,
- Do `ediff-files' if the buffers are associated to files and the buffers
  have not been modified.
- Do `ediff-buffers' otherwise.

Otherwise call `ediff-buffers' interactively."
  (interactive)
  (if (region-active-p)
      (call-interactively 'ediff-regions-wordwise)
    (if (= 2 (safe-length (window-list)))
        (let (bufa bufb filea fileb)
          (setq bufa  (get-buffer (buffer-name)))
          (setq filea (buffer-file-name bufa))
          (save-excursion
            (other-window 1)
            (setq bufb (get-buffer (buffer-name))))
          (setq fileb (buffer-file-name bufb))
          (if (or
               ;; if either of the buffers is not associated to a file
               (null filea) (null fileb)
               ;; if either of the buffers is modified
               (buffer-modified-p bufa) (buffer-modified-p bufb))
              (progn
                (message "Running (ediff-buffers \"%s\" \"%s\") .." bufa bufb)
                (ediff-buffers bufa bufb))
            (progn
              (message "Running (ediff-files \"%s\" \"%s\") .." filea fileb)
              (ediff-files filea fileb))))
      (call-interactively 'ediff-buffers))))

(defun csb/toggle-diff-hl-mode()
  (interactive)
  (if diff-hl-mode
      (progn
        (diff-hl-mode -1)
        (diff-hl-flydiff-mode -1)
        (redraw-display)
        (message "diff hl disabled"))
    (progn
      (diff-hl-mode t)
      (diff-hl-flydiff-mode t)
      (redraw-display)
      (message "diff hl enabled"))))
;; (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
;; (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

(require 'diff-hl)
(global-diff-hl-mode t)
(global-diff-hl-amend-mode t)
(diff-hl-flydiff-mode t)

(with-eval-after-load 'diff-hl
  (define-key diff-hl-command-map "n" #'diff-hl-next-hunk)
  (define-key diff-hl-command-map "p" #'diff-hl-previous-hunk)
  (define-key diff-hl-command-map "[" nil)
  (define-key diff-hl-command-map "]" nil)
  (define-key diff-hl-command-map "=" nil)
  (define-key diff-hl-command-map "j" #'diff-hl-diff-goto-hunk)
  (define-key diff-hl-command-map "r" #'diff-hl-revert-hunk))

(stante-after hilit-chg
              (define-prefix-command 'my-highlight-key)
              (global-set-key (kbd "C-x h") 'my-highlight-key)
              (define-key my-highlight-key (kbd "n") 'highlight-changes-next-change)
              (define-key my-highlight-key (kbd "p") 'highlight-changes-previous-change)
              (define-key my-highlight-key (kbd "k") (lambda () (interactive)
                                                       (highlight-changes-remove-highlight
                                                        (point-min) (point-max))))
              (define-key my-highlight-key (kbd "R") 'highlight-changes-rotate-faces)
              (define-key my-highlight-key (kbd "s") 'highlight-symbol-at-point)
              (define-key my-highlight-key (kbd "v") 'highlight-changes-visible-mode)
              ;; toggle
              (defun csb/highlight-changes-toggle ()
                (interactive)
                (if highlight-changes-mode
                    (progn
                      (highlight-changes-mode -1)
                      (message "highlight changes off"))
                  (progn
                    (highlight-changes-mode t)
                    (message "highlight changes on"))))
              (define-key my-highlight-key (kbd "t") #'csb/highlight-changes-toggle))
;; (key-chord-define-global (kbd "th") #'csb/highlight-changes-toggle)
;; M-x highlight-compare-buffers highlights differences between two buffers.

(provide 'csb-vc)

;;; csb-vc.el ends here
