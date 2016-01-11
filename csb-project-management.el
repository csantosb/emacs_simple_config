;;; my_project_management.el --- Summary
;;
;;; License:
;;
;;; Commentary:
;;
;; This file contains defaults for variables as well as global keystrokes
;;
;;; Code:

(org-require 'csb-project-management-ggtags csb/compile-org-require)

(stante-after perspective
              ;; swithching
              (define-key persp-mode-map (kbd "C-x x S") 'persp-switch-quick)
              (define-key persp-mode-map (kbd "C-¿") 'persp-switch-quick)
              (define-key persp-mode-map (kbd "C-¡") 'persp-switch)
              (define-key persp-mode-map (kbd "C-x x s") 'persp-switch)
              (define-key persp-mode-map (kbd "C-x x n") 'persp-next)
              (define-key persp-mode-map (kbd "C-x x p") 'persp-prev)
              (define-key persp-mode-map (kbd "C-_") #'persp-switch)

              ;; killing
              (defun csb/projectile-perspective-kill()
                ""
                (interactive)
                (mapc (lambda(arg) (kill-buffer arg)) (persp-buffers persp-curr))
                (persp-kill (persp-name persp-curr)))
              (define-key persp-mode-map (kbd "C-x x k") #'persp-kill)
              (define-key persp-mode-map (kbd "C-x x K") #'csb/projectile-perspective-kill)
              (key-chord-define persp-mode-map (kbd "xk") #'csb/projectile-perspective-kill)

              (define-key persp-mode-map (kbd "C-x x A") 'persp-set-buffer)
              (define-key persp-mode-map (kbd "C-x x a") 'persp-add-buffer)
              (define-key persp-mode-map (kbd "C-x x d") 'persp-remove-buffer)
              (define-key persp-mode-map (kbd "C-x x i") 'persp-import-buffers)
              (define-key persp-mode-map (kbd "C-x x ,") 'persp-rename)

              ;; (define-key persp-mode-map (kbd "C-x x w") 'persp-save-state-to-file)
              ;; (define-key persp-mode-map (kbd "C-x x l") 'persp-load-state-from-file)
              (define-key persp-mode-map (kbd "C-x x <left>") nil)
              (define-key persp-mode-map (kbd "C-x x <right>") nil)
              (define-key persp-mode-map (kbd "C-x x c") nil))

(add-hook 'after-init-hook #'(lambda()(persp-mode t)))
(stante-after perspective
              (setq-default persp-show-modestring 'header)
              (if running-os-is-linux
                  (setq persp-interactive-completion-function 'ido-completing-read)
                (setq persp-interactive-completion-function 'completing-read))
              (setq persp-initial-frame-name "init")
              (setq-default persp-modestring-dividers '(" " " " " "))
              (require 'persp-projectile))

(projectile-global-mode 1)

(defun csb/projectile-todo()
  (interactive)
  (find-file-other-window (format "%sTODO.org" (projectile-project-root))))

(defun csb/projectile-readme()
  (interactive)
  (cond ((file-exists-p (format "%sreadme.org" (projectile-project-root)))
         (find-file-other-window (format "%sreadme.org"
                                         (projectile-project-root))))
        ((file-exists-p (format "%sREADME.org" (projectile-project-root)))
         (find-file-other-window (format "%sREADME.org"
                                         (projectile-project-root))))
        ((file-exists-p (format "%sreadme.md" (projectile-project-root)))
         (find-file-other-window (format "%sreadme.md"
                                         (projectile-project-root))))
        ((file-exists-p (format "%sREADME.md" (projectile-project-root)))
         (find-file-other-window (format "%sREADME.md"
                                         (projectile-project-root))))
        (t (find-file-other-window (format "%sREADME.org"
                                           (projectile-project-root))))))

(setq helm-projectile-sources-list '(helm-source-projectile-projects
                                     helm-source-projectile-files-list))
;; default is
;; (setq helm-projectile-sources-list '(helm-source-projectile-buffers-list
;;                                      helm-source-projectile-files-list
;;                                      helm-source-projectile-projects))

(helm-projectile-on)

(helm-projectile-on)

(with-eval-after-load 'projectile
  (setq projectile-completion-system 'helm
        projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-tags-command "ctags -Re -f %s %s" ;; doesn't matter if using gtags
        projectile-remember-window-config t
        projectile-mode-line-lighter "P"
        projectile-mode-line '(:eval
                               (format " Projectile" ))
                                        ; projectile-dired, projectile-find-file, helm-projectile
        projectile-switch-project-action 'projectile-dired ;; 'helm-projectile-find-file
        projectile-enable-idle-timer t
        projectile-idle-timer-seconds 60
        projectile-idle-timer-hook '(projectile-regenerate-tags)))

;; (add-hook 'projectile-idle-timer-hook 'my-projectile-idle-timer-function)
(with-eval-after-load 'projectile
   (define-key projectile-command-map (kbd "<return>") #'csb/projectile-todo)
   (define-key projectile-command-map (kbd "<backspace>") #'csb/projectile-readme))

;; current project name
(defvar prj-current-name "template")

;; current project home dir
(defvar prj-current-home-dir "~/Documents/OrgProjects/template")

;; list of valid projects
(defconst prj-list-of-projects '("positioner" "small_daq" "template"))

;; ;; list of screens
;; (defvar prj-screens-list nil)

(defun prj-new-tab(NAME CREATE)
  (if CREATE
      (elscreen-create)            ;; create new tab
    )
  (elscreen-screen-nickname NAME)  ;; rename screen
  )

(defun prj-buffer-exists (NAME)
  "Check if buffer already open."
  (not (eq nil (get-buffer NAME))))

(defun prj-OpenBuffer(FILE NAME TOTO)
  "Opens a buffer FILE and renames it to NAME.
Checks if buffer already exists by its name.
The file appears at 'org_projects_home_dir/project-name/file'"
  (if (not (prj-buffer-exists NAME))
      (progn
        (find-file (concat prj-current-home-dir prj-current-name "/" FILE))
        (rename-buffer NAME)
        )
    (message (concat NAME " already exist"))
    )
  )

;; (defun OpenBufferAbsolute(file name)
;;   "Opens a buffer FILE and renames it to NAME.
;; Checks if buffer already exists by its name.
;; The file appears at 'org_projects_home_dir/project-name/file'"
;;   (if (not (buffer-exists name))
;;       (progn
;;         (find-file file)
;;         (rename-buffer name)
;;         )
;;     (message (concat name " already exist"))
;;     )
;;   )

(defun prj-OpenBufferDirex(FILE NAME)
  "Opens a buffer FILE and renames it to NAME.
Checks if buffer already exists by its name.
The file appears at 'org_projects_home_dir/project-name/file'"
  (if (not (buffer-exists NAME))
      (progn
        (direx:find-directory (concat org-projects-home-dir project-name FILE))
        (rename-buffer NAME)
        )
    (message (concat NAME " already exist"))
    )
  )

(defun create-new-eshell(NUMBER NAME &optional COMMAND1 COMMAND2)
  (interactive)
  (if (not (buffer-exists NAME))
      (progn
        (eshell NUMBER)
        (rename-buffer NAME)
        (insert_command COMMAND1)
        (insert_command COMMAND2)
        )
(message (concat NAME " already exist"))
  )
)

(provide 'csb-project-management)
;;; csb-project-management.el ends here
