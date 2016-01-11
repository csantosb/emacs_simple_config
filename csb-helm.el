;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode 1)

(stante-after helm

              (setq helm-google-suggest-use-curl-p t
                    helm-mode-fuzzy-match t
                    helm-completion-in-region-fuzzy-match t
                    helm-display-header-line t            ;; Display header-line when non nil.
                    helm-split-window-in-side-p t         ;; Force splitting inside selected window when non nil
                    helm-autoresize-max-height 30
                    helm-autoresize-min-height 4
                    helm-scroll-amount 4                  ;; scroll 4 lines other window using M-<next>/M-<prior>
                    helm-quick-update t                   ;; do not display invisible candidates
                    helm-idle-delay 0.01                  ;; be idle for this many seconds, before updating in delayed sources.
                    helm-input-idle-delay 0.01            ;; be idle for this many seconds, before updating candidate buffer
                    helm-ff-search-library-in-sexp t      ;; search for library in `require' and `declare-function' sexp.

                    helm-split-window-default-side 'other ;; open helm buffer in another window
                    helm-split-window-in-side-p t         ;; open helm buffer inside current window, not occupy whole other window
                    helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                                        '(picture-mode artist-mode))
                    helm-candidate-number-limit 20        ;; limit the number of displayed canidates
                    helm-M-x-requires-pattern 0           ;; show all candidates when set to 0
                    helm-boring-file-regexp-list
                    '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
                    helm-ff-file-name-history-use-recentf t
                    helm-move-to-line-cycle-in-source t   ;; move to end or beginning of source
                    ;; when reaching top or bottom of source.
                    ;; ido-use-virtual-buffers t           ;; Needed in helm-buffers-list
                    helm-semantic-fuzzy-match t
                    helm-imenu-fuzzy-match t
                    helm-recentf-fuzzy-match t
                    helm-buffers-fuzzy-matching t ))      ;; fuzzy matching buffer names when non--nil
;; useful in helm-mini that lists buffers

(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x b")   'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)
;; helm-recent see below

(define-key helm-command-map (kbd "r") 'helm-register)
(define-key helm-command-map (kbd "b") 'helm-resume)
;; (define-key helm-command-map (kbd "C-x c C") 'helm-color)
;; (global-set-key (kbd "C-x c") 'helm-list-emacs-proces)
;; (global-set-key (kbd "C-x c H") 'helm-complex-command-history)

(define-key helm-command-map (kbd "a") nil)  ;; default helm-apropos

(define-key helm-map (kbd "C-b") 'helm-previous-source)
(define-key helm-map (kbd "C-f") 'helm-next-source)
(define-key helm-map (kbd "<tab>") 'helm-select-action)
;; (define-key helm-map (kbd "C-i") 'helm-select-action) ;; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-execute-persistent-action)
;; (define-key helm-map (kbd "C-c ?")  'helm-ff-help)

(defun helm-backspace ()
  "Forward to `backward-delete-char'.
On error (read-only), quit without selecting."
  (interactive)
  (condition-case nil
      (backward-delete-char 1)
    (error
     (helm-keyboard-quit))))
(define-key helm-map (kbd "DEL") 'helm-backspace)

(add-hook 'persp-switch-hook
          #'(lambda()
              (interactive)
              (helm-perso-wiki-mode
               (if (and (symbolp 'helm-perso-wiki-wikis)
                        (member (persp-name persp-curr) helm-perso-wiki-wikis))
                   t
                 -1))))

(add-hook 'projectile-switch-project-hook
          #'(lambda()
              (interactive)
              (helm-perso-wiki-mode
               (if (and (symbolp 'helm-perso-wiki-wikis)
                        (member (persp-name persp-curr) helm-perso-wiki-wikis))
                   t
                 -1))))

(require 'helm-perso-wiki)
(setq helm-perso-wiki-wikis '("wikidata" "wikidata-ciemat" "Iphis" "wikidata-apc"))
(stante-after helm-perso-wiki
              (setq helm-perso-wiki-category-extension "cat"
                    helm-perso-wiki-subcategory-extension "page"
                    helm-perso-wiki-fuzzy-match t
                    helm-perso-wiki-back-tip "❮ Back"
                    helm-perso-wiki-top-tip "⚑ Top"
                    helm-perso-wiki-cat-tip " ❯ " )

              (setq helm-perso-wiki-template
                    "(save-excursion
          (next-line)
          (let ((init (point)))
            (re-search-forward \"----------\" nil t 1)
            (kill-region init (point)))
          (insert \"\\n* Table of Contents\\n\\n\")
          (helm-perso-wiki-construct-toc default-directory \"**\" t t)
          (insert \"\\n----------\"))
  (save-excursion
    (let ((init (point)))
      (re-search-forward \"----------\" nil t 1)
      (align-regexp init (point) \"::\")))" )

              (add-to-list 'auto-mode-alist '("\\.page\\'" . org-mode)))

(stante-after helm-perso-wiki
              (add-hook 'org-mode-hook
                        #'(lambda() (when (and
                                      (buffer-file-name)
                                      (string= (file-name-extension (buffer-file-name))
                                               helm-perso-wiki-subcategory-extension))
                                 (setq-local org-link-abbrev-alist
                                             '(("file" . "%(helm-perso-wiki-file-link-parser)"))))))

              (defun helm-perso-wiki-file-link-parser(arg)
                ;; pages
                (if (not (file-name-extension arg))
                    (format "file:%s%s.page%s" (projectile-project-root)
                            (replace-regexp-in-string "file:/" "" (replace-regexp-in-string "::.*" "" arg))
                            (if (string-match "::" arg)
                                (replace-regexp-in-string ".*::" "::" arg)
                              ""))
                  ;; other documents
                  (format "%s%s" (projectile-project-root)
                          (replace-regexp-in-string "file:/" "" arg))))

              ;; (defun helm-perso-wiki-file-link-parser(arg)
              ;;        (format "%s/%s.page%s" (projectile-project-root)
              ;;                (replace-regexp-in-string "::.*" "" arg)
              ;;                (if (string-match "::" arg)
              ;;                    (replace-regexp-in-string ".*::" "::" arg)
              ;;                  "")))

              ;; makes org-export work
              (defadvice org-export-dispatch (around org-export-dispatch-helm-perso-wiki activate)
                (let ((org-link-abbrev-alist (default-value 'org-link-abbrev-alist)))
                  ad-do-it )))

(define-key helm-command-map (kbd "wa") 'helm-perso-wiki-add-project)

;; (setenv "LC_ALL" "C")
;; (setenv "LANG" "C")
(define-key helm-command-map (kbd "s") 'helm-surfraw)

(defun csb/helm-semantic-or-imenu (arg)
  (interactive "P")
  (cond ((or (string= (persp-name persp-curr) "OrgAgenda")
             (string= (persp-name persp-curr) "calendar"))
         (progn
           (helm-org-agenda-files-headings)
           ;; (mapc 'find-file org-agenda-files)
           ;; (helm-imenu-in-all-buffers)
           ))
        ((string= major-mode "org-mode")
         (helm-org-in-buffer-headings))
        ((string= major-mode "dired-mode")
         (beginning-of-buffer)
         (helm-imenu))
        (t (helm-semantic-or-imenu arg))))

(define-key helm-command-map (kbd "i") 'csb/helm-semantic-or-imenu)

;; (setenv "LC_ALL" "C")
;; (setenv "LANG" "C")
(define-key helm-command-map (kbd "s") 'helm-surfraw)

(define-key helm-command-map (kbd "s") #'swiper) ;; swiper-helm

(helm-descbinds-mode)
(setq helm-descbinds-window-style 'split-window)

;; (autoload 'helm-dictionary "helm-dictionary" "" t)
;; (require 'helm-dictionary)
(with-eval-after-load 'helm-dictionary
  (setq helm-dictionary-database (concat user-emacs-directory "dicts" "/dict-en-es.ding"))
  (add-to-list 'helm-dictionary-online-dicts '("es.wiktionary.org" . "http://es.wiktionary.org/wiki/%s")))
(define-key helm-command-map (kbd "d") 'helm-dictionary)
;; (add-hook 'helm-select-action-hook (lambda() (delete-trailing-whitespace)))
(add-hook 'helm-update-hook (lambda() (delete-trailing-whitespace)))

(when (not (string= system-name "apcpc152"))
  ;; (add-to-list 'load-path "~/Projects/perso/helm-recoll")
  ;; (require 'helm-recoll)
  (setq-default helm-recoll-options '("recoll" "-t" "-b"))
  (helm-recoll-create-source "docs" "~/.recoll/home")
  (helm-recoll-create-source "maildir" "~/.recoll/maildir")
  (define-key helm-command-map (kbd "R") (lambda () (interactive)
                                           (if (file-exists-p "~/apc-home/")
                                               (helm-recoll-create-source "myCore"
                                                                          "~/apc-home/.recoll-myCore")
                                             (when (boundp 'helm-source-recoll-myCore)
                                               (unintern 'helm-source-recoll-myCore obarray)))
                                           (helm-recoll))))

(setq helm-dash-common-docsets '(""))
(define-key helm-command-map (kbd "D") 'helm-dash)
;; (global-set-key (kbd "C-x c d") 'helm-dash-at-point) <- use M-n for this
(defun helm-dash-update-all-docsets ()
  (interactive)
  (setq lista (helm-dash-installed-docsets))
  (message "starting --->")
  (dolist (elt lista)
    (message "********")
    (message elt)
    (setq elt (replace-regexp-in-string " " "_" elt))
    (if (string= elt "Lua")
        (setq elt "Lua_5.2"))
    (message elt)
    (helm-dash-update-docset elt)
    (message "--------")))

(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'helm-recentf) ;; 'recentf-open-files
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(setq recentf-max-menu-items 50)

(defun ps-candidates ()
    "return a list of cons cells (line . pid) for the output of ps"
    (loop for line in
          ;; skip the first line which is a header
          (cdr (split-string
                (shell-command-to-string
                 "ps ax -o ruser,pid,command") "\n"))
          collect
          (cons
           line
           (elt (split-string line) 1))))

  (defun ps-details (pid)
    "give details of PID."
    (message-box "%s" (shell-command-to-string (format "ps ux %s" pid))))

  (defun ps-kill (pid)
    "Message box instead of killing PID."
    (let ((SIG (read-string "Kill with signal: ")))
      (message-box "Killing pid %s with signal %s" pid SIG)))

  (defun ps-hello (pid)
    (message-box "Silly 3rd action for %s" pid))

  (defun ps-bye (pid)
    (message-box "Silly 4th action for %s" pid))

  (defun ps-byebye (pid)
    (message-box "Silly 5th action for %s" pid))

  ;; the source variable for helm
  (setq helm-source-ps '((name . "ps output")
                         ;; these are the entries you can select
                         (candidates . ps-candidates)
                         ;; these are the actions available for the
                         ;; selected entry. each function gets the cdr
                         ;; of the entry selected.
                         (action . (("details" . ps-details)
                                    ("kill" . ps-kill)
                                    ("hello" . ps-hello)
                                    ("bye" . ps-bye)
                                    ("byb-bye" . ps-byebye)))))

(defun helm-ps ()
  "Select a page-file to jump to."
  (interactive)
  (helm :sources '(helm-source-ps)))

  ;; (define-key helm-command-map (kbd "P") 'helm-ps)

(define-key helm-command-map (kbd "T") #'helm-themes)

(define-key helm-command-map (kbd "C-<SPC>") 'helm-all-mark-rings)

(defun csb/lobsters (arg)
  "Launch helm-lobsters hottest.
        With a prefix argument ARG, launch newest instead."
  (interactive "P")
  (require 'helm-lobsters)
  (let ((helm-lobsters-url
         (if (equal arg '(4))
             "https://lobste.rs/hottest.json"
           "https://lobste.rs/newest.json" ))
        (helm-lobsters-persp-name
         (if (equal arg '(4))
             "lobste.rs-hottest"
           "lobste.rs-newest" )))
    (persp-switch helm-lobsters-persp-name)
    (helm-lobsters)
    (csb/mode-line-off)
    (local-set-key "Q" (lambda()
                         (interactive)
                         (kill-this-buffer)
                         (persp-kill (persp-name persp-curr))))))

(defun csb/journal-pirate (arg)
  "Launch helm-journal-pirate hottest.
        With a prefix argument ARG, launch newest instead."
  (interactive "P")
  (require 'helm-lobsters)
  (let ((helm-lobsters-url
         (if (equal arg '(4))
             "https://infos.mytux.fr/hottest.json"
           "https://infos.mytux.fr/newest.json" ))
        (helm-lobsters-persp-name
         (if (equal arg '(4))
             "journal-pirate-hottest"
           "journal-pirate-newest" )))
    (persp-switch helm-lobsters-persp-name)
    (helm-lobsters)
    (csb/mode-line-off)
    (local-set-key "Q" (lambda()
                         (interactive)
                         (kill-this-buffer)
                         (persp-kill (persp-name persp-curr))))))

(define-key launcher-map "wl" #'csb/lobsters)
(define-key launcher-map "wL" #'csb/journal-pirate)

;; (define-key helm-command-map (kbd "g") #'helm-gtags-dwim)

(stante-after helm-gtags
              (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
              (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
              (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
              (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
              (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
              (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))

(setq helm-gtags-prefix-key "\C-cg"
      helm-gtags-suggested-key-mapping t)

(stante-after helm-gtags
              helm-gtags-ignore-case t
              helm-gtags-auto-update t
              helm-gtags-use-input-at-cursor t
              helm-gtags-pulse-at-cursor t )

(define-key helm-command-map (kbd "y") 'helm-yas-complete)

;; (define-key helm-command-map (kbd "y") 'helm-dabbrev)

(require 'helm-flyspell)

(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(provide 'csb-helm)

;;; csb-helm.el ends here
