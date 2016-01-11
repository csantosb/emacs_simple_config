;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains defaults for variables as well as global keystrokes

(stante-after org
              (setq org-ellipsis " ⤵")  ;; http://bit.ly/1MuDoMT
              (setq org-use-speed-commands t)
              (setq org-imenu-depth 4)
              (if running-os-is-linux
                  (setq org-directory "~/OrgAgenda/")
                (setq org-directory "C:/csantos/Documents/"))
              (if running-os-is-linux
                  (setq org-notes-directory (concat user-documents-directory "OrgNotes/"))
                (setq org-notes-directory (concat user-documents-directory "OrgNotes/")))
              (setq-default org-log-done 'note)
              (setq-default org-startup-folded t)
              (setq org-src-fontify-natively t)
              (setq-default org-startup-indented t)
              (setq org-confirm-shell-link-function nil)
              (setq-default org-insert-mode-line-in-empty-file t)
              (setq-default org-hide-leading-stars t)
              (setq-default org-return-follows-link t))

;; (setq-default org-completion-use-iswitchb nil)
;; (setq-default org-completion-use-ido nil)
;; (global-set-key (kbd "C-c a") 'org-agenda)
;; (setq org-log-done t)
;; (setq org-todo-keywords
;;       '((sequence "TODO" "INPROGRESS" "DONE")))
;; (setq org-todo-keyword-faces
;;       '(("INPROGRESS" . (:foreground "blue" :weight bold))))
;; (setq org-agenda-files (list "~/Dropbox/org/personal.org"))

(stante-after org-src
  (defun csb/org-edit-src-exit()(interactive)(org-edit-src-exit)(csb/toggle-fullscreen))
  (define-key org-src-mode-map (kbd "C-c '") #'csb/org-edit-src-exit))

;; http://stackoverflow.com/questions/2736087/eval-after-load-vs-mode-hook
;; executes ONCE the very first time org-mode loads on memory
(stante-after org
  ;; (bind-key "C-TAB" 'org-cycle org-mode-map)
  ;; (bind-key "C-c v" 'org-show-todo-tree org-mode-map)
  ;; (bind-key "C-c C-r" 'org-refile org-mode-map)
  ;; (bind-key "C-c R" 'org-reveal org-mode-map)

  (defun csb/org-edit-special()(interactive)(org-edit-special)(csb/toggle-fullscreen))
  (define-key org-mode-map (kbd "C-c '") #'csb/org-edit-special)

  (define-key org-mode-map (kbd "C-x c H") 'helm-org-headlines)
  (define-key org-mode-map (kbd "C-'") nil)

  (define-key org-mode-map (kbd "C-c i t") 'org-toggle-inline-images)
  (define-key org-mode-map (kbd "C-c i d") 'org-remove-inline-images)
  (define-key org-mode-map (kbd "C-c i I") 'org-display-inline-images)
  (define-key org-mode-map (kbd "C-c i r") 'org-redisplay-inline-images)
  (define-key org-mode-map (kbd "C-j") 'org-insert-heading)
  ;; (define-key org-mode-map (kbd "ESC-TAB") 'org-shifttab)
  ;; emacs under urxvt without tmux works fine: this way i can get the correct functions
  ;; looking at the error message i get the key pressed
  ;; REFACTOR: have a look at here http://unix.stackexchange.com/questions/24414/shift-arrow-not-working-in-emacs-within-tmux?rq=1
  (define-key org-mode-map "\M-[1;3A" 'org-metaup) ;; Alt-up
  (define-key org-mode-map "\M-[1;3B" 'org-metadown) ;; Alt-down
  (define-key org-mode-map "\M-[1;3D" 'org-metaleft) ;; Atl-left
  (define-key org-mode-map "\M-[1;3C" 'org-metaright) ;; Alt-right

  (define-key org-mode-map "\M-[1;5A" 'org-backward-paragraph) ;; C- up
  (define-key org-mode-map "\M-[1;5B" 'org-forward-paragraph) ;; C-down
  ;; (define-key org-mode-map "\M-[1;5C" 'org-) ;; C-right
  ;; (define-key org-mode-map "\M-[1;5D" 'org-) ;; C-left

  (defun find-org-config-file-other ()
    "Edit the org-mode config file, in another window."
    (interactive)
    (find-file-other-window (concat user-emacs-directory "elisp/my_orgmode.org")))

  (defun find-org-config-file ()
    "Edit the org-mode config file."
    (interactive)
    (find-file (concat user-emacs-directory "elisp/my_orgmode.org")))
  )

(setq org-agenda-files (list
                        (concat org-directory "agenda_apc.org")
                        (concat org-directory "agenda_perso.org")
                        (concat org-directory "agenda_reyes.org")
                        (concat org-directory "agenda_compton.org")
                        (concat org-directory "agenda_ebex.org")
                        (concat org-directory "agenda_ciemat.org")
                        (concat org-directory "agenda_lisa.org")
                        (concat org-directory "agenda_athena.org")))
(setq org-agenda-span 'month)
(setq org-agenda-start-on-weekday 1)
(setq org-agenda-start-day nil)
(global-set-key (kbd "C-c a") 'org-agenda)

;; org-caldav variables
(setq org-caldav-url "https://csantosb.ddns.net:444/remote.php/caldav/calendars/")
;; (setq org-caldav-calendar-id "csantosb/perso")
(setq org-icalendar-timezone "Europe/Madrid")
;; (setq org-caldav-files '(list (concat org-directory "Meeting.org")))
;; (setq org-caldav-files org-agenda-files)
;; (setq org-caldav-inbox (concat org-directory "agenda_inbox.org"))
(setq org-caldav-backup-file (concat org-directory "org-caldav-backup.org"))
(setq org-caldav-calendars
      '((:calendar-id "csantosb/perso"  :files ("~/Documents/OrgAgenda/agenda_perso.org")
                      :inbox "~/Documents/OrgAgenda/agenda_perso_inbox_caldav.org" :select-tags nil)
        (:calendar-id "csantosb/ciemat" :files ("~/Documents/OrgAgenda/agenda_ciemat.org")
                      :inbox "~/Documents/OrgAgenda/agenda_ciemat_inbox_caldav.org" :select-tags nil)))
(setq org-caldav-sync-changes-to-org 'all)
(setq org-caldav-save-directory org-directory)
(setq org-caldav-delete-org-entries 'ask)
;; org-icalendar variables
(setq org-icalendar-include-todo 'all)
(setq org-icalendar-include-body t)
(setq org-icalendar-store-UID t)
(setq org-icalendar-with-timestamps t)
(setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due))
(setq org-icalendar-use-scheduled '(event-if-todo event-if-not-todo todo-start))

(defun my-org-goto-isearch-off (&optional alternative-interface)
  (interactive "P")

  (let ((org-goto-auto-isearch nil)
        (org-goto-interface 'outline-path-completion))
    (org-goto alternative-interface)))

(defun my-org-goto-isearch-on (&optional alternative-interface)
  (interactive "P")
  (let ((org-goto-auto-isearch t)
        (org-goto-interface 'outline-path-completion))
    (org-goto alternative-interface)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-S-j") 'my-org-goto-isearch-off)
  (define-key org-mode-map (kbd "C-c C-j") 'my-org-goto-isearch-on))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c M-t")
    (lambda()(interactive)(org-toc-show)(csb/toggle-fullscreen))))

(with-eval-after-load 'org-toc

  ;; Navigation
  (define-key org-toc-mode-map "k" 'org-toc-previous)
  (define-key org-toc-mode-map "j" 'org-toc-next)
  (define-key org-toc-mode-map "p" 'org-toc-previous)
  (define-key org-toc-mode-map "n" 'org-toc-next)
  (define-key org-toc-mode-map "f" 'org-toc-forward)
  (define-key org-toc-mode-map "b" 'org-toc-back)

  ;; Tree
  (define-key org-toc-mode-map "1" (lambda() (interactive) (org-toc-show 1 (point))))
  (define-key org-toc-mode-map "2" (lambda() (interactive) (org-toc-show 2 (point))))
  (define-key org-toc-mode-map "3" (lambda() (interactive) (org-toc-show 3 (point))))
  (define-key org-toc-mode-map "4" (lambda() (interactive) (org-toc-show 4 (point))))
  (define-key org-toc-mode-map "a" (lambda() (interactive) (org-toc-show 5 (point))))
  (define-key org-toc-mode-map (kbd "TAB") 'org-toc-cycle-subtree)
  ;; global cycling in the base buffer
  (define-key org-toc-mode-map (kbd "C-S-<iso-lefttab>") 'org-toc-cycle-base-buffer)
  ;; subtree cycling in the base buffer
  (define-key org-toc-mode-map [(control tab)] (lambda() (interactive) (org-toc-goto nil t)))

  ;; Follow
  (define-key org-toc-mode-map "F" 'org-toc-follow-mode)
  ;; compatibility with default in grep and occur modes
  (define-key org-toc-mode-map "C-c C-f" 'org-toc-follow-mode)
  ;; go to the location and ...
  (define-key org-toc-mode-map "l" 'org-toc-goto)       ;; let point in toc buffer
  (define-key org-toc-mode-map "h" 'org-toc-jump)       ;; move point to the original buffer
  (define-key org-toc-mode-map (kbd "<enter>")          ;; delete other windows
    (lambda() (interactive) (org-toc-jump nil)))
  (define-key org-toc-mode-map (kbd "SPC")              ;; go to the location and delete other windows
    (lambda() (interactive) (org-toc-jump t)))

  ;; Quit
  (define-key org-toc-mode-map "Q" 'org-toc-quit)

  ;; Help
  (define-key org-toc-mode-map "?" 'org-toc-help)

  ;; Nil - remove defaults
  (define-key org-toc-mode-map (kbd " ") nil)
  (define-key org-toc-mode-map (kbd "<tab>") nil)
  (define-key org-toc-mode-map (kbd "<enter>") nil)
  (define-key org-toc-mode-map "x" nil)
  (define-key org-toc-mode-map [(left)] nil)
  (define-key org-toc-mode-map [(right)] nil)
  (define-key org-toc-mode-map [(up)] nil)
  (define-key org-toc-mode-map [(down)] nil))





(require 'org-crypt)
(with-eval-after-load 'org-crypt
  (add-to-list 'org-tags-exclude-from-inheritance "encrypt")
  (setq org-crypt-tag-matcher '"encrypt")
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key "F1B4CAD1F94EE99A")
  (setq epa-file-select-keys nil)
  (setq epa-file-encrypt-to "F1B4CAD1F94EE99A")
  (org-crypt-use-before-save-magic)
  (define-key org-mode-map (kbd "C-c M-d") #'org-decrypt-entry))

;;; * Browse Org Card
;; browse the orgcard by helm
;; (require 'helm-orgcard)

(with-eval-after-load 'org-contacts
  (setq org-contacts-files (list "~/.mutt/contacts.org")))

(require 'org-man)

(require 'org-page)
(setq op/repository-directory (concat user-projects-directory "perso/csantosb.github.io"))
(setq op/site-domain "https://csantosb.github.io/")
(setq op/repository-org-branch "source")
(setq op/repository-html-branch "master")
(setq op/site-preview-directory "/tmp/.op-tmp/")
(setq op/theme 'phaer) ;; mdo, kd_mdo, phaer
(setq op/site-main-title "@csantosb")
(setq op/site-sub-title "")
(setq op/personal-github-link "https://github.com/csantosb")
(setq op/highlight-render 'js)
;; (setq op/personal-disqus-shortname "csantosb@inventati.org")
;; (setq op/rss-template)
;; the configuration below are optional
;;(op/verify-configuration)
(setq op/category-config-alist
      '(("blog" ;; this is the default configuration
         :show-meta t
         :show-comment t
         :uri-generator op/generate-uri
         :uri-template "/blog/%y/%m/%d/%t/"
         :sort-by :date ;; how to sort the posts
         :category-index t) ;; generate category index or not
        ("index"
         :show-meta nil
         :show-comment nil
         :uri-generator op/generate-uri
         :uri-template "/"
         :sort-by :date
         :category-index nil)
        ("about"
         :show-meta nil
         :show-comment nil
         :uri-generator op/generate-uri
         :uri-template "/about/"
         :sort-by :date
         :category-index nil)))

;; (require 'org2blog-autoloads)
(with-eval-after-load 'org2blog
  (setq org2blog/wp-use-sourcecode-shortcode nil)
  ;; org2blog/wp-sourcecode-default-params
  (setq org2blog/wp-sourcecode-langs
        '("actionscript3" "bash" "coldfusion" "cpp" "csharp" "css" "delphi"
          "erlang" "fsharp" "diff" "groovy" "javascript" "java" "javafx" "matlab"
          "objc" "perl" "php" "text" "powershell" "python" "ruby" "scala" "sql"
          "vb" "xml" "sh" "emacs-lisp" "lisp" "lua"))

  (setq org2blog/wp-default-title "My New Title")

  (defun csb/org2blog/wp-login()
    "Custom login function to my blog. Overloads org2blog/wp-login"
    (interactive)
    (require 'org2blog)
    (cond ((and (boundp 'org2blog/wp-logged-in) org2blog/wp-logged-in)
           (message "Already logged in."))
          ((file-exists-p "~/.gnupg/pubring.gpg")
           (let* ()
             (setq noblogs_password (eval-expression (car (split-string (shell-command-to-string "pass www/noblogs") "\n"))))
             (setq org2blog/wp-blog-alist
                   '(("noblogs"
                      :url "http://csantosb.noblogs.org/xmlrpc.php"
                      :username "csantosb"
                      :default-title "Ciao mondo"
                      :default-categories ("org2blog" "emacs")
                      :password noblogs_password
                      :confirm t
                      :show 'ask ;; show, nil
                      :keep-new-lines nil
                      :wp-latex t
                      :tags-as-categories nil)))
             (org2blog/wp-login)))
          ((t)
           (message "NO GPG KEY FOUND, SB OPEN ?")))))

(setq org-publish-project-alist
           '(("PublishTest"
              :base-directory "~/Projects/PublishTest/"
              :publishing-directory "/tmp/public_html"
              :publishing-function org-html-publish-to-html
              :makeindex nil
              :section-numbers nil
              :with-toc nil
              :html-head "<link rel=\"stylesheet\"
                         href=\"../other/mystyle.css\"
                         type=\"text/css\"/>")))

;;; * pdf support
(delete '("\\.pdf\\'" . default) org-file-apps)
;; (add-to-list 'org-file-apps '("\\.pdf::\\([0-9]+\\)\\'" . "evince \"%s\" -p %1"))
(add-to-list 'org-file-apps '("\\.pdf::\\([0-9]+\\)\\'" . "zathura \"%s\" -P %1"))

;; To open a link with a program other than the default(e.g. modify a pdf rather than read it):
(defun my-org-open-at-point()
  (interactive)
  (let ((org-file-apps '(("\\.pdf\\'" . "xournal %s"))))
    (org-open-at-point)))

(setq org-src-window-setup 'current-window)

(with-eval-after-load 'org

  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_9.jar")

    ;;; * Org Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (matlab . t)
     (python . t)
     (octave . t)
     (R . t)
     (ditaa . t)
     (org . t)))
  ;; (require 'ob)

  (setq org-babel-default-header-args:matlab '((:session . "")
                                               (:results . "replace")
                                               (:exports . "code")
                                               (:cache . "no")
                                               (:noweb . "no")
                                               (:hlines . "no")
                                               (:tangle . "no")))

  (setq org-babel-default-header-args '((:session . "")
                                        (:results . "replace")
                                        (:exports . "code")
                                        (:cache . "no")
                                        (:noweb . "no")
                                        (:hlines . "no")
                                        (:tangle . "no")))

  ;; (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

  ;; (defvar org-babel-default-header-args:clojure
  ;;   '((:results . "silent") (:tangle . "yes")))

  ;; (defun org-babel-execute:clojure (body params)
  ;;   (lisp-eval-string body)
  ;;   "Done!")

  ;; (provide 'ob-clojure)

  ;; (setq org-src-fontify-natively t)
  ;; (setq org-confirm-babel-evaluate nil)
  (setq org-confirm-babel-evaluate  nil))

(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode 1)))
(stante-after org-bullets
              (setq org-bullets-bullet-list
                    '("➯" "➯" "➯" "➯" "➯" "➯" "➯")))

(setq org-todo-keywords '((sequence "TODO" "WEEK" "MONTH" "YEAR" "ONEDAY" "|" "DONE")))

;; '((sequence "TODO(t)" "|" "DONE(d)")
;;                 (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
;;                 (sequence "|" "CANCELED(c)"))
;; (setq org-todo-keywords
;;  '((sequence
;;     "TODO(t)"  ; next action
;;     "TOBLOG(b)"  ; next action
;;     "STARTED(s)"
;;     "WAITING(w@/!)"
;;     "POSTPONED(p)" "SOMEDAY(s@/!)" "|" "DONE(x!)" "CANCELLED(c@)")
;;    (sequence "TODELEGATE(-)" "DELEGATED(d)" "COMPLETE(x)")))
;; (setq org-fontify-done-headline t)
;; (custom-set-faces
;;  '(org-done ((t (:foreground "PaleGreen"
;;                  :weight normal
;;                  :strike-through t))))
;;  '(org-headline-done
;;             ((((class color) (min-colors 16) (background dark))
;;                (:foreground "LightSalmon" :strike-through t)))))

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

(defun my-org-open-at-point-global-other-window()
  (interactive)
  (if (eq (length (window-list)) 1)
      (progn
        (split-window-right)
        (other-window 1)
        (balance-windows)))
  (setq-local org-link-frame-setup
              '((vm . vm-visit-folder-other-frame)
                (vm-imap . vm-visit-imap-folder-other-frame)
                (gnus . org-gnus-no-new-news)
                (file . find-file-other-window)
                (wl . wl-other-frame)))
  (org-open-at-point-global))

(defun my-org-open-at-point-global()
  (interactive)
  (setq-local org-link-frame-setup
              '((vm . vm-visit-folder-other-frame)
                (vm-imap . vm-visit-imap-folder-other-frame)
                (gnus . org-gnus-no-new-news)
                (file . find-file)
                (wl . wl-other-frame)))
  (org-open-at-point-global))
  ;; (delete-other-windows)

(defun my-org-open-at-point-other-window()
  (interactive)
  (if (eq (length (window-list)) 1)
      (progn
        (split-window-right)
        (other-window 1)
        (balance-windows)))
  (setq-local org-link-frame-setup
              '((vm . vm-visit-folder-other-frame)
                (vm-imap . vm-visit-imap-folder-other-frame)
                (gnus . org-gnus-no-new-news)
                (file . find-file-other-window)
                (wl . wl-other-frame)))
  (org-open-at-point))

(defun my-org-open-at-point()
  (interactive)
  (setq-local org-link-frame-setup
              '((vm . vm-visit-folder-other-frame)
                (vm-imap . vm-visit-imap-folder-other-frame)
                (gnus . org-gnus-no-new-news)
                (file . find-file)
                (wl . wl-other-frame)))
  (org-open-at-point))

(global-set-key "\C-xL" 'org-store-link)
(global-set-key "\C-x\C-l" 'org-insert-link-global)  ;; shadowed by 'org-insert-link in orgmode
(global-set-key (kbd "C-x o") 'my-org-open-at-point-global-other-window)  ;; shadowed by 'org-open-at-point in orgmode
(global-set-key (kbd "C-x O") 'my-org-open-at-point-global)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-x O") 'my-org-open-at-point)
  (define-key org-mode-map (kbd "C-x o") 'my-org-open-at-point-other-window)
  (define-key org-mode-map (kbd "C-c C-o") nil)
  (define-key org-mode-map (kbd "C-x C-l") 'org-insert-link))

;;; * Org iswitchdb
(global-set-key "\C-cb" 'org-iswitchb)

(stante-after org

              (defun csb/org-attach()
                "The dispatcher for attachment commands, customized (see org-attach)
Shows a list of commands and prompts for another key to execute a command.
i ) sets the attach-directory variable based on the buffer-file-name
ii) sets symbolic link as the preferred attach method
ii) calls the original function org-attach"
                (interactive)
                ;; i) set the directory where attachments are stored.
                ;;    if this is a relative path, it will be interpreted relative to the directory where the Org file lives.
                (if (buffer-file-name) ;; if not nil
                    ;; for the rest of buffers, create a hidden folder containing its buffer-file-name
                    (setq org-attach-directory
                          (concat (file-name-directory (buffer-file-name)) "." (file-name-nondirectory (buffer-file-name))
                                  ".attachments/"))
                  ;; capture buffer-name is called CAPTURE-XXX.org and its buffer-file-name is nil : use
                  ;; (substring (buffer-name) 8) instead to get the name
                  (setq org-attach-directory
                        (concat (file-name-directory org-default-notes-file) "." (substring (buffer-name) 8) ;;
                                ".attachments/"))
                  )
                ;; (message
                ;;  (concat (file-name-directory (buffer-file-name)) "." (file-name-nondirectory(buffer-file-name))
                ;;          ".attachments/"))
                ;; ii)
                (setq-local org-attach-method 'lns)
                ;; (setq-default org-attach-method 'lns)
                ;; iii)
                (org-attach))

              (define-key org-mode-map (kbd "C-c C-a") 'csb/org-attach))

(require 'org-protocol)

(org-require 'csb-orgmode-capture csb/compile-org-require)

;; lib to path
(add-to-list 'load-path "~/.mutt/org-mutt")

;; ensure that emacsclient will show just the note to be edited when invoked
;; from Mutt, and that it will shut down emacsclient once finished;
;; fallback to legacy behavior when not invoked via org-protocol.
(add-hook 'org-capture-mode-hook 'delete-other-windows)
(setq my-org-protocol-flag nil)
(defadvice org-capture-finalize (after delete-frame-at-end activate)
  "Delete frame at remember finalization"
  (progn (if my-org-protocol-flag (delete-frame))
         (setq my-org-protocol-flag nil)))
(defadvice org-capture-kill (after delete-frame-at-end activate)
  "Delete frame at remember abort"
  (progn (if my-org-protocol-flag (delete-frame))
         (setq my-org-protocol-flag nil)))
(defadvice org-protocol-capture (before set-org-protocol-flag activate)
  (setq my-org-protocol-flag t))

(defun open-mail-in-mutt (message)
  "Open a mail message in Mutt, using an external terminal.
Message can be specified either by a path pointing inside a
Maildir, or by Message-ID."
  (interactive "MPath or Message-ID: ")
  (message (format "urxvt -e %s %s" (substitute-in-file-name "~/.mutt/org-mutt/mutt-open") message))
  (shell-command
   (format "urxvt -e %s %s" (substitute-in-file-name "~/.mutt/org-mutt/mutt-open") message)))
;; (start-process "toto" nil "urxvt" "-e" "~/mutt-open.sh 20140102221559.GA8546@laptop.home.arch")
;; (start-process "page-me" nil "urxvt" "-e" "dolphin")
;; (format "%s %s" (substitute-in-file-name "~/.mutt/org-mutt/mutt-open") message)

;; add support for "mutt:ID" links
(org-add-link-type "mutt" 'open-mail-in-mutt)

;; If non-nil then add a link as a second level to the actual location in the file
(setq-default org-annotate-file-add-search t)
;; non-nil means always expand the full tree when you visit `org-annotate-file-storage-file'.
(setq-default org-annotate-file-always-open t)

;;; ** Custom Functions
(defun get-annotate-file(buf)
  "sets the org-annotate-file-storage-file variable"
  (if (string= "" buf)
      (progn
        (setq org-annotate-file-storage-file
              ;; keeps extension and adds custom
              (expand-file-name (concat (file-name-directory (buffer-file-name)) "." (file-name-nondirectory (buffer-file-name)) ".annotate.org")))
        (message org-annotate-file-storage-file))
    (setq org-annotate-file-storage-file
          (concat (file-name-directory buf) "." (file-name-nondirectory buf) ".annotate.org"))))

(defun my_org_annotate_file(buf)
  "Put a section for the current file into your annotation files, customized (org-annotate-file)
i ) sets the org-annotate-file-storage-file variable based on the buffer-file-name
ii) calls the original function org-annotate-file"
  (interactive "sEnter file name ... : ")
  (get-annotate-file buf)
  (split-window-right)
  (other-window 1)
  (balance-windows)
  (message org-annotate-file-storage-file)
  (org-annotate-file))

(define-key org-mode-map (kbd "C-c M-a") #'my_org_annotate_file)

;; (defun my-org-annotate-file(buf)
;; "The idea here is to implement annotating by using the
;; org-capture features; problem: how to refer to an old annotation ?"
;;   (interactive "sEnter file name ... : ")
;;   ;;
;;   (get-annotate-file buf)
;;   ;; i) add new template to the list of capture templates
;;   (add-to-list 'org-capture-templates
;;                             '("J" "Todo tasks"
;;                               entry
;;                               (file+function org-annotate-file-storage-file (defun org-annotate-file-show-section (&optional buffer))
;;                               "* %a\n%?")
;;             )
;;   ;; ii) capture
;;   (org-capture nil '"J")
;;   ;; iii) remove template
;;   (setq org-capture-templates (remove
;;                             '("J" "Todo tasks"
;;                               entry
;;                               (file org-annotate-file-storage-file)
;;                               "* %a\n%?")
;;                             org-capture-templates))
;;   )
;; (global-set-key (kbd "C-c M-a") 'my-org-annotate-file)

(defun RunOrgConfigFile()
  "For a given filename.org file, checks if there exist a .filename.el file at the same location of current org file, sharing the same file name without
    extension, and load it in memory.
    If file exists, check existence of function called 'org-file-default-config' and run it"
  (interactive)
  (if (buffer-file-name)
      (let  ((filename (concat (file-name-directory (buffer-file-name)) "." (file-name-base (buffer-file-name)) ".el")))
        (cond ((file-exists-p filename)
               (load-file filename)
               (if (fboundp 'org-file-default-config)
                   (progn
                     (org-file-default-config)
                     (message (concat "Loaded org config function in " filename))
                     )
                 (message "No config function found in org config file.")
                 ))
              (t
               ;;(message "No org config file %s found." (file-name-nondirectory filename))
               )))))

(defun OpenOrgConfigFile()
  "For a given filename.org file, checks if there exist a .filename.el file at the same location of current org file, sharing the same file name without
    extension, and load it in memory.
    If file exists, open it."
  (interactive)
  (if (buffer-file-name)
      (let  ((filename (concat (file-name-directory (buffer-file-name)) "." (file-name-base (buffer-file-name)) ".el")))
        (cond ((file-exists-p filename)
               (helm-find-files-1 filename))
              (t (message "No org config file %s found." (file-name-nondirectory filename)))))))

;; (with-eval-after-load 'org (define-key org-mode-map (kbd "C-c g c") 'OpenOrgConfigFile))

(defun org-readonly-mark ()
  (interactive)
  (org-map-entries
   (lambda ()
     (let* ((element (org-element-at-point))
            (begin (org-element-property :begin element))
            (end (org-element-property :end element)))
       (add-text-properties begin (- end 1) '(read-only t))))
   "read_only")
 (message "Made readonly!"))

(defun org-readonly-remove ()
  (interactive)
  (org-map-entries
   (lambda ()
     (let* ((element (org-element-at-point))
            (begin (org-element-property :begin element))
            (end (org-element-property :end element))
            (inhibit-read-only t))
         (remove-text-properties begin (- end 1) '(read-only t))))
     "read_only"))

(add-hook 'org-mode-hook (lambda ()
                           ;; (setq buffer-face-mode-face '(:family "Symbola" :height 140))
                           ;; (buffer-face-mode nil)
                           ;; (org-readonly-mark)

                           (csb/mode-line-off)
                           (redraw-display)
                           (org-bullets-mode 1)
                           ;; (conditionally-turn-on-pandoc)
                           (font-lock-comment-annotations)
                           (font-lock-doc-annotations)
                           (RunOrgConfigFile)
                           ;; (variable-pitch-mode 1)
                           (flyspell-mode 1)
                           (imenu-add-to-menubar "Imenu")
                           ;; (color-theme-buffer-local 'color-theme-solarized-light (current-buffer))
                           ;; (add-to-list 'imenu-generic-expression '(nil "^ *%% *\\(\\*.*\\)" 1))
                           ;; (setq-mode-local org-mode imenu-generic-expression '((nil "^\\(?:[*]+\\).*$" 0)))
                           ;; (setq-local imenu-generic-expression '((nil " " 0)))
                           ))

  (add-hook 'org-beamer-mode-hook (lambda ()
                                    (define-key org-beamer-mode-map (kbd "C-c C-b") nil)
                                    (define-key org-beamer-mode-map (kbd "C-c b") 'org-beamer-select-environment)))


  ;; (global-set-key "<f9> <f9>" 'org-agenda-list)
  ;; (global-set-key "<f9> <f8>" (lambda () (interactive) (org-capture nil "r")))
  ;; (eval-after-load 'org
  ;;   '(progn
  ;;      (global-set-key "C-TAB" 'org-cycle org-mode-map)
  ;;      (global-set-key "C-c v" 'org-show-todo-tree org-mode-map)
  ;;      (global-set-key "C-c C-r" 'org-refile org-mode-map)
  ;;      (global-set-key "C-c R" 'org-reveal org-mode-map)))

(defun endless/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
(add-hook 'org-mode-hook #'endless/org-ispell)

(provide 'csb-orgmode)
;;; csb-orgmode.el ends here
