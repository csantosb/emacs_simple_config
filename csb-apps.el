;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(define-key launcher-map "c" #'calc)
(define-key launcher-map "F" #'find-dired)
(define-key launcher-map "h" #'man) ; Help
;; (define-key launcher-map "i" #'package-install-from-buffer)
;; (define-key launcher-map "n" #'nethack)
;; (define-key launcher-map "i" #'my-irc)
(define-key launcher-map "we" #'csb-elfeed)

(defun my-twit()
  "Launch twit."
  (interactive)
  (cond( (file-exists-p "~/sb")
         ;; change perspective
         (persp-switch "twitter")
         ;;
         (let* ((bitlbee-server (password-store-get "www/bitlbee" "server:"))
                (bitlbee-buffer (concat "&bitlbee@" bitlbee-server))
                (bitlbee-server-buffer (concat "*" bitlbee-server "*"))
                (bitlbee-twitter-buffer (concat "#twitter_csantosb@" bitlbee-server)))
           ;; if already running, not launch it again
           (unless (and (get-buffer bitlbee-server-buffer) (get-buffer bitlbee-buffer))
             (let* ((bitlbee-user (password-store-get "www/bitlbee" "user:"))
                    (bitlbee-password (password-store-get "www/bitlbee"))
                    (bitlbee-key (password-store-get "www/bitlbee" "key:"))
                    (bitlbee-port (password-store-get "www/bitlbee" "port:"))
                    ;; server config
                    ;; ref: https://stackoverflow.com/questions/1664202/emacs-lisp-evaluate-variable-in-alist
                    (rcirc-server-freenode `(,bitlbee-server
                                             :nick ,bitlbee-user
                                             :port ,bitlbee-port
                                             :user-name ,bitlbee-user
                                             :full-name ,bitlbee-user
                                             :password ,bitlbee-password))
                    (rcirc-server-alist (list rcirc-server-freenode))
                    ;; credentials
                    (rcirc-authinfo-freenode (list bitlbee-server
                                                   'bitlbee
                                                   bitlbee-user
                                                   bitlbee-key))
                    (rcirc-authinfo (list rcirc-authinfo-freenode)))
               (irc nil)
               (sit-for 5)))
           ;; start twittering if necessary
           (unless (get-buffer ":home")
             (twit)
             (sit-for 2))
           ;; once launched, setup windows
           (persp-switch "twitter")
           ;; add buffers to this perspective
           (persp-set-buffer bitlbee-buffer)
           (persp-set-buffer bitlbee-server-buffer)
           (persp-set-buffer ":home")
           (when (get-buffer bitlbee-twitter-buffer)
             (persp-set-buffer bitlbee-twitter-buffer)
             (split-window-multiple-ways 1 2)
             (set-window-buffer-in-frame 0 1 (get-buffer bitlbee-twitter-buffer))
             (set-window-buffer-in-frame 0 0 (get-buffer ":home"))
             (enlarge-window 20)
             (end-of-buffer)
             (recenter 80))
           (select-window-2)
           (csb/mode-line-off)
           (select-window-1)
           (when (frame-parameter nil 'fullscreen)
             (toggle-frame-fullscreen))))

       (t (message "NO GPG KEY FOUND, SB OPEN??"))))

(with-eval-after-load 'twittering-mode

  (defun find-twittering-config-file-other ()
    "Edit the twittering-mode config file, in another window."
    (interactive)
    (find-file-other-window (concat org-config-path "csb-global-launchermap.page")))

  (defun find-twittering-config-file ()
    "Edit the twittering-mode config file."
    (interactive)
    (find-file (concat org-config-path "csb-global-launchermap.page"))))

(define-key launcher-map "wt" #'my-twit)

(with-eval-after-load 'twittering-mode
  (define-key twittering-mode-map (kbd "S-SPC") 'twittering-scroll-down))

(with-eval-after-load 'twittering-mode
     (twittering-enable-unread-status-notifier)
     (setq twittering-private-info-file (concat user-emacs-directory ".twittering-mode.gpg"))
     (setq twittering-use-master-password t)
     (setq twittering-tinyurl-service 'bit.ly)
     (setq twittering-url-show-status nil)
     (setq twittering-edit-skeleton 'inherit-any)
     (twittering-icon-mode 1)
     ;; (setq twittering-initial-timeline-spec-string
     ;;       '("(:home+@)"
     ;;         "(:search/twittering mode/+:search/twmode/)"))
     (setq twittering-retweet-format '(nil _ " %u RT @%s: %t"))
     (setq twittering-status-format "%i %S %T ||%R%r") ;; %FILL[  ]{}  %RT{%s}
     ;; (setq twittering-status-format "%FOLD{%RT{%FACE[bold]{RT}}%i%s>>%r @%C{%Y-%m-%d %H:%M:%S} %@{}\n%FOLD[ ]{%T%RT{\nretweeted by %s @%C{%Y-%m-%d %H:%M:%S}}}}")
     (setq twittering-number-of-tweets-on-retrieval 20)
     (setq twittering-display-remaining t)
     (setq twittering-timer-interval 60)
     (setq twittering-use-icon-storage t)
     (setq twittering-convert-fix-size 48)
     (setq twittering-icon-storage-file (concat user-emacs-directory ".twittering-mode-icons.gz"))
     (setq twittering-reverse-mode t))

(add-hook 'twittering-mode-hook (lambda ()
                                  (setq-local show-trailing-whitespace nil)
                                  (rename-modeline "twittering-mode" twittering-mode "twit")
                                  (local-set-key (kbd "C-c g I") 'find-twittering-config-file)
                                  (local-set-key (kbd "C-c g i") 'find-twittering-config-file-other)))

;; (eval-after-load 'readability
;;   '(progn
;;      (define-key readability-map-common (kbd "n") nil)
;;      (define-key readability-map-common (kbd "j") nil)
;;      (define-key readability-map-common (kbd "p") nil)
;;      (define-key readability-map-common (kbd "k") nil)
;;      (define-key readability-map-common (kbd "f") nil)
;;      (define-key readability-map-common (kbd "l") nil)
;;      (define-key readability-map-common (kbd "b") nil)
;;      (define-key readability-map-common (kbd "h") nil)))
(defun readability--open-article ($article-id)
  (w3m-browse-url (message (concat readability-url-base (format "/articles/%s" $article-id)))))
(define-key launcher-map "R" 'readability-get-reading-list)
;; (setq readability-parameters
;;       '(("archive"  . nil)  ;; "0", "1"
;;         ("favorite" . nil)  ;; "0", "1"
;;         ("order"    . nil)  ;; "-date_added", "date_added", "-date_updated", "date_updated"
;;         ("page"     . nil)  ;; "1" ~
;;         ("per_page" . "50") ;; "1" ~ "50"
;;         ("domain"   . nil)  ;; string
;;         ("tags"     . "myhdl,tutorial" )  ;; string
;;         ))

(defun my-pinboard ()
    "Opens pinboard in eww-new-session"
    (interactive)
    ;; (interactive (list
    ;;              (read-string "Enter the reddit (default: emacs): " nil nil "emacs" nil)))
    (persp-switch "pinboard")
    (if (not (get-buffer "eww - Pinboard: bookmarks for csantosb"))
        (progn
          (eww "https://pinboard.in/u:csantosb")
          ;; (sit-for 2)
          ;; (switch-to-buffer "*eww*")
          ;; (rename-buffer (concat "reddit-" reddit))
          (persp-set-buffer "eww - Pinboard: bookmarks for csantosb"))))

(define-key launcher-map "wp" #'my-pinboard)

(with-eval-after-load 'eww

     (defun eww-pinboard-add-current-buffer ()
       (interactive)
       (require 'pinboard-api)
       ;; (url &optional title description toread shared)
       (pinboard-add-interactively (plist-get eww-data :url) (plist-get eww-data :title) nil "yes" "yes"))

     (defun eww-pinboard-get (tag)
       "Only search tags (separate a maximum of 3 TAGS with a '+' eg., emacs+manual)"
       (interactive "sEnter tag ... : ")
       (require 'pinboard-api)
       (setenv "LC_ALL" "C")
       (setenv "LANG" "C")
       (eww-browse-url (shell-command-to-string (concat "sr pin -t=" tag " -u=csantosb")) t)))

;;  (eval-after-load 'w3m
;;    '(progn
;;       (require 'pinboard-api)))
;; ("csantosb:2786ea2660c963b1b161")
  (with-eval-after-load 'pinboard-api
       (if (file-exists-p "~/.gnupg/pubring.gpg")
           (setq pinboard-api-token (eval-expression (shell-command-to-string "pass www/pinboard-api-token")))
         (message "NO GPG KEY FOUND, SB OPEN??"))
       (setq pinboard-tags-cache-file-name "~/Dropbox/emacs.d/.pinboard-tags-cache")
       (setq pinboard-completing-read-function 'helm-comp-read)) ;; C-Ret to abort (see mode-line)

(with-eval-after-load 'eww

  (define-prefix-command 'pinboard-prefix)
  (define-key eww-mode-map (kbd "M-p") 'pinboard-prefix)

  (define-key pinboard-prefix (kbd "a") 'eww-pinboard-add-current-buffer)  ;; add
  (define-key pinboard-prefix (kbd "g") 'eww-pinboard-get)                 ;; get
  (define-key pinboard-prefix (kbd "h")                                    ;; go-home
    (lambda ()(interactive)
      (eww-browse-url "http://pinboard.in/u:csantosb")))
  (define-key pinboard-prefix (kbd "u")                                    ;; unread
    (lambda()(interactive)
      (eww-browse-url "http://pinboard.in/u:csantosb/unread")))
  (define-key pinboard-prefix (kbd "t")                                    ;; tag: query anyone's tags
    (lambda () (interactive)
      (let ((a-tag (read-from-minibuffer "tag?" " ")))
        (browse-url (shell-command-to-string (concat "sr pin " a-tag)))))))

(defun my-reddit (reddit)
  "Opens the REDDIT in eww-new-session"
  (interactive (list
                (read-string "Enter the reddit (default: emacs): " nil nil "emacs" nil)))
  (persp-switch "reddit")
  (when (not (get-buffer (concat "reddit - " reddit)))
    (switch-to-buffer (get-buffer-create "*eww*"))
    (eww-browse-url (format "http://m.reddit.com/r/%s" reddit) t)
    (sit-for 2)
    (rename-buffer (concat "reddit - " reddit))
    (csb/mode-line-off)
    (local-set-key "Q" (lambda()(interactive)(kill-this-buffer)(persp-kill "reddit"))))
  (persp-set-buffer (concat "reddit - " reddit))
  (switch-to-buffer (concat "reddit - " reddit)))

(define-key launcher-map "wr" #'my-reddit)

(defun my-diaspora ()
  (interactive)
  (if (file-exists-p "~/.gnupg/pubring.gpg")
      (progn
        (add-to-list 'load-path (concat user-emacs-directory "libraries/diaspora.el"))
        (add-to-list 'load-path (concat user-emacs-directory "libraries/diaspora.el/libraries"))
        (require 'diaspora)
        (setq diaspora-show-images-by-default t)
        (setq diaspora-pod "diaspora-fr.org")
        (setq diaspora-username "csantosb")
        (if (eq diaspora-password nil)
            (setq diaspora-password (eval-expression (car (split-string (shell-command-to-string "pass www/diaspora") "\n")))))
        (persp-switch "diaspora")
        (if (not (get-buffer "D*"))
            (diaspora)))
    (message "NO GPG KEY FOUND, SB OPEN ?")))

;; (define-key launcher-map "d" #'my-diaspora)

(with-eval-after-load 'sx-question-list
  (setq sx-question-mode-display-buffer-function #'pop-to-buffer)) ;; switch-to-buffer

(with-eval-after-load 'sx-question-list
  (setq sx-question-list--header-line
        '((" "
           ((:propertize "l" face mode-line-buffer-id))
           ":Display")
          (" "
           ((:propertize "k" face mode-line-buffer-id)
            (","
             (:propertize "j" face mode-line-buffer-id))
            (","
             (:propertize "p" face mode-line-buffer-id))
            (","
             (:propertize "n" face mode-line-buffer-id)))
           ":Navigate")
          (" "
           ((:propertize "t" face mode-line-buffer-id))
           "ab")
          (" "
           ((:propertize "a" face mode-line-buffer-id))
           "sk")
          (" "
           ((:propertize "S" face mode-line-buffer-id))
           "earch")
          (" "
           ((:propertize "s" face mode-line-buffer-id))
           "witch-to")
          (" "
           ((:propertize "v" face mode-line-buffer-id))
           "isit")
          (" "
           ((:propertize "h" face mode-line-buffer-id))
           "ide")
           (" "
           ((:propertize "ql" face mode-line-buffer-id))
           "ist")
          (" "
           ((:propertize "m" face mode-line-buffer-id))
           "ark-read")
           (" "
           ((:propertize "o" face mode-line-buffer-id))
           "rder"))))

(with-eval-after-load 'sx-question-list

  (define-key sx-question-list-mode-map (kbd "t") #'(lambda()
                                                      (interactive)
                                                      (with-initial-minibuffer "sx-tab")))

  (define-key sx-question-list-mode-map (kbd "ql") #'(lambda()
                                                       (interactive)
                                                       (with-initial-minibuffer "sx-question-list")))

  (define-key sx-question-list-mode-map (kbd "k") #'scroll-down-line)
  (define-key sx-question-list-mode-map (kbd "j") #'scroll-up-line)
  (define-key sx-question-list-mode-map (kbd "n") '(lambda ()
                                                     (interactive)
                                                     (sx-question-list-next 1)
                                                     (call-interactively 'sx-display-question)
                                                     (other-window 1)))
  (define-key sx-question-list-mode-map (kbd "p") '(lambda ()
                                                     (interactive)
                                                     (sx-question-list-previous 1)
                                                     (call-interactively 'sx-display-question)
                                                     (other-window 1)))
  (define-key sx-switchto-map "m" #'sx-tab-month)
  (define-key sx-switchto-map "e" #'sx-tab-meta-or-main)
  (define-key sx-switchto-map "o" #'sx-tab-hot)
  (define-key sx-switchto-map "a" #'sx-tab-all-questions)
  (define-key sx-switchto-map "I" #'sx-inbox-notifications)
  (define-key sx-switchto-map "h" nil) ;; obsolete
  (define-key sx-switchto-map "s" #'sx-question-list-switch-site)

  (define-key sx-question-list-mode-map (kbd "o") #'sx-question-list-order-by)
  (define-key sx-question-list-mode-map (kbd "l") #'sx-display-question))

(add-hook 'sx-question-list-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))

(with-eval-after-load 'sx-question-mode
  (define-key sx-question-mode-map (kbd "h") '(lambda()(interactive)(other-window 1)))
  ;; scrolling
  (define-key sx-question-mode-map (kbd "j") 'scroll-up-line)
  (define-key sx-question-mode-map (kbd "k") 'scroll-down-line)

  (defun sx-question-mode-view-next()
    (interactive)
    (other-window 1)
    (sx-question-list-next 1)
    (call-interactively 'sx-display-question))

  (defun sx-question-mode-view-previous()
    (interactive)
    (other-window 1)
    (sx-question-list-previous 1)
    (call-interactively 'sx-display-question))

  (define-key sx-question-mode-map (kbd "f") #'sx-question-mode-view-next)
  (define-key sx-question-mode-map (kbd "b") #'sx-question-mode-view-previous))

(with-eval-after-load 'sx-question-mode
  (setq sx-question-mode--header-line
        '((" "
           ((:propertize "TAB" face mode-line-buffer-id)
           (","
             (:propertize "p" face mode-line-buffer-id))
            (","
             (:propertize "n" face mode-line-buffer-id))
            (","
             (:propertize "f" face mode-line-buffer-id))
            (","
             (:propertize "b" face mode-line-buffer-id)))
           ":Navigate")
          (" "
           ((:propertize "u" face mode-line-buffer-id))
           "p-")
          (""
           ((:propertize "d" face mode-line-buffer-id))
           "ownvote")
          (" "
           ((:propertize "e" face mode-line-buffer-id))
           "dit")
          (" "
           ((:propertize "*" face mode-line-buffer-id))
           ":star")
          (" "
           ((:propertize "K" face mode-line-buffer-id))
           ":Delete")
          (" "
           ((:propertize "s" face mode-line-buffer-id))
           "witch-to")
          (" "
           ((:propertize "o" face mode-line-buffer-id))
           "rder")
          (" "
           ((:propertize "c" face mode-line-buffer-id))
           "omment")
          (" "
           ((:propertize "a" face mode-line-buffer-id))
           "nswer"))))

(add-hook 'sx-question-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))

(stante-after deft
  ;; Handle backspace and delete
  (define-key deft-mode-map (kbd "DEL") 'deft-filter-decrement)
  (define-key deft-mode-map (kbd "M-DEL") 'deft-filter-decrement-word)
  ;; Handle return via completion or opening file
  (define-key deft-mode-map (kbd "RET") 'deft-complete)
  ;; Filtering
  (define-key deft-mode-map (kbd "C-c C-l") 'deft-filter)
  (define-key deft-mode-map (kbd "C-c C-c") 'deft-filter-clear)
  (define-key deft-mode-map (kbd "C-y") 'deft-filter-yank)
  ;; File creation
  (define-key deft-mode-map (kbd "C-c C-n") 'deft-new-file-named)
  (define-key deft-mode-map (kbd "C-c C-m") 'deft-new-file)
  (define-key deft-mode-map (kbd "<C-return>") 'deft-new-file-named)
  ;; File management
  (define-key deft-mode-map (kbd "C-c C-d") 'deft-delete-file)
  (define-key deft-mode-map (kbd "C-c C-r") 'deft-rename-file)
  (define-key deft-mode-map (kbd "C-c C-f") 'deft-find-file)
  (define-key deft-mode-map (kbd "C-c C-a") 'deft-archive-file)
  ;; Settings
  (define-key deft-mode-map (kbd "C-c C-t") 'deft-toggle-incremental-search)
  (define-key deft-mode-map (kbd "C-c C-s") 'deft-toggle-sort-method)
  ;; Miscellaneous
  (define-key deft-mode-map (kbd "C-c C-g") 'deft-refresh)
  (define-key deft-mode-map (kbd "C-c C-q") 'quit-window)
  ;; (define-key deft-mode-map (kbd "C-c C-Q") (lambda()(interactive)(kill-this-buffer)(persp-kill "deft")))
  (define-key deft-mode-map (kbd "C-c C-Q") 'kill-this-buffer)
  ;; Widgets
  (define-key deft-mode-map [down-mouse-1] 'widget-button-click)
  (define-key deft-mode-map [down-mouse-2] 'widget-button-click)
  (define-key deft-mode-map (kbd "<tab>") 'widget-forward)
  (define-key deft-mode-map (kbd "<backtab>") 'widget-backward)
  (define-key deft-mode-map (kbd "<S-tab>") 'widget-backward)
  (define-key deft-mode-map (kbd "C-o") 'deft-open-file-other-window))
;; Global
;; (define-key launcher-map "ww" (lambda()(interactive)(persp-switch "Wiki")(if (not (get-buffer "*Deft*"))(deft))))
;; (global-set-key (kbd "C-x C-g") 'deft-find-file) ;; I prefer helm-org-wiki

(add-hook 'proced-mode-hook (lambda()(interactive)(proced-toggle-auto-update t)))
(define-key launcher-map "P" (lambda()(interactive)(persp-switch "proced")(if (not (get-buffer "*Proced*"))(proced))))
(stante-after proced (setq proced-auto-update-interval 2))

(define-key launcher-map "p"
  (lambda (&optional no-fetch)
    (interactive "p")
    (persp-switch "paradox")
    (if (equal no-fetch '(4))
        (paradox-list-packages no-fetch)
      (if (file-exists-p "~/sb/.password-store/")
          (progn
            (setq paradox-github-token (password-store-get "www/github"
                                                           "paradox-token:"))
            (paradox-list-packages no-fetch))
        (message "NO GPG KEY FOUND, SB OPEN??")))))

(when running-os-is-linux (org-require 'csb-elfeed csb/compile-org-require))

(provide 'csb-apps)

;;; csb-apps.el ends here
