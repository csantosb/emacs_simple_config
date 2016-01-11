;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(defconst rss-alert-script "/home/csantos/Scripts/rss-alert.sh")

(defun csb-elfeed ()
  (interactive)
  (persp-switch "elfeed")
  (when (not (get-buffer "*elfeed-search*"))
      (progn
        (elfeed)
        (persp-set-buffer "*elfeed-search*"))))

;; (add-to-list 'load-path (concat user-emacs-directory "libraries/"))
;;  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
;;  (setq rmh-elfeed-org-tree-id "elfeed")

;; (load "rmh-elfeed-org.el")
;; Use an advice to load the configuration.
;; (defadvice elfeed (before configure-elfeed activate)
;;  "Load all feed settings before elfeed is started."
;;  (rmh-elfeed-org-configure))

(stante-after elfeed

              (defun elfeed-search-browse-url-other-window()
                (interactive)
                (delete-other-windows)
                (split-window-right)
                (other-window 1)
                (balance-windows)
                (elfeed-search-browse-url))

              (defun elfeed-show-visit-other-window()
                (interactive)
                (delete-other-windows)
                (split-window-right)
                (other-window 1)
                (balance-windows)
                (elfeed-show-visit))

              (defun rss_callback(entry)
                (start-process "page-me" nil rss-alert-script (elfeed-feed-title (elfeed-entry-feed entry))
                               "New RSS Feed" (elfeed-entry-title entry) (elfeed-feed-title (elfeed-entry-feed entry))))

              (defun elfeed-send-pinboard (entry)
                "Send current feed link to pocket by mail."
                ;; (shell-command (concat  "contents=/tmp/borrar; touch $contents; echo '  ' > $contents; echo  '" (elfeed-entry-link entry) "' > $contents; " "mutt -F $HOME/Dropbox/scripts/muttrc_otra_offline -s '" (elfeed-feed-title (elfeed-entry-feed entry)) " : " (elfeed-entry-title entry) "' -- add@getpocket.com < $contents"))
                ;; (shell-command (concat  "contents=/tmp/borrar; touch $contents; echo '  ' > $contents; echo  '" (elfeed-entry-link entry) "' > $contents; " "mutt -F $HOME/Dropbox/scripts/muttrc_otra_offline -s '" (elfeed-feed-title (elfeed-entry-feed entry)) " : " (elfeed-entry-title entry) "' -- csantosb+csantosb@inbox.readability.com < $contents"))
                ;; (elfeed-untag entry 'unread)
                (require 'pinboard-api)
                ;; call signature: (url &optional description tags extended toread shared)
                ;; test: (pinboard-api-add "http://www.google.com" "titulo" "borrar otro unomas" "desc" "yes" "yes")
                (pinboard-api-add (elfeed-entry-link entry) (elfeed-entry-title entry) (concat "elfeed "
                                                                                               "readability " (elfeed-feed-title (elfeed-entry-feed entry))) " " "yes" "yes")
                (message "elfeed - sent %s : %s" (elfeed-entry-title entry) (elfeed-entry-link entry) )
                (elfeed-untag entry 'unread)
                (elfeed-search-update-entry entry)
                (unless (use-region-p) (forward-line)))

              (defun elfeed-send-pocket-interactively ()
                "Send current feed link to pocket by mail."
                (interactive)
                (let ((entries (elfeed-search-selected)))
                  (dolist (entry entries)
                    (shell-command (concat  "contents=/tmp/borrar; touch $contents; echo '  ' > $contents; echo  '" (elfeed-entry-link entry) "' > $contents; " "mutt -F $HOME/Dropbox/scripts/muttrc_otra_offline -s '" (elfeed-feed-title (elfeed-entry-feed entry)) " : " (elfeed-entry-title entry) "' -- add@getpocket.com < $contents"))
                    (elfeed-untag entry 'unread)
                    (elfeed-search-update-entry entry)
                    (unless (use-region-p) (forward-line)))))

              (defun elfeed-send-readability-interactively ()
                "Send current feed link to pocket by mail."
                (interactive)
                (let ((entries (elfeed-search-selected)))
                  (dolist (entry entries)
                    (shell-command (concat  "contents=/tmp/borrar; touch $contents; echo '  ' > $contents; echo  '" (elfeed-entry-link entry) "' > $contents; " "mutt -F $HOME/Dropbox/scripts/muttrc_otra_offline -s '" (elfeed-feed-title (elfeed-entry-feed entry)) " : " (elfeed-entry-title entry) "' -- csantosb+csantosb@inbox.readability.com < $contents"))
                    (elfeed-untag entry 'unread)
                    (elfeed-search-update-entry entry)
                    (unless (use-region-p) (forward-line)))))

              (defun elfeed-send-pinboard-interactively ()
                "Send current feed link to pocket by mail."
                (interactive)
                (let ((entries (elfeed-search-selected)))
                  (dolist (entry entries)
                    (require 'pinboard-api)
                    (pinboard-api-add (elfeed-entry-link entry) (elfeed-entry-title entry) (concat "elfeed "
                                                                                                   "readability " (elfeed-feed-title (elfeed-entry-feed entry))) " " "yes" "yes")
                    (message "elfeed - sent %s : %s" (elfeed-entry-title entry) (elfeed-entry-link entry) )
                    (elfeed-untag entry 'unread)
                    (elfeed-search-update-entry entry)
                    (unless (use-region-p) (forward-line))))))

(defun csb/elfeed-fringes ()
  (let ((csb/fringe (cond ((eq (display-pixel-width) 1680) 250)
                          ((eq (display-pixel-width) 1920) 280)
                          (t 0))))
    (set-window-fringes (selected-window) csb/fringe csb/fringe nil))
  (redraw-display))

(add-hook 'persp-switch-hook
          (lambda() (if (string= (persp-name persp-curr) "elfeed")
                   (add-hook 'window-configuration-change-hook 'csb/elfeed-fringes)
                 (remove-hook 'window-configuration-change-hook 'csb/elfeed-fringes))))

(stante-after elfeed

              (define-key elfeed-search-mode-map (kbd "M-o a") 'elfeed-send-pocket-interactively)
              (define-key elfeed-search-mode-map (kbd "M-r a") 'elfeed-send-readability-interactively)
              (define-key elfeed-search-mode-map (kbd "M-p a") 'elfeed-send-pinboard-interactively)

              ;; navigation / scrolling

              (define-key elfeed-search-mode-map (kbd "<") 'beginning-of-buffer)
              (define-key elfeed-search-mode-map (kbd ">") 'end-of-buffer)

              ;; Enter
              (define-key elfeed-search-mode-map (kbd "l") 'elfeed-search-show-entry)
              (define-key elfeed-search-mode-map (kbd "RET") 'elfeed-search-show-entry)

              ;; (define-key elfeed-search-mode-map (kbd "SPC") '(lambda()(interactive)(scroll-up-command 4)))
              ;; (define-key elfeed-search-mode-map (kbd "S-SPC") '(lambda()(interactive)(scroll-down-command 4)))

              ;; filtering
              ;; (define-key elfeed-search-mode-map (kbd "f") 'elfeed-search-live-filter)
              ;; (define-key elfeed-search-mode-map (kbd "F") 'elfeed-search-set-filter)
              (define-key elfeed-search-mode-map "f" #'(lambda(arg)
                                                         (interactive "P")
                                                         (cond ((equal arg '(4))
                                                                (elfeed-search-set-filter))
                                                               (t (elfeed-search-live-filter)))))  ;; Filter the elfeed-search buffer as the filter is written.

              ;; updating
              (define-key elfeed-search-mode-map (kbd "$") 'elfeed-update)                  ;; update with fetch
              (define-key elfeed-search-mode-map (kbd "S") 'elfeed-search-update--force)    ;; update without fetch

              ;; tagging
              (define-key elfeed-search-mode-map (kbd "+") 'elfeed-search-tag-all)          ;; add tag
              (define-key elfeed-search-mode-map (kbd "-") 'elfeed-search-untag-all)        ;; remove tag
              (define-key elfeed-search-mode-map (kbd "r") 'elfeed-search-untag-all-unread) ;; add tag read
              (define-key elfeed-search-mode-map (kbd "u") 'elfeed-search-tag-all-unread)   ;; remove tag read
              (define-key elfeed-search-mode-map (kbd "e") (elfeed-expose #'elfeed-search-tag-all 'emacs))     ;; add tag emacs
              (define-key elfeed-search-mode-map (kbd "E") (elfeed-expose #'elfeed-search-untag-all 'emacs))   ;; remove tag emacs

              ;; browsing
              (define-key elfeed-search-mode-map (kbd "o") 'elfeed-search-browse-url-other-window)
              (define-key elfeed-search-mode-map (kbd "O") 'elfeed-search-browse-url)

              (define-key elfeed-search-mode-map "+" 'elfeed-search-tag-all)
              (define-key elfeed-search-mode-map "-" 'elfeed-search-untag-all)

              ;; (define-key elfeed-search-mode-map (kbd "e") 'elfeed-search-tag-all-emacs)
              ;; (define-key elfeed-search-mode-map (kbd "E") 'elfeed-search-untag-all-emacs)

              (define-key elfeed-search-mode-map (kbd "y") 'elfeed-search-yank) ;; Copy the selected feed item to

              ;; quit
              (define-key elfeed-search-mode-map (kbd "Q") (lambda()(interactive)(kill-this-buffer)(persp-kill "elfeed")))
              (define-key elfeed-search-mode-map (kbd "q") 'quit-window)

              ;; remove keys
              (define-key elfeed-search-mode-map (kbd "b") nil))

(stante-after elfeed

              ;; general
              (define-key elfeed-show-mode-map (kbd "y") 'elfeed-show-yank)
              (define-key elfeed-show-mode-map (kbd "g") 'elfeed-show-refresh)

              ;; navigation / scrolling

              (define-key elfeed-show-mode-map (kbd "f") 'elfeed-show-next)
              (define-key elfeed-show-mode-map (kbd "b") 'elfeed-show-prev)

              (define-key elfeed-show-mode-map (kbd "<") 'beginning-of-buffer)
              (define-key elfeed-show-mode-map (kbd ">") 'end-of-buffer)

              ;; links
              (define-key elfeed-show-mode-map (kbd "tab") 'shr-next-link)
              (define-key elfeed-show-mode-map [backtab] 'shr-previous-link)
              (define-key elfeed-show-mode-map (kbd "tab") 'shr-browse-url)

              ;; tagging
              (define-key elfeed-show-mode-map (kbd "+") 'elfeed-show-tag)
              (define-key elfeed-show-mode-map (kbd "-") 'elfeed-show-untag)

              ;; browsing
              (define-key elfeed-show-mode-map (kbd "o") 'elfeed-show-visit-other-window)
              (define-key elfeed-show-mode-map (kbd "O") 'elfeed-show-visit)

              ;; quit
              (define-key elfeed-show-mode-map (kbd "h") #'(lambda()(interactive)(elfeed-kill-buffer)(switch-to-buffer "*elfeed-search*")))

              ;; backup feed
              (define-key elfeed-show-mode-map (kbd "M-o a") 'elfeed-send-pocket-interactively)
              (define-key elfeed-show-mode-map (kbd "M-r a") 'elfeed-send-readability-interactively)
              (define-key elfeed-show-mode-map (kbd "M-i a") 'elfeed-send-pinboard-interactively)

              ;; remove defaults
              ;; (define-key elfeed-show-mode-map (kbd "b") nil)
              )

(stante-after elfeed
  (setq elfeed-search-filter "@6-months-ago -boulot +unread")
  (setq elfeed-max-connections 10)
  (setq elfeed-db-directory "~/.elfeed")
  (setq elfeed-search-title-max-width 70)
  (setq elfeed-sort-order 'descending)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-search-refresh-rate 5)
  (setq elfeed-search--offset 2)
  (setq url-queue-timeout 30)
  (setq elfeed-feeds
        '(;; Emacs
          ("http://emacsrocks.com/atom.xml" emacs emacs-rocks)
          ("http://emacspeak.blogspot.com/feeds/posts/default" emacs emacspeak)
          ("http://feeds.feedburner.com/GotEmacs?format=xml" emacs gotemacs)
          ;;("http://ergoemacs.org/emacs/blog.xml" emacs ergoemacs)
          ("http://planet.emacsen.org/atom.xml" emacs emacsen)
          ("http://whattheemacsd.com/atom.xml" emacs whattheemacsd)

          ("http://emacs-fu.blogspot.com/feeds/posts/default?alt=rss" emacs emacs-fu)

          ;; ("http://www.reddit.com/r/emacs/.rss" emacs reddit)
          ("http://upsilon.cc/~zack/tags/emacs/index.atom" emacs zack)
          ("http://upsilon.cc/~zack/tags/orgmode/index.rss" emacs zack)
          ("http://jr0cket.co.uk/atom.xml" emacs jr0cket)

          ("http://blog.binchen.org/rss.xml" emacs chenbin)
          ;; ("http://feeds.feedburner.com/KrisJenkinsBlog" emacs kris)
          ("http://emacsmovies.org/atom.xml" emacs emacsmovies)

          ;; Quora
          ;; ("https://www.quora.com/Emacs/rss" emacs quora)
          ;; ("https://www.quora.com/Emacs-Tips/rss" emacs quora)
          ;; ("https://www.quora.com/Org-Mode-emacs/rss" emacs quora org-mode)

          ;; Sacha
          ("http://feeds.sachachua.com/sachac" sachac)
          ("https://yoo2080.wordpress.com/category/Emacs/feed/" emacs yoobox)
          ("https://yoo2080.wordpress.com/category/Lisp/feed/" lisp yoobox)
          ("http://nullprogram.com/tags/lisp/feed/" lisp nullprogram)
          ("http://nullprogram.com/tags/emacs/feed/" emacs nullprogram)
          ;; ("https://emacs.stackexchange.com/feeds" emacs se)

          ;; Kitchin Group
          ("http://kitchingroup.cheme.cmu.edu/blog/feed/index.xml" emacs kitchin)                       ;; all
          ("https://kitchinresearchgroup.disqus.com/latest.rss" emacs kitchin comments)                 ;; comments
          ("http://kitchingroup.cheme.cmu.edu/blog/category/org/feed" emacs kitchin org)                ;; org
          ("http://kitchingroup.cheme.cmu.edu/blog/category/orgmode/feed" emacs kitchin orgmode)        ;; orgmode
          ("http://kitchingroup.cheme.cmu.edu/blog/category/emacs/feed" emacs kitchin em)               ;; emacs
          ("http://kitchingroup.cheme.cmu.edu/blog/category/emacs-lisp/feed" emacs kitchin emacs-lisp)  ;; emacs-lisp

          ;; Irreal
          ("http://irreal.org/blog/?feed=rss2" emacs irreal)
          ("http://irreal.org/blog/?feed=comments-rss2" emacs irreal comments)

          ;; Blogs
          ("http://emacsredux.com/atom.xml" emacs emacsredux)
          ("http://www.masteringemacs.org/feed" emacs mastering)
          ("http://feeds.feedburner.com/emacsblog" emacs all-things-emacs)
          ("http://www.lunaryorn.com/feed.atom" emacs lunarsite)
          ("http://oremacs.com/atom.xml" emacs oremacs)
          ("http://endlessparentheses.com/atom.xml" emacs endlessparentheses)
          ("http://feeds.feedburner.com/WisdomAndWonder" emacs wisdomandwonder)
          ("http://www.howardism.org/index.xml" emacs howardism)
          ("http://pragmaticemacs.com/feed/" emacs pragmatic)

          ;; Linux
          ("http://feedproxy.google.com/linuxtoday/linux" linux today)
          ("http://xmodulo.com/feed" linux xmodulo)

          ;; Lobsters
          ;; ("https://lobste.rs/rss" lobsters)
          ;; Boulot
          ;; ("http://www.optioncarriere.com/search/rss.html?s=fpga&amp;l=ile%20de%20france&amp;sb_lang=&amp;lid=24110" boulot optioncarriere)
          ;; ("http://www.optioncarriere.com/search/rss.html?s=fpga&amp;l=France&amp;sb_lang=&amp;lid=57" boulot optioncarriere)
          ;; ("http://www.optioncarriere.com/search/rss.html?s=fpga&amp;l=paris&amp;lid=24172" boulot optioncarriere)
          ;; ("http://www.jobijoba.es/rss.php?rch=fpga&amp;loc=" boulot jobijoba)
          ;; ("http://www.opcionempleo.com/search/rss.html?s=fpga&amp;l=espa%C3%B1a&amp;lid=34835" boulot opcionempleo)
          ;; ("http://www.infojobs.net/trabajos.rss/kw_fpga/" boulot infojobs)
          ;; ("http://www.tecnoempleo.com/alertas-empleo-rss.php?te=fpga&amp;pr=0&amp;ex=&amp;es=&amp;cp=&amp;co=&amp;du=&amp;min=&amp;max=" boulot tecnoempleo)

          ;; Economy
          ("http://juantorreslopez.com/feed/" economy juantorreslopez)
          ("http://www.vnavarro.org/?feed=atom" economy vincent)

          ;; Politics
          ("http://sistemaencrisis.es/feed/" politics sistemaencrisis))))

(stante-after elfeed
              ;; update every half hour
              ;;(if (string= (system-name) "csantosb.do.arch")
              ;;    '(progn
              (run-with-timer 0 6000 'elfeed-update)
              ;; (elfeed-make-tagger &key FEED-TITLE FEED-URL ENTRY-TITLE ENTRY-LINK AFTER BEFORE ADD REMOVE CALLBACK)
              (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "h"
                                                                   :callback 'rss_callback))
              ;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "lobste"
              ;;                                                      :callback 'elfeed-send-pinboard
              ;;                                                      :remove 'unread))
              ;;)
              (setq elfeed-timer-update (run-with-timer 0 6000 'elfeed-search-update--force))
              (setq elfeed-db-compact (run-with-timer 0 11234 'elfeed-db-compact)))

(add-hook 'elfeed-show-mode-hook
          (lambda ()
            (setq-local show-trailing-whitespace nil)
            (setq-local fill-column 120)
            (smartscan-mode -1)
            (modal-mode 1)
            (setq-local mode-line-format nil)
            ;; (text-scale-set 0.5)
            ;; Edit elfeed config mile
            ;; (local-set-key (kbd "<f3>")
            ;;             #'(lambda()(interactive)(find-config-file "csb-elfeed.page")))
            ;; (local-set-key (kbd "<f4>")
            ;;             #'(lambda()(interactive)(find-config-file-other-window "csb-elfeed.page")))
            ))

(add-hook 'elfeed-search-mode-hook
          (lambda ()
            (setq-local focus-out-hook nil)
            (setq-local show-trailing-whitespace nil)
            (setq-local fill-column 120)
            (smartscan-mode -1)
            (modal-mode 1)
            (text-scale-increase 0.5)
            (setq-local mode-line-format nil)
            ;; (fringe-mode '( 280 . 280))
            ;; (text-scale-set 0.5)
            ;; Edit elfeed config mile
            ;; (local-set-key (kbd "<f3>")
            ;;             #'(lambda()(interactive)(find-config-file "csb-elfeed.page")))
            ;; (local-set-key (kbd "<f4>")
            ;;             #'(lambda()(interactive)(find-config-file-other-window
            ;;                                 "csb-elfeed.page")))
            ))

(provide 'csb-elfeed)

;;; csb-elfeed.el ends here
