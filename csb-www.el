;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(autoload 'w3m-search "w3m-search" "Ask a WWW browser to show a URL." t)
(autoload 'eww-browse-url "eww" "Ask a WWW browser to show a URL." t)

;; (setq browse-url-browser-function 'w3m-browse-url)
;; (setq browse-url-browser-function 'browse-url-firefox)
(setq browse-url-browser-function 'eww-browse-url)

(setq browse-url-generic-program "firefox" )

(setq browse-url-new-window-flag t)

(eval-after-load 'w3m
  '(progn
     (setq w3m-use-title-buffer-name nil)
     (setq w3m-coding-system 'utf-8)
     (setq w3m-file-coding-system 'utf-8)
     (setq w3m-file-name-coding-system 'utf-8)
     (setq w3m-input-coding-system 'utf-8)
     (setq w3m-output-coding-system 'utf-8)
     (setq w3m-terminal-coding-system 'utf-8)
     (setq w3m-home-page "https://pinboard.in/u:csantosb")
     (setq w3m-use-title-buffer-name t)))

(eval-after-load 'w3m-search
  '(progn
     (require 'w3m-search)
     (setq-default w3m-search-default-engine '"google")
     (setq-default w3m-search-word-at-point t)
     (setq-default w3m-search-thing-at-point-arg 'word)
     ;; (add-to-list 'w3m-search-engine-alist '("emacs-wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?search=%s"))
     (add-to-list 'w3m-search-engine-alist '("pin" "https://pinboard.in/u:csantosb/t:%s"))
     ;; (add-to-list 'w3m-search-engine-alist
     ;;              '("My engine"
     ;;                "http://my.searchengine.com/?query=%s"
     ;;                nil))
     ;; (add-to-list 'w3m-uri-replace-alist
     ;;              '("\\`my:" w3m-search-uri-replace "My engine"))
     )
  )

(with-eval-after-load 'eww
  ;; (defvar-local my-local-eww-timer nil "Eww-Buffer-local timer.")
  (defvar-local do-it-once t "just run timer once")
  (defvar avoid-justification '("startpage.com"  "pinboard.in" "duckduckgo.com" "google.com" "google.es" "google.fr" "www.reddit.com" "m.reddit.com"
                                "gnu.org" "stackoverflow.com" "emacs.stackexchange.com" "web-rh.dsi.cnrs.fr" "kitchingroup.cheme.cmu.edu"
                                "oremacs.com" "endlessparentheses.com" "github.com" "irreal.org")
    "List of domains not to be auto-justified")
  ;; (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  ;; (defvar-local eww-update-buffer-name nil)
  (setq eww-search-prefix "https://startpage.com/do/m/mobilesearch?query=")
  (setq eww-search-prefix "https://startpage.com/do/search?query=")
  (setq shr-external-browser 'browse-url-default-browser)
  (setq eww-download-directory "/tmp/")
  (setq eww-header-line-format "  %t - %u")
  (setq shr-max-image-proportion '0.9)
  (setq shr-blocked-images nil)
  (setq eww-download-directory "/tmp/")
  (setq eww-use-external-browser-for-content-type "\\`\\(video/\\|audio/\\|application/ogg\\)"))

(defun toggle-eww ()
  "Switch to a eww buffer or return to the previous buffer."
  (interactive)
  (if (derived-mode-p 'eww-mode)
      ;; Currently in a eww buffer
      ;; Bury buffers until you reach a non-w3m one
      (while (derived-mode-p 'eww-mode)
        (bury-buffer))
    ;; Not in eww: find the first eww buffer
    (let ((list (buffer-list)))
      (while list
        (if (with-current-buffer (car list)
              (derived-mode-p 'eww-mode))
            (progn
              (switch-to-buffer (car list))
              (setq list nil))
          (setq list (cdr list))))
      (unless (derived-mode-p 'eww-mode)
        (call-interactively 'eww)))))

(with-eval-after-load 'w3m

  (defun wicked/w3m-open-current-page-in-firefox ()
    "Open the current URL in Mozilla Firefox."
    (interactive)
    (browse-url-firefox w3m-current-url))

  (defun wicked/w3m-open-link-or-image-in-firefox ()
    "Open the current link or image in Firefox."
    (interactive)
    (browse-url-firefox (or (w3m-anchor)
                            (w3m-image)))))

(eval-after-load 'w3m
  '(progn
     (defun my-w3m-search-new-session()
       (interactive)
       (let ((current-prefix-arg t))
         (call-interactively 'w3m-search-new-session)))))

(define-key launcher-map "wwj" #'browse-url)
(define-key launcher-map "wwp" #'browse-url-at-point)
(define-key launcher-map "wwt" #'toggle-eww)
;; (define-key endless/toggle-map (kbd "w") 'toggle-eww)
;; select search engine by menu
(defvar eww-search-engine-map (make-sparse-keymap "Search engine:"))
(define-key launcher-map "wws" eww-search-engine-map)
(define-key eww-search-engine-map (kbd "d")
  (cons "Duckduckgo" (lambda()(interactive)(setq eww-search-prefix "https://duckduckgo.com/html/?q="))))
(define-key eww-search-engine-map (kbd "e")
  (cons "Google es" (lambda()(interactive)(setq eww-search-prefix "https://www.google.es/search?q="))))
(define-key eww-search-engine-map (kbd "f")
  (cons "Google fr" (lambda()(interactive)(setq eww-search-prefix "https://www.google.fr/search?q="))))
(define-key eww-search-engine-map (kbd "g")
  (cons "Google.com" (lambda()(interactive)(setq eww-search-prefix "https://www.google.com/search?q="))))
(define-key eww-search-engine-map (kbd "s")
  (cons "Startpage" (lambda()(interactive)(setq eww-search-prefix "https://startpage.com/do/m/mobilesearch?query="))))
(define-key eww-search-engine-map (kbd "c")
  (cons "Cancel" (lambda()(interactive)())))

(with-eval-after-load 'eww
  ;; ;; (setq w3m-coding-system 'utf-8
  ;; ;;       w3m-home-page "https://pinboard.in/u:csantosb"
  ;; ;;       )
  ;; (define-key eww-mode-map "o" 'eww-browse-with-external-browser)
  ;; (define-key eww-mode-map "&" nil)
  ;; ;; (define-key eww-mode-map "\M-p" 'eww-list-bookmarks)
  ;; ;; (define-key eww-mode-map "\M-p" 'url-cookie-list)
  ;; ;; (define-key eww-mode-map "\M-p" 'eww-list-histories)
  ;; ;; (define-key eww-mode-map "\M-p" 'eww-download)

  (define-key eww-mode-map (kbd "C-+") (lambda () (interactive) (text-scale-increase 0.5) (eww-reload)))
  (define-key eww-mode-map (kbd "C--") (lambda () (interactive) (text-scale-decrease 0.5) (eww-reload)))

  (define-key eww-mode-map "p" 'eww-previous-url)
  (define-key eww-mode-map "n" 'eww-next-url)

  ;; Readability
  (define-key eww-mode-map "R" (lambda nil (interactive)(eww-readable)(csb/mode-line-off)))

  ;; links
  (define-key eww-mode-map "f" 'eww-lnum-follow)
  (define-key eww-mode-map "F" 'eww-lnum-universal)
  (define-key eww-mode-map "<return>" 'eww-follow-link)
  (define-key eww-mode-map "O" (lambda()(interactive)(call-interactively 'browse-url)(insert (plist-get eww-data :url))))

  ;; History
  (define-key eww-mode-map "H" 'eww-back-url)
  (define-key eww-mode-map "L" 'eww-forward-url)
  (define-key eww-mode-map "l" 'eww-list-histories)

  ;; (define-key eww-mode-map "U" 'eww-top-url)
  ;; (define-key eww-mode-map "u" 'eww-up-url)

  (define-key eww-mode-map "u" (lambda()
                                 "Go up one level"
                                 (interactive)
                                 ;; split adress
                                 (setq mylist (split-string (nth 1 (split-string (plist-get eww-data :url) "//")) "/"))
                                 ;; remove last ("") in case address ends by an /
                                 (if (string= (car (last mylist)) "")
                                     (setq nb_to_remove 2)
                                   (setq nb_to_remove 1))
                                 ;; concat all but last chunk
                                 (setq salida nil)
                                 (while (> (length mylist) nb_to_remove)
                                   (setq salida (concat salida (car mylist) "/"))
                                   (setq mylist (cdr mylist)))
                                 ;; say it
                                 (message (format "requesting address %s" salida))
                                 ;; get there
                                 (eww-browse-url salida)))

  (define-key eww-mode-map "U" (lambda()
                                 "Go to the Top Level"
                                 (interactive)
                                 (eww-browse-url (nth 0 (split-string (nth 1 (split-string (plist-get eww-data :url) "//")) "/")))))

  ;; (define-key eww-mode-map "h" 'eww-history-browse)

  ;; quit
  (define-key eww-mode-map "q" 'quit-window)
  (define-key eww-mode-map "r" (lambda()(interactive)(eww-reload)(kill-other-buffer)))
  (define-key eww-mode-map "Q" 'kill-this-buffer)

  ;; (define-key eww-mode-map "p" 'csb/jump-up)
  ;; (define-key eww-mode-map "n" 'csb/jump-down)

  ;; (define-key eww-mode-map "j" 'scroll-up-line)
  ;; (define-key eww-mode-map "k" 'scroll-down-line)

  (define-key eww-mode-map "J" (lambda()(interactive)
                                 (read-only-mode 0)
                                 (set-justification-full (point-min) (point-max))
                                 (read-only-mode t)))

  ;; (define-key eww-mode-map "g" 'beginning-of-buffer)
  ;; (define-key eww-mode-map "G" 'end-of-buffer)
  (define-key eww-mode-map "G" 'eww-end-of-text)
  (define-key eww-mode-map "g" 'eww-beginning-of-text)

  (define-key eww-mode-map "d" 'eww-download)

  (define-key eww-mode-map "y" 'eww-copy-page-url)
  (define-key eww-mode-map "o" 'eww-browse-with-external-browser)

  (define-key eww-mode-map "s" 'eww-view-source)

  (define-key eww-mode-map "h"
    (lambda ()(interactive)
      (eww-browse-url "http://pinboard.in/u:csantosb")))

  (defvar-local endless/display-images t)
  (defun endless/backup-display-property (invert &optional object)
    "Move the 'display property at POS to 'display-backup.
    Only applies if display property is an image.
    If INVERT is non-nil, move from 'display-backup to 'display
    instead.
    Optional OBJECT specifies the string or buffer. Nil means current
    buffer."
    (let* ((inhibit-read-only t)
           (from (if invert 'display-backup 'display))
           (to (if invert 'display 'display-backup))
           (pos (point-min))
           left prop)
      (while (and pos (/= pos (point-max)))
        (if (get-text-property pos from object)
            (setq left pos)
          (setq left (next-single-property-change pos from object)))
        (if (or (null left) (= left (point-max)))
            (setq pos nil)
          (setq prop (get-text-property left from object))
          (setq pos (or (next-single-property-change left from object)
                        (point-max)))
          (when (eq (car prop) 'image)
            (add-text-properties left pos (list from nil to prop) object))))))
  (define-key eww-mode-map "i" (lambda() "Toggle images display on current buffer."
                                 (interactive)
                                 (setq endless/display-images
                                       (null endless/display-images))
                                 (endless/backup-display-property endless/display-images)))


  ;; Bookmarking
  ;; (define-key eww-mode-map "\M-n" 'eww-next-bookmark)
  ;; (define-key eww-mode-map "\M-p" 'eww-previous-bookmark)
  (define-key eww-mode-map "b" 'eww-add-bookmark)
  (define-key eww-mode-map "B" 'eww-list-bookmark)
  ;; ;; (define-key eww-mode-map "U" 'eww-bookmark-browse)
  ;; ;; (define-key eww-mode-map "U" 'eww-bookmark-yank)
  ;; ;; (define-key eww-mode-map "U" 'eww-bookmark-kill)

  ;; ;; (define-key eww-mode-map "U" 'eww-bookmark-kill-ring)
  ;; ;; (define-key eww-mode-map "U" 'eww-toggle-checkbox)

  ;; ;; (define-key eww-mode-map "U" 'eww-submit)


  ;; nil
  (define-key eww-mode-map "w" nil)
  (define-key eww-mode-map "r" nil)
  (define-key eww-mode-map "&" nil)
  (define-key eww-mode-map "v" nil)
  ;; ;; DEL     scroll-down-command
  ;; ;; S-SPC       scroll-down-command
  ;; ;; <delete>    scroll-down-command
  ;; ;; <remap>     Prefix Command
  )

(eval-after-load 'w3m
  '(progn

     )
)

(eval-after-load 'w3m
    '(progn

       ;; (defun w3m-pinboard-add-current-buffer ()
       ;;   (interactive)
       ;;   (pinboard-add-interactively w3m-current-url w3m-current-title))

       (defun w3m-autistici-link-get (tag)
         "Only search tags (separate a maximum of 3 TAGS with a '+' eg., emacs+manual)"
         (interactive "sEnter tag ... : ")
         (setenv "LC_ALL" "C")
         (setenv "LANG" "C")
         (w3m-browse-url (concat "https://link.autistici.org/search.php/csantosb/" tag)  t)
         )
)
)

(eval-after-load 'w3m
  '(progn

     ;; (require 'pinboard-api)

     ;; (setq pinboard-tags-cache-file-name "~/Dropbox/.emacs.d/.pinboard-tags-cache")
     ;; (setq pinboard-completing-read-function 'helm-comp-read) ;; C-Ret to abort (see mode-line)

     )
)

(eval-after-load 'w3m
  '(progn

     ;; ;; add
     ;; (define-key w3m-mode-map (kbd "M-p a") 'w3m-pinboard-add-current-buffer)

     ;; ;; get
     ;; (define-key w3m-mode-map (kbd "M-p g") 'w3m-pinboard-get)

     ;; ;; go-home
     ;; (define-key w3m-mode-map (kbd "M-p H") (lambda () (interactive) (w3m-browse-url "https://pinboard.in/u:csantosb")))
     ;; (define-key w3m-mode-map (kbd "M-p h") (lambda () (interactive) (w3m-goto-url-new-session "https://pinboard.in/u:csantosb")))

     ;; ;; unread
     ;; (define-key w3m-mode-map (kbd "M-p u")
     ;;   (lambda()
     ;;     (interactive)
     ;;     (w3m-browse-url "https://pinboard.in/u:csantosb/unread")))

     ;; ;; tag
     ;; (define-key w3m-mode-map (kbd "M-p t")
     ;;   (lambda () (interactive)
     ;;     (let ((a-tag (read-from-minibuffer "tag?" " ")))
     ;;       (browse-url (shell-command-to-string (concat "sr pin " a-tag))))))

     )
)

(add-hook 'w3m-mode-hook '(lambda ()
                            (w3m-lnum-mode 1)
                            (toggle-read-only)
                            (local-set-key (kbd "C-c g I") 'find-www-config-file)
                            (local-set-key (kbd "C-c g i") 'find-www-config-file-other)
                            (smartscan-mode -1)
                            (if (eq 'x (window-system))
                                (w3m-toggle-inline-images))))

(add-hook 'eww-mode-hook (lambda ()
                           ;; (setq-local fill-column 120)
                           (set-window-fringes (selected-window) 250 250 nil)
                           (setq-local show-trailing-whitespace nil)
                           (csb/mode-line-off)
                           (text-scale-set 1)
                           (modal-mode 1)
                           (text-scale-adjust 0)
                           (smartscan-mode -1)))

;; (defadvice eww-update-header-line-format (after eww-update-header-line-format-modif activate)
;;   (progn
;;     (read-only-mode -1)
;;     ;; (when (and (plist-get eww-data :title) eww-update-buffer-name)
;;     ;;  (rename-buffer (concat "eww - " (plist-get eww-data :title)) t))
;;     (when (and (plist-get eww-data :url)
;;                (not (member (nth 0 (split-string (nth 1 (split-string (plist-get eww-data :url) "//")) "/")) avoid-justification)))
;;       (set-justification-full (point-min) (point-max)))
;;     (read-only-mode t)))

;; (ad-unadvise 'eww-update-header-line-format)

(provide 'csb-www)

;;; csb-www.el ends here
