;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

;; (add-hook 'persp-switch-hook
;;           (lambda()
;;             (let ((csb/fringe (cond ((eq (display-pixel-width) 1680) 250)
;;                                     ((eq (display-pixel-width) 1920) 320)
;;                                     (t 0))))
;;               (when (string= (persp-name persp-curr) "Mail")
;;                 (fringe-mode `( ,csb/fringe . ,csb/fringe ))))))

(defun csb/mail-fringes ()
  (let ((csb/fringe (cond ((eq (display-pixel-width) 1680) 250)
                          ((eq (display-pixel-width) 1920) 320)
                          (t 0))))
    (set-window-fringes (selected-window) csb/fringe csb/fringe nil))
  (redraw-display))

(stante-after mu4e
              (setq mu4e-org-contacts-file "~/.mutt/contacts.org"
                    mu4e-msg2pdf (executable-find "msg2pdf")
                    mu4e-maildir       "~/Maildir"                 ;; top-level Maildir
                    mml-smime-sign-with-sender t
                    mu4e-mu-home       nil                         ;; Location of the mu homedir, or nil for the default.
                    mu4e-sent-folder   "/sent"                     ;; folder for sent messages
                    mu4e-drafts-folder "/drafts"                   ;; unfinished messages
                    mu4e-trash-folder  "/trash"                    ;; saved messages
                    mu4e-get-mail-command "true"                   ;; command to retrieve new mail; true does nothing
                    mu4e-main-buffer-name "*mu4e-main*"
                    mu4e-update-interval 60                        ;; seconds between automatic calls to retrieve mail and update the database.
                    ;; If nil, don't update automatically.
                    mu4e-view-show-images t                        ;; automatically display attached images in the message view buffer
                    mu4e-view-image-max-width 800
                    mu4e-sent-messages-behavior 'sent              ;; determines what mu4e does with sent messages
                    mu4e-split-view 'horizontal
                    mu4e-decryption-policy t
                    mu4e-use-fancy-chars t
                    mu4e-html2text-command 'mu4e-shr2text          ;; use eww for html (html2text)
                    mu4e-completing-read-function 'completing-read ;; helm instead of ido
                    mu4e-compose-signature "error"
                    mu4e-compose-signature-auto-include nil))      ;; Whether to automatically include a message-signature in new

(setq mu4e-maildir-shortcuts '( ("/inventati/inbox" . ?i)
                                ("/ciemat/inbox" . ?c)
                                ("/otra/inbox" . ?t)
                                ("/curro/inbox" . ?u)
                                ("/iphc/inbox" . ?h)
                                ("/reyes/inbox" . ?r)
                                ("/cern/inbox" . ?e)))

(stante-after mu4e
              (when (fboundp 'imagemagick-register-types)
                (imagemagick-register-types)))

(stante-after mu4e
              (defun csb/mu4e-guess-current-account()
                "Try to guess current account based on last query"
                (let ((current-account))
                  (catch 'error
                    (dolist (account mu4e-multi-account-alist)
                      (when (string-prefix-p
                             (format "maildir:\"/%s/" (car account))
                             (mu4e-last-query))
                        (setq current-account (car account))
                        (throw 'error t))))
                  current-account)))

(stante-after mu4e
              (defun csb/mu4e-reply-mail ()
                 (interactive)
                "Compose a new email directed to the selected contacts."

                (let* ((csb/mu4e-reply-mail-comma ", ")
                       (current-account (csb/mu4e-guess-current-account)))

                  ;; create new message buffer except if already in one
                  ;; then, set to the corresponding account
                  ;; if I cannot guess, prompt the user
                  (setq mu4e-multi-last-read-account current-account)
                  (mu4e-compose 'reply)
                  (mu4e-multi-compose-set-account current-account)

                  (end-of-buffer)
                  (newline)
                  (newline)
                  (message-insert-signature)
                  (delete-trailing-whitespace)
                  (re-search-backward "^--[\n]")
                  (forward-line -2))))
                  ;; for each candidate, insert the relevant information in To: field
                  ;; candidates are separated by commas
                  ;; (dolist (candidate (helm-marked-candidates))
                  ;;   (let* ((cand (split-string candidate "\t"))
                  ;;          (name (cadr cand))
                  ;;          (address (car cand)))
                  ;;     (message-goto-to)
                  ;;     (insert (format "%s%s <%s>" csb/mu4e-reply-mail-comma name address))
                  ;;     (setq csb/mu4e-reply-mail-comma ", ")))

                  ;; ;; get to Subject: field and prompt the user
                  ;; ;; except if already done
                  ;; (unless already-in-message
                  ;;   (let ((subject (read-from-minibuffer "Subject: ")))
                  ;;     (message-goto-subject)
                  ;;     (insert subject)))

                  ;; (if already-in-message
                  ;;     ;; ready for composing the message
                  ;;     (progn
                  ;;    (re-search-forward "^--[\n]")
                  ;;    (previous-line)
                  ;;    (re-search-backward ".")
                  ;;    (forward-char))
                  ;;   ;; insert signature
                  ;;   (progn
                  ;;     ))

(stante-after mu4e
              (defun csb/mu4e-compose-mail (candidate)
                "Compose a new email directed to the selected contacts."
                (let* ((already-in-message (eq major-mode 'mu4e-compose-mode))
                       (csb/mu4e-compose-mail-comma (if already-in-message ", " ""))
                       (current-account (csb/mu4e-guess-current-account)))

                  (unless already-in-message
                    (persp-switch "Mail")
                    (when (get-buffer "*mu4e-headers*")
                      (switch-to-buffer "*mu4e-headers*")))

                  ;; create new message buffer except if already in one
                  ;; then, set to the corresponding account
                  ;; if I cannot guess, prompt the user
                  (unless already-in-message
                    (let ((account (if (and current-account (not (equal helm-current-prefix-arg '(4))))
                                       current-account
                                     (mu4e-multi-minibuffer-read-account))))
                      (setq mu4e-multi-last-read-account account)
                      (mu4e-compose 'new)
                      (mu4e-multi-compose-set-account account)))

                  ;; for each candidate, insert the relevant information in To: field
                  ;; candidates are separated by commas
                  (dolist (candidate (helm-marked-candidates))
                    (let* ((cand (split-string candidate "\t"))
                           (name (cadr cand))
                           (address (car cand)))
                      (message-goto-to)
                      (insert (format "%s%s <%s>" csb/mu4e-compose-mail-comma name address))
                      (setq csb/mu4e-compose-mail-comma ", ")))

                  ;; get to Subject: field and prompt the user
                  ;; except if already done
                  (unless already-in-message
                    (let ((subject (read-from-minibuffer "Subject: ")))
                      (message-goto-subject)
                      (insert subject)))

                  (if already-in-message
                      ;; ready for composing the message
                      (progn
                        (re-search-forward "^--[\n]")
                        (previous-line)
                        (re-search-backward ".")
                        (forward-char))
                    ;; insert signature
                    (progn
                      (end-of-buffer)
                      (newline)
                      (newline)
                      (message-insert-signature)
                      (delete-trailing-whitespace)
                      (re-search-backward "^--[\n]")
                      (forward-line -2))))))

(global-set-key (kbd "C-x m") (lambda()(interactive)
                                (require 'mu4e)
                                (helm-mu-contacts)))

(stante-after mu4e
              (defun csb/mu4e-get-to()
                "Get to a maildir.
Dynamically compute the ´sent´ maildir based on current maildir"
                (interactive)
                (let ((mu4e-maildir-shortcuts
                       (add-to-list 'mu4e-maildir-shortcuts
                                    `(,(format "/%s/sent" (csb/mu4e-guess-current-account)) . ?s))))
                  (call-interactively 'mu4e~headers-jump-to-maildir))))

(defun csb/mu4e ()
  (interactive)
  (cond ((file-exists-p "~/.gnupg/pubring.kbx")
         (require 'jl-encrypt)
         (require 'mu4e)
         (require 'mu4e-contrib)
         (require 'helm-mu)
         (persp-switch "Mail")
         (if (get-buffer "*mu4e-headers*")
             (progn
               (switch-to-buffer "*mu4e-headers*")
               (delete-other-windows))
           (progn
             (mu4e)
             (csb/mode-line-off))))
        (t (message "NO GPG KEY FOUND, SB OPEN??"))))
  (define-key launcher-map "4" #'csb/mu4e)

(stante-after mu4e
              (defun csb/mu4e-sync-with-server() (interactive)
                     (cond ((file-exists-p "~/.gnupg/pubring.kbx")
                            (let* ((account (mu4e-multi-minibuffer-read-account))
                                   (mu4e-get-mail-command
                                    (format "%s %s %s%s%s%s%s"
                                            "offlineimap -a"
                                            account
                                            "-f INBOX -o -k "
                                            "Repository_remote-"
                                            account
                                            "-repository:remotepass="
                                            (password-store-get "mail/mypasswd"
                                                                (concat "pw_" account ":")))))
                              (mu4e-update-mail-and-index nil)))
                           (t (message "NO GPG KEY FOUND, SB OPEN??")))))

(stante-after mu4e
              (define-key mu4e-compose-mode-map (kbd "C-c C-k")
                (lambda()(interactive)(message-kill-buffer)
                  (when (and (string= (persp-name persp-curr) "Mail")
                             (get-buffer "*mu4e-headers*"))
                    (switch-to-buffer "*mu4e-headers*")))))

(stante-after mu4e (add-hook 'mu4e-compose-mode-hook (lambda()(epa-mail-mode))))

(stante-after mu4e
              (define-key mu4e-main-mode-map "$" #'csb/mu4e-sync-with-server)   ;; syncs with server and update db
              (define-key mu4e-main-mode-map "S" #'mu4e-update-mail-and-index)) ;; only update db when mu4e-get-mail-command is "true"

(stante-after mu4e
              (define-key mu4e-main-mode-map "q" 'mu4e-quit)
              (define-key mu4e-main-mode-map (kbd "Q")
                #'(lambda() (interactive)
                    (setq-local mu4e-confirm-quit nil)
                    (mu4e-quit)
                    (persp-kill "Mail"))))

(stante-after mu4e
              (define-key mu4e-main-mode-map "g" 'mu4e~headers-jump-to-maildir)
              ;; zoom
              (define-key mu4e-main-mode-map (kbd "C-+") #'(lambda() (interactive) (text-scale-increase 0.5)))
              (define-key mu4e-main-mode-map (kbd "C--") #'(lambda() (interactive) (text-scale-decrease 0.5))))

(stante-after mu4e
              (define-key mu4e-main-mode-map "C" nil)
              (define-key mu4e-main-mode-map "U" nil)
              (define-key mu4e-main-mode-map "j" nil))

(stante-after mu4e
  (add-hook 'mu4e-main-mode-hook (lambda () (setq-local show-trailing-whitespace nil))))

(stante-after mu4e-headers
              (setq mu4e-headers-visible-columns 30 ;; number of columns to display for the header view when using the vertical split-view
                    mu4e-headers-visible-lines 10
                    mu4e~headers-sort-direction 'descending
                    mu4e-headers-fields '((:human-date . 12)
                                          (:flags . 6)
                                          (:mailing-list . 20)
                                          (:from . 30)
                                          (:subject . nil))))

(stante-after mu4e-headers
              (setq mu4e-headers-visible-flags
                    '(draft flagged new passed replied seen trashed attach encrypted signed unread)))

(stante-after mu4e-headers

              ;; navigation / scrolling
              ;; (define-key mu4e-headers-mode-map "j" 'mu4e-headers-next) ;; def n
              ;; (define-key mu4e-headers-mode-map "k" 'mu4e-headers-prev) ;; def p

              ;; (define-key mu4e-headers-mode-map "n" 'mu4e-headers-next) ;; def
              ;; (define-key mu4e-headers-mode-map "p" 'mu4e-headers-prev) ;; def

              ;; (define-key mu4e-headers-mode-map (kbd "SPC") 'scroll-up-line)
              ;; (define-key mu4e-headers-mode-map (kbd "S-SPC") 'scroll-down-line)

              (define-key mu4e-headers-mode-map (kbd "<") 'beginning-of-buffer)
              (define-key mu4e-headers-mode-map (kbd ">") 'end-of-buffer)

              ;; Enter
              (define-key mu4e-headers-mode-map "l" 'mu4e-headers-view-message)
              (define-key mu4e-headers-mode-map (kbd "RET") 'mu4e-headers-view-message)

              ;; jump to ...
              (define-key mu4e-headers-mode-map "g" #'csb/mu4e-get-to)

              ;;mu4e-headers-rerun-search def g
              (define-key mu4e-headers-mode-map "P" 'mu4e-headers-toggle-threading)

              ;; mark
              (define-key mu4e-headers-mode-map "D" 'mu4e-headers-mark-for-delete)
              (define-key mu4e-headers-mode-map "d" 'mu4e-headers-mark-for-trash)

              ;; message composition
              (define-key mu4e-headers-mode-map "R" #'csb/mu4e-reply-mail)
              (define-key mu4e-headers-mode-map "F" 'mu4e-compose-forward)
              (define-key mu4e-headers-mode-map "C" 'mu4e-compose-new)
              (define-key mu4e-headers-mode-map "E" 'mu4e-compose-edit)

              (define-key mu4e-headers-mode-map "$" 'mu4e-show-log)
              (define-key mu4e-headers-mode-map "H" 'mu4e-display-manual)

              (define-key mu4e-headers-mode-map "a" 'mu4e-headers-action)

              (define-key mu4e-headers-mode-map "/" 'mu4e-headers-search-narrow)
              (define-key mu4e-headers-mode-map "i" 'mu4e-headers-search-narrow)

              ;; Zoom
              (define-key mu4e-headers-mode-map (kbd "C-+") (lambda() (interactive) (text-scale-increase 0.5)))
              (define-key mu4e-headers-mode-map (kbd "C--") (lambda() (interactive) (text-scale-decrease 0.5)))

              ;; Sync
              (define-key mu4e-headers-mode-map "$" #'csb/mu4e-sync-with-server)
              (define-key mu4e-headers-mode-map "S" #'mu4e-update-mail-and-index)

              ;; quit
              (define-key mu4e-headers-mode-map (kbd "Q") #'(lambda()(interactive)
                                                              (setq-local mu4e-confirm-quit nil)
                                                              (mu4e~headers-quit-buffer)
                                                              (mu4e-quit)
                                                              (persp-kill "Mail")))
              (define-key mu4e-headers-mode-map (kbd "q") #'(lambda()(interactive)(mu4e) (kill-buffer "*mu4e-headers*"))))

(stante-after mu4e-headers
              (add-to-list 'mu4e-headers-actions '("pdf view" . mu4e-action-view-as-pdf))
              (add-to-list 'mu4e-headers-actions '("add contact"  . mu4e-action-add-org-contact)))

(stante-after mu4e-headers
              (add-hook 'mu4e-headers-mode-hook (lambda ()
                                                  (setq-local show-trailing-whitespace nil)
                                                  ;; (when (not (string= (system-name) "uinpc4"))
                                                  ;;   (text-scale-increase 1))
                                                  (modal-mode 1)
                                                  (if (equal window-system 'x)
                                                      (progn
                                                        (set-fontset-font "fontset-default" 'unicode "Dejavu Sans Mono")
                                                        (set-face-font 'default "Inconsolata-12"))))))

(stante-after mu4e-view
              (setq mu4e-view-fields ;; Header fields to display in the message view buffer.
                    '(:from :to :cc :subject :flags :date :maildir :mailing-list :tags :attachments :signature :decryption)
                    mu4e-view-show-addresses t ;; show full addresses in view message (instead of just names)
                    mml2015-use 'epg
                    mu4e-view-prefer-html t ))

(stante-after mu4e-view

              ;; (define-key mu4e-view-mode-map "j" 'scroll-up-line)
              ;; (define-key mu4e-view-mode-map "k" 'scroll-down-line)

              ;; (define-key mu4e-view-mode-map (kbd "n") 'csb/jump-down)
              ;; (define-key mu4e-view-mode-map (kbd "p") 'csb/jump-up)

              (define-key mu4e-view-mode-map (kbd "f") #'mu4e-view-headers-next)
              (define-key mu4e-view-mode-map (kbd "b") #'mu4e-view-headers-prev)

              (define-key mu4e-view-mode-map (kbd "J") #'mu4e-view-headers-next)
              (define-key mu4e-view-mode-map (kbd "K") #'mu4e-view-headers-prev)

              (define-key mu4e-view-mode-map (kbd "<") #'beginning-of-buffer)
              (define-key mu4e-view-mode-map (kbd ">") #'end-of-buffer))

(stante-after mu4e-view
              (define-key mu4e-view-mode-map "e" 'mu4e-view-save-attachment)
              (define-key mu4e-view-mode-map "o" 'mu4e-view-open-attachment)
              (define-key mu4e-view-mode-map "A" 'mu4e-view-attachment-action))

(stante-after mu4e-view
              ;; Sync
              (define-key mu4e-view-mode-map "$" #'csb/mu4e-sync-with-server)
              (define-key mu4e-view-mode-map "S" #'mu4e-update-mail-and-index))

(stante-after mu4e-view
              ;; Zoom
              (define-key mu4e-view-mode-map (kbd "C-+") (lambda() (interactive) (text-scale-increase 0.5)))
              (define-key mu4e-view-mode-map (kbd "C--") (lambda() (interactive) (text-scale-decrease 0.5)))
              ;; quit
              (define-key mu4e-view-mode-map (kbd "h") #'(lambda()(interactive)
                                                           (mu4e~view-quit-buffer)
                                                           (switch-to-buffer "*mu4e-headers*"))))

(stante-after mu4e-view
              (setq mu4e-view-actions '(("capture message" . mu4e-action-capture-message)))
              (add-to-list 'mu4e-view-actions '("pdf view" . mu4e-action-view-as-pdf))
              (add-to-list 'mu4e-view-actions '("browse externally" . mu4e-action-view-in-browser))
              (add-to-list 'mu4e-view-actions '("add contact"  . mu4e-action-add-org-contact)))

(stante-after mu4e-view
              (add-hook 'mu4e-view-mode-hook (lambda ()
                                               (setq-local show-trailing-whitespace nil)
                                               (csb/mode-line-off)
                                               (modal-mode 1))))

(stante-after mu4e
              (add-hook 'message-send-mail-hook 'mu4e-multi-smtpmail-set-msmtp-account))

(stante-after mu4e
              (require 'org-mu4e)
              (setq org-mu4e-convert-to-html t)
              (defalias 'org-mail 'org-mu4e-compose-org-mode))

(stante-after mu4e

              (defun org~mu4e-mime-switch-headers-or-body ()
                "Switch the buffer to either mu4e-compose-mode (when in headers)
or org-mode (when in the body)."
                (interactive)
                (let* ((sepapoint
                        (save-excursion
                          (goto-char (point-min))
                          (search-forward-regexp mail-header-separator nil t))))
                  ;; only do stuff when the sepapoint exist; note that after sending the
                  ;; message, this function maybe called on a message with the sepapoint
                  ;; stripped. This is why we don't use `message-point-in-header'.
                  (when sepapoint
                    (cond
                     ;; we're in the body, but in mu4e-compose-mode?
                     ;; if so, switch to org-mode
                     ((and (> (point) sepapoint) (eq major-mode 'mu4e-compose-mode))
                      (org-mode)
                      ;; csb
                      ;; (add-hook 'before-save-hook
                      ;;        (lambda ()
                      ;;          (mu4e-error "Switch to mu4e-compose-mode (M-m) before saving."))
                      ;;        nil t)
                      (org~mu4e-mime-decorate-headers)
                      (local-set-key (kbd "M-m")
                                     (lambda (keyseq)
                                       (interactive "kEnter mu4e-compose-mode key sequence: ")
                                       (let ((func (lookup-key mu4e-compose-mode-map keyseq)))
                                         (if func (funcall func) (insert keyseq))))))
                     ;; we're in the headers, but in org-mode?
                     ;; if so, switch to mu4e-compose-mode
                     ((and (<= (point) sepapoint) (eq major-mode 'org-mode))
                      (org~mu4e-mime-undecorate-headers)
                      (mu4e-compose-mode)
                      (add-hook 'message-send-hook 'org~mu4e-mime-convert-to-html-maybe nil t)
                      ;; csb
                      (mu4e-multi-compose-set-account (csb/mu4e-guess-current-account))))
                    ;; and add the hook
                    (add-hook 'post-command-hook 'org~mu4e-mime-switch-headers-or-body t t)))))

(stante-after mu4e

              (mu4e-maildirs-extension)

              (setq mu4e-maildirs-extension-custom-list '( "/inventati/inbox"
                                                           "/inventati/perso/*"
                                                           "/ciemat/inbox"
                                                           "/cern/inbox"
                                                           "/iphc/inbox"
                                                           "/curro/inbox"
                                                           "/otra/inbox"
                                                           "/reyes/inbox")
                    mu4e-maildirs-extension-action-text nil
                    mu4e-maildirs-extension-propertize-func
                    'my/mu4e-maildirs-extension-propertize-unread-only)

              (defun my/mu4e-maildirs-extension-propertize-unread-only (item)
                "Propertize only the maildir unread count using ITEM plist."
                (format "%s\t%s%s %s (%s/%s)\n"
                        (if (equal (plist-get item :level) 0) "\n" "")
                        (plist-get item :indent)
                        (plist-get item :separator)
                        (plist-get item :name)
                        (propertize (number-to-string (plist-get item :unread))
                                    'face (cond
                                           ((> (plist-get item :unread) 0) 'mu4e-maildirs-extension-maildir-unread-face)
                                           (t            'mu4e-maildirs-extension-maildir-face)))
                        (plist-get item :total))))

(stante-after mu4e
              (add-to-list 'load-path (concat user-projects-directory "mu4e-multi"))
              (require 'mu4e-multi)
              (setq mu4e-multi-account-alist
                    `(("inventati"
                       (user-full-name . "Cayetano Santos")
                       (user-mail-address . ,(password-store-get "mail/mypasswd" "from_inventati:"))
                       (mu4e-compose-reply-to-address . ,(password-store-get "mail/mypasswd" "reply_to_inventati:"))
                       (smtpmail-mail-address . ,(password-store-get "mail/mypasswd" "from_inventati:"))
                       (smtpmail-stream-type . starttls) ;; ssl, plain, nil
                       (smtpmail-smtp-server . ,(password-store-get "mail/mypasswd" "imap_server_inventati:"))
                       (smtpmail-smtp-service . ,(password-store-get "mail/mypasswd" "imap_server_ciemat:"))
                       (smtpmail-smtp-user . ,(password-store-get "mail/mypasswd" "imap_user_inventati:"))
                       (mu4e-compose-signature . "inventati")
                       (message-signature . t)
                       (message-signature-file . "~/.mutt/signature-inventati")
                       (mu4e-sent-folder . "/inventati/sent")
                       (mu4e-drafts-folder . "/inventati/drafts")
                       (mu4e-refile-folder . "/inventati/archive")
                       (mu4e-trash-folder . "/inventati/trash"))
                      ("ciemat"
                       (user-full-name . "Cayetano Santos")
                       (user-mail-address . ,(password-store-get "mail/mypasswd" "from_ciemat:"))
                       (mu4e-compose-reply-to-address . ,(password-store-get "mail/mypasswd" "reply_to_ciemat:"))
                       (smtpmail-mail-address . ,(password-store-get "mail/mypasswd" "from_ciemat:"))
                       (smtpmail-stream-type . starttls) ;; ssl, plain, nil
                       (smtpmail-smtp-server . ,(password-store-get "mail/mypasswd" "imap_server_ciemat:"))
                       (smtpmail-smtp-service . ,(password-store-get "mail/mypasswd" "imap_port_ciemat:"))
                       (smtpmail-smtp-user . ,(password-store-get "mail/mypasswd" "imap_user_ciemat:"))
                       (mu4e-compose-signature . "ciemat")
                       (message-signature . t)
                       (message-signature-file . "~/.mutt/signature-ciemat")
                       (mu4e-sent-folder . "/ciemat/sent")
                       (mu4e-drafts-folder . "/ciemat/drafts")
                       (mu4e-refile-folder . "/ciemat/archive")
                       (mu4e-trash-folder . "/ciemat/trash"))))
              (mu4e-multi-enable))

(stante-after mu4e
              (add-to-list 'load-path (concat user-projects-directory "helm-mu"))
              (require 'helm-mu)
              (defalias 'helm-mu-compose-mail 'csb/mu4e-compose-mail)
              (setq helm-mu-default-search-string ""  ;; A default search string for new searches
                    helm-mu-contacts-name-colwidth 40 ;; The width of the column showing names when searching contacts.
                    helm-mu-contacts-after "01-Jan-1970 00:00:00"  ;; Only show contacts from mails received after that time.
                    helm-mu-contacts-personal nil )
              (define-key helm-command-map (kbd "4") 'helm-mu))

(stante-after mu4e
              ;; preference for a mail composition package.
              (setq mail-user-agent 'message-user-agent)  ;; use the message package.
              (setq compose-mail-user-agent-warnings t))

(stante-after mu4e
              ;; Function to call to send the current buffer as mail
              (setq send-mail-function 'message-send-mail-with-sendmail)
              ;; (setq send-mail-function 'smtpmail-send-it)
              ;; (setq send-mail-function 'sendmail-query-once)
              ;;
              (setq smtpmail-debug-info t)
              (setq smtpmail-debug-verb t))

(stante-after mu4e
              (setq smtpmail-queue-mail nil)
              (setq smtpmail-queue-dir "~/Maildir/inventati/queued-mail/"))

(stante-after mu4e
              (setq message-kill-buffer-on-exit t) ;; don't keep message buffers around
              (setq message-confirm-send nil)
              ;; (setq message-default-headers "Reply-to: foo@example.com\nFCC: ~/Mail/sent") ;; Header lines to be inserted in outgoing messages.
              ;; when using smtp
              ;; (setq message-send-mail-function 'smtpmail-send-it)
              ;;
              ;; when using msmtp/sendmail
              ;; tell msmtp to choose the SMTP server according to the from field in the outgoing email
              ;; (setq message-sendmail-extra-arguments '("--read-envelope-from"))
              ;; (setq message-sendmail-f-is-evil t)
              (setq message-send-mail-function 'message-send-mail-with-sendmail)
              (setq message-from-style 'angles)              ;; Specifies how "From:" fields look.
              ;; (setq message-send-mail-partially-limit nil)
              ;; (setq message-sendmail-envelope-from 'header)  ;; Envelope-from when sending mail with sendmail
              )

(stante-after message
              (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime))

(stante-after jl-encrypt
              (setq jl-encrypt-insert-signature 'always)
              )

(stante-after mu4e
              (setq sendmail-program (executable-find "msmtp")))

(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))
;; (setq auto-mode-alist (append '(("/tmp/mutt.*" . message-mode)) auto-mode-alist))

(add-hook 'mail-mode-hook (lambda ()
                            (auto-fill-mode)
                            (highlight-changes-mode 0)
                            (turn-on-auto-fill)
                            (ispell-minor-mode)
                            (flyspell-mode 1)
                            (mail-abbrevs-setup)
                            (csb/spell-switch-dictionary-french)))
;; ;; (my_spell_local)
;; ;; (local-set-key "\C-Xk" 'server-edit)
;; (turn-on-orgstruct++)
;; ;; (auto-complete)
;; ;; (auto-complete-mode 1)
;; ;; (turn-on-predictive-mode)
;; ;; (setq auto-completion-min-chars (quote ( . 3)))
;; ;; (setq completion-auto-show-delay (quote ( . 0.5)))
;; ;; (setq auto-completion-delay (quote ( . 0.5)))
;; (setq-local orgstruct-heading-prefix-regexp " *## ")
;; (add-to-list 'imenu-generic-expression '(nil  "^## *\\(.*\\)" 1))

(provide 'csb-mail)

;; csb-mail.el ends here
