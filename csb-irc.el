;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

;; (autoload 'rcirc "rcirc.el" "Connect to IRC." t)
(with-eval-after-load 'rcirc
  (require 'rcirc-help)
  (require 'rcirc-nonames-on-join)
  (require 'rcirc-alert)
  (rename-modeline "rcirc" rcirc-mode "IRC"))

(with-eval-after-load 'rcirc
  (setq-default rcirc-auto-authenticate-flag t)
  (setq-default rcirc-timeout-seconds 36000))

(with-eval-after-load 'rcirc
  (defun rcirc-handler-004 (process sender args text)
    (rcirc-handler-generic process "004" sender args text)
    (let ((freenode-password (password-store-get "www/freenode"))
          (freenode-user (password-store-get "www/freenode" "user:")))
      (when (string= rcirc-nick "csantosb`")
        (rcirc-send-message process "nickserv" (concat "recover " freenode-user " " freenode-password))
        (rcirc-send-message process "nickserv" (concat "recover " freenode-user " " freenode-password))
        (sit-for 1)
        (rcirc-send-string process (concat "NICK " freenode-user))))))

(with-eval-after-load 'rcirc
  (defun occur-irc (regexp)
    "Run `multi-occur' for all buffers in `rcirc-mode'."
    (interactive "sList lines matching regexp: ")
    (multi-occur (let (result)
                   (dolist (buf (buffer-list))
                     (with-current-buffer buf
                       (when (eq major-mode 'rcirc-mode)
                         (setq result (cons buf result)))))
                   result) regexp)))

(with-eval-after-load 'rcirc
  '(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
            (port (process-contact process :service))
            (nick (rcirc-nick process))
            channels query-buffers)
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (eq process (rcirc-buffer-process))
             (remove-hook 'change-major-mode-hook
                          'rcirc-change-major-mode-hook)
             (if (rcirc-channel-p rcirc-target)
                 (setq channels (cons rcirc-target channels))
               (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
                      rcirc-default-user-name
                      rcirc-default-full-name
                      channels))))

(defun my-irc ()
  "Launch IRC."
  (interactive)
  ;; if secure box is open to get credentials from the password store, proceed
  (cond ((file-exists-p "~/sb")
         ;; change perspective
         (persp-switch "freenode")
         ;; if already running, not launch it again
         (unless (and (get-buffer "*irc.freenode.net*") (get-buffer "#emacs@irc.freenode.net"))
           (let* ((freenode-user (password-store-get "www/freenode" "user:"))
                  (freenode-password (password-store-get "www/freenode"))
                  (freenode-channels '("#emacs" "#archlinux" "#org-mode"))
                  (freenode-port 6697)
                  ;; server config
                  ;; ref: https://stackoverflow.com/questions/1664202/emacs-lisp-evaluate-variable-in-alist
                  (rcirc-server-freenode `("irc.freenode.net"
                                           :channels ,freenode-channels
                                           :encryption tls
                                           :nick ,freenode-user
                                           :port ,freenode-port
                                           :user-name ,freenode-user
                                           :full-name ,freenode-user))
                  (rcirc-server-alist (list rcirc-server-freenode))
                  ;; credentials
                  (rcirc-authinfo-freenode (list "freenode"
                                                 'nickserv
                                                 freenode-user
                                                 freenode-password))
                  (rcirc-authinfo (list rcirc-authinfo-freenode)))
             (irc nil)
             (sit-for 5)))
         ;; once launched, setup windows
         (persp-switch "freenode")
         ;; add buffers to this perspective
         (persp-set-buffer "*irc.freenode.net*")
         (persp-set-buffer "#emacs@irc.freenode.net")
         (persp-set-buffer "#archlinux@irc.freenode.net")
         (persp-set-buffer "#org-mode@irc.freenode.net")
         ;; do a 2x2 tile
         (split-window-multiple-ways 2 2)
         (set-window-buffer-in-frame 1 0 (get-buffer "#emacs@irc.freenode.net"))
         (set-window-buffer-in-frame 1 1  (get-buffer "#archlinux@irc.freenode.net"))
         (set-window-buffer-in-frame 0 0 (get-buffer "#org-mode@irc.freenode.net"))
         ;; set layout
         (select-window-2)
         (delete-window)
         (csb/set-tile 0))
        (t (message "NO GPG KEY FOUND, SB OPEN??"))))

(stante-after rcirc
              (define-key rcirc-mode-map (kbd "G") 'end-of-buffer)
              (define-key rcirc-mode-map (kbd "M->") nil))

(stante-after rcirc-alert

              ;; Enable / disable notifications
              (setq-default rcirc-enable-alert-nick t)
              (setq-default rcirc-enable-alert-keywords t)
              (setq-default rcirc-enable-alert-always nil)

              ;; Alert library triggers when one of these words appear
              (setq-default rcirc-alert-keywords nil)

              ;; Alert library triggers the nick status change on these nicks
              (setq-default rcirc-alert-nicks nil)

              ;; Dim some nicks
              (setq-default rcirc-dim-nicks nil)

              ;; Bright the list of contacts for any channel
              (setq-default rcirc-bright-nicks nil)

              ;; Omit mode
              (setq-default rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
              (setq-default rcirc-omit-threshold' 100)

              ;; Notification timeout
              (setq-default rcirc-alert-timeout 5)

              ;; Hide trailing white space
              (setq-default show-trailing-whitespace nil)

              ;; Alert library uses this script
              (setq-default my-rcirc-notification-script "~/Projects/rcirc-alert/rcirc-alert.sh"))

(add-hook 'rcirc-mode-hook
          (lambda ()
            (rcirc-track-minor-mode 1)
            (setq-local focus-out-hook nil)
            (rcirc-omit-mode)
            (read-only-mode 1)
            (flyspell-mode 1)
            (smartscan-mode 0)
            (text-scale-decrease 2)
            (set (make-local-variable 'scroll-conservatively) 8192)))

(add-hook 'rcirc-mode-hook
          #'(lambda()
              (when (string= (substring (buffer-name) 0 (string-match-p (regexp-quote "@") (buffer-name))) "#twitter_csantosb")
                ;; Enable / disable notifications
                (setq-local rcirc-enable-alert-nick nil)
                (setq-local rcirc-enable-alert-keywords nil)
                (setq-local rcirc-enable-alert-always t)
                (setq-local rcirc-alert-message-always "%s:  %s")
                ;; Alert library triggers when one of these words appear
                (setq-local  rcirc-alert-keywords '("XilinxInc" "realmurciacfsad" "TEDNews"))
                ;; Alert library triggers the nick status change on these nicks
                (setq-local rcirc-alert-nicks '("XilinxInc" "realmurciacfsad" "nicferrier" "jasonwryan" "TEDNews" "melpa_emacs" "Pinboard"))
                ;; Dim some nicks
                (setq-local rcirc-dim-nicks nil)
                ;; Bright the list of contacts for any channel
                (setq-local rcirc-bright-nicks rcirc-alert-nicks)
                ;; Omit mode
                (setq-local rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
                (setq-local rcirc-omit-threshold' 100))))

(add-hook 'rcirc-mode-hook
          #'(lambda()
              (when (string= rcirc-target "&bitlbee")
                ;; Enable / disable notifications
                (setq-local rcirc-enable-alert-nick t)
                (setq-local rcirc-enable-alert-keywords t)
                (setq-local rcirc-enable-alert-always nil)
                ;; Alert library triggers when one of these words appear
                (setq-local rcirc-alert-keywords '("toto"))
                ;; Alert library triggers the nick status change on these nicks
                (setq-local rcirc-alert-nicks '("carlosgalanarnillas" "CarmemPrezArellano" "SofiaBartolomeMateos" "karinelaurencont" "cayetanosantos_"))
                ;; Dim some nicks
                (setq-local rcirc-dim-nicks nil)
                ;; Bright the list of contacts for any channel
                (setq-local rcirc-bright-nicks rcirc-alert-nicks)
                ;; Omit mode
                (setq-local rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
                (setq-local rcirc-omit-threshold' 100))))

(add-hook 'rcirc-mode-hook
          #'(lambda()
              (when (string= rcirc-target "#emacs")
                ;; Enable / disable notifications
                (setq-local rcirc-enable-alert-nick t)
                (setq-local rcirc-enable-alert-keywords t)
                (setq-local rcirc-enable-alert-always nil)
                ;; Alert library triggers when one of these words appear
                (setq-local rcirc-alert-keywords '("org-mode"))
                ;; Alert library triggers the nick status change on these nicks
                (setq-local rcirc-alert-nicks '("jasonwryan" "nicferrier" "sachac" "kensanata"))
                ;; Dim some nicks
                (setq-local rcirc-dim-nicks nil)
                ;; Bright the list of contacts for any channel
                (setq-local rcirc-bright-nicks rcirc-alert-nicks)
                ;; Omit mode
                (setq-local rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
                (setq-local rcirc-omit-threshold' 100))))

(add-hook 'rcirc-mode-hook
          #'(lambda()
              (when (string= rcirc-target "#archlinux")
                ;; Enable / disable notifications
                (setq-local rcirc-enable-alert-nick t)
                (setq-local rcirc-enable-alert-keywords t)
                (setq-local rcirc-enable-alert-always nil)
                ;; Alert library triggers when one of these words appear
                (setq-local rcirc-alert-keywords '("luakit"))
                ;; Alert library triggers the nick status change on these nicks
                (setq-local rcirc-alert-nicks '("jasonwryan" "nicferrier" "sachac" "Namarrgon"))
                ;; Dim some nicks
                (setq-local rcirc-dim-nicks nil)
                ;; Bright the list of contacts for any channel
                (setq-local rcirc-bright-nicks rcirc-alert-nicks)
                ;; Omit mode
                (setq-local rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
                (setq-local rcirc-omit-threshold' 100))))

(add-hook 'rcirc-mode-hook
          #'(lambda()
              (when (string= rcirc-target "#org-mode")
                ;; Enable / disable notifications
                (setq-local rcirc-enable-alert-nick t)
                (setq-local rcirc-enable-alert-keywords t)
                (setq-local rcirc-enable-alert-always nil)
                ;; Alert library triggers when one of these words appear
                (setq-local rcirc-alert-keywords '("manual"))
                ;; Alert library triggers the nick status change on these nicks
                (setq-local rcirc-alert-nicks '("jasonwryan" "nicferrier" "sachac" "zeltac"))
                ;; Dim some nicks
                (setq-local rcirc-dim-nicks nil)
                ;; Bright the list of contacts for any channel
                (setq-local rcirc-bright-nicks rcirc-alert-nicks)
                ;; Omit mode
                (setq-local rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
                (setq-local rcirc-omit-threshold' 100))))

(provide 'csb-page)

;;; csb-page.el ends here
