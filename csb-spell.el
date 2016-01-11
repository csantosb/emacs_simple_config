;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(setq ispell-program-name
      (cond
       ((executable-find "aspell")
        "/usr/sbin/aspell")
       ((executable-find "hunspell")
        "/usr/sbin/hunspell")))

(stante-after ispell
              (setq ispell-dictionary "french"        ;; Default dictionary to use if ‘ispell-local-dictionary’ is nil.
                    ;;    ispell-local-dictionary "french"  ;; nil, which means use the global setting in ‘ispell-dictionary’
                    ;; same effect as calling M-x ispell-change-dictionary
                    ))

(defun csb/spell-switch-dictionary(dictionary)
  (ispell-change-dictionary dictionary) ;; sets ‘ispell-local-dictionary’
  (setq-local ispell-personal-dictionary (format "~/Dropbox/.ispell_%s" dictionary)))

(defun csb/spell-switch-dictionary-spanish()
  (interactive)
  (csb/spell-switch-dictionary "spanish")
  (setq ispell-extra-args
        (cond
         ((executable-find "aspell")
          '("--sug-mode=ultra" "--lang=es_ES.UTF-8" "--run-together" "--run-together-limit=5" "--run-together-min=3") )
         ((executable-find "hunspell")
          '("-d es_ES"))))
  (message "Dictionary switched to spanish"))

(defun csb/spell-switch-dictionary-english()
  (interactive)
  (csb/spell-switch-dictionary "english")
  (setq ispell-extra-args
        (cond
         ((executable-find "aspell")
          '("--sug-mode=ultra" "--lang=en_GB.UTF-8" "--run-together" "--run-together-limit=5" "--run-together-min=3") )
         ((executable-find "hunspell")
          '("-d en_GB"))))
  (message "Dictionary switched to english"))

(defun csb/spell-switch-dictionary-french()
  (interactive)
  (csb/spell-switch-dictionary "french")
  (setq ispell-extra-args
        (cond
         ((executable-find "aspell")
          '("--sug-mode=ultra" "--lang=fr_FR.UTF-8" "--run-together" "--run-together-limit=5" "--run-together-min=3") )
         ((executable-find "hunspell")
          '("-d fr_FR"))))
  (message "Dictionary switched to french"))

(stante-after ispell

     ;; release keys to avoid conflicts
     ;; (define-key ispell-minor-keymap (kbd "SPC") nil)
     ;; (define-key ispell-minor-keymap (kbd "RET") nil)
     ;; (global-set-key (kbd "M-s o") nil)

     ;; Ispell
     (define-key launcher-map "ib" #'ispell-buffer)
     (define-key launcher-map "iw" #'ispell-word)
     (define-key launcher-map "ir" #'ispell-region)
     (define-key launcher-map "im" #'ispell-message)
     (define-key launcher-map "ik" #'ispell-kill-ispell)
     (define-key launcher-map "ic" #'ispell-continue)

     (define-key launcher-map "is" #'csb/spell-switch-dictionary-spanish)
     (define-key launcher-map "if" #'csb/spell-switch-dictionary-french)
     (define-key launcher-map "ie" #'csb/spell-switch-dictionary-english)

     (define-key ispell-minor-keymap (kbd "M-s s f") 'search-word-forward)
     (define-key ispell-minor-keymap (kbd "M-s s b") 'search-word-backward))

(stante-after ispell
  (defun endless/org-ispell ()
    "Configure `ispell-skip-region-alist' for `org-mode'."
    (make-local-variable 'ispell-skip-region-alist)
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
    (add-to-list 'ispell-skip-region-alist '("~" "~"))
    (add-to-list 'ispell-skip-region-alist '("=" "="))
    (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))))

(add-hook 'org-mode-hook #'endless/org-ispell)

(define-key ctl-x-map "\C-i" #'endless/ispell-word-then-abbrev)

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word)
                           "")))
        aft)
    (call-interactively 'ispell-word)
    (setq aft (downcase
               (or (thing-at-point 'word) "")))
    (unless (or (string= aft bef)
                (string= aft "")
                (string= bef ""))
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table)
        bef aft))))

(setq save-abbrevs t)
(setq-default abbrev-mode t)

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  ;; (flyspell-auto-correct-previous-word (point))
  (flyspell-auto-correct-word))

(defun flyspell-check-previous-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-check-previous-highlighted-word)
  ;;(flyspell-goto-previous-error)
  ;;(flyspell-word)
 )

(stante-after flyspell
     ;; flyspell-auto-correct-previous-hook
     ;; flyspell-check-next-highlighted-word
     ;; flyspell-check-previous-highlighted-word
     ;; flyspell-correct-word
     ;; flyspell-correct-word-before-point
     ;; flyspell-delay-command
     ;; flyspell-deplacement-command
     ;; flyspell-post-command-hook
     ;; flyspell-pre-command-hook

     ;; release keys to avoid conflicts
     ;; (define-key flyspell-mode-map (kbd "C-M-i") nil)
     ;; (define-key flyspell-mode-map (kbd "C-,") nil)
     ;; (define-key flyspell-mode-map (kbd "C-.") nil)
     ;; (define-key flyspell-mode-map (kbd "C-;") nil)
     ;; (define-key flyspell-mode-map (kbd "C-c $") nil)

     ;; declare keys
     (define-key launcher-map "fa" #'flyspell-auto-correct-word)
     (define-key launcher-map "fp" #'flyspell-auto-correct-previous-word)
     (define-key launcher-map "fb" #'flyspell-buffer)
     (define-key launcher-map "fr" #'flyspell-region)
     (define-key launcher-map "fw" #'flyspell-word)
     ;; (define-key launcher-map "fn" #'flyspell-check-next-highlighted-word)
     ;; (define-key launcher-map "fp" #'flyspell-check-previous-highlighted-word)
     (define-key launcher-map "fh" #'helm-flyspell-correct)
     (define-key launcher-map "fe" #'endless/ispell-word-then-abbrev))

(global-flycheck-mode 1)

(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c &"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
  (define-key flycheck-command-map "b" #'flycheck-buffer)
  (define-key flycheck-command-map "c" nil)
  (define-key flycheck-command-map "h" #'helm-flycheck)
  (define-key flycheck-command-map "/" nil)
  (define-key flycheck-command-map "g" #'flycheck-google-messages))

(with-eval-after-load 'flycheck
  (setq flycheck-idle-change-delay 0
        flycheck-completion-system 'helm-comp-read
        flycheck-mode-line
        '(:eval
          (pcase flycheck-last-status-change
            (`not-checked nil)
            (`no-checker (propertize " -" 'face 'warning))
            (`running (propertize " ✷" 'face 'success))
            (`errored (propertize " !" 'face 'error))
            (`finished
             (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                    (no-errors (cdr (assq 'error error-counts)))
                    (no-warnings (cdr (assq 'warning error-counts)))
                    (face (cond (no-errors 'error)
                                (no-warnings 'warning)
                                (t 'success))))
               (propertize (format " %s/%s" (or no-errors 0) (or no-warnings 0))
                           'face face)))
            (`interrupted " -")
            (`suspicious '(propertize " ?" 'face 'warning))))))

;; (define-key endless/toggle-map "f" 'flycheck-mode)

(with-eval-after-load 'flycheck
  (define-key flycheck-command-map "d" #'endless/flycheck-dir)

  (defun endless/flycheck-dir (dir)
    "Run flycheck for each file in current directory.
  Results are reported in a compilation buffer."
    (interactive "DDirectory: ")
    (displaying-byte-compile-warnings
     (let ((p nil))
       (with-current-buffer (get-buffer-create
                             byte-compile-log-buffer)
         (setq default-directory dir)
         (unless (eq major-mode 'compilation-mode)
           (compilation-mode))
         (goto-char (point-max))
         (let ((inhibit-read-only t))
           (insert "\n\xc\n\n"))
         (setq p (point)))
       (dolist (file (directory-files "./" nil
                                      "\\`[^\\.].*\\'"))
         (endless/-flycheck-file file))
       (with-selected-window (display-buffer
                              byte-compile-log-buffer)
         (goto-char p)
         (recenter 1)))))

  (defun endless/-report-error (fmt &rest args)
    "Print an error on `byte-compile-log-buffer'."
    (let ((inhibit-read-only t)
          (fill-prefix "    "))
      (with-current-buffer byte-compile-log-buffer
        (let ((l (point)))
          (insert "\n" (apply #'format fmt args))
          (fill-region (1+ l) (point))))))

  (defun endless/-flycheck-file (file)
    "Check FILE and report to `byte-compile-log-buffer'."
    (let ((was-visited (find-buffer-visiting file)))
      (with-current-buffer (or was-visited
                               (progn (find-file file)
                                      (current-buffer)))
        (when (ignore-errors (flycheck-buffer))
          (while (flycheck-running-p)
            (accept-process-output nil 0.1))
          (pcase flycheck-last-status-change
            ((or `errored `suspicious)
             (endless/-report-error
              "%s: Something wrong here!"
              (file-name-nondirectory (buffer-file-name))))
            (`finished
             (dolist (e flycheck-current-errors)
               (endless/-report-error
                "%s:%s:%s:%s: %s"
                (file-name-nondirectory (buffer-file-name))
                (flycheck-error-line e)
                (flycheck-error-column e)
                (flycheck-error-level e)
                (flycheck-error-message e))))))
        (if was-visited
            (bury-buffer was-visited)
          (kill-buffer (current-buffer)))))))

(with-eval-after-load 'flycheck
  (require 'flycheck-color-mode-line)
  (add-hook 'flycheck-mode-hook
            'flycheck-color-mode-line-mode))

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

(with-eval-after-load 'flycheck
  '(flycheck-package-setup))



(provide 'csb-spell)
;;; csb-spell.el ends here
