;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains defaults for variables as well as global keystrokes
;;; Code:

(defun with-initial-minibuffer (str)
    (interactive)
    (funcall `(lambda ()
                (interactive)
                (minibuffer-with-setup-hook
                    (lambda () (insert (format "%s " ,str)))
                  (call-interactively 'helm-M-x)))))

(global-set-key [remap goto-line] 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (setq nlinum-format "%d ")
        (goto-line (read-number "Goto line: ")))
    (nlinum-mode nil)))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)
(global-set-key [remap org-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun csb/new-empty-buffer ()
  "Open a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and 'emacs-lisp-mode))
    (setq buffer-offer-save t)))
(global-set-key (kbd "C-x C-n") 'csb/new-empty-buffer)

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(defun font-lock-doc-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(DOC\\(ME\\)?\\|DOCME\\):"
          1 font-lock-doc-face t))))

(defun csb/kill-symbol-at-point()
  (interactive)
  (setq current-point (point))
  (setq bds (bounds-of-thing-at-point 'symbol))
  (setq p1 (car bds) )
  (setq p2 (cdr bds) )
  (copy-region-as-kill p1 p2)
  ;; (setq csb/kill-symbol-at-point-thing (buffer-substring-no-properties p1 p2))
  (goto-char current-point))
(global-set-key (kbd "C-x M-k") 'csb/kill-symbol-at-point)

(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

(defun make-repeatable-command (cmd)
  "Returns a new command that is a repeatable version of CMD.
The new command is named CMD-repeat.  CMD should be a quoted
command.

This allows you to bind the command to a compound keystroke and
repeat it with just the final key.  For example:

  (global-set-key (kbd \"C-c a\") (make-repeatable-command 'foo))

will create a new command called foo-repeat.  Typing C-c a will
just invoke foo.  Typing C-c a a a will invoke foo three times,
and so on."
  (fset (intern (concat (symbol-name cmd) "-repeat"))
        `(lambda ,(help-function-arglist cmd) ;; arg list
           ,(format "A repeatable version of `%s'." (symbol-name cmd)) ;; doc string
           ,(interactive-form cmd) ;; interactive form
           ;; see also repeat-message-function
           (setq last-repeatable-command ',cmd)
           (repeat nil)))
  (intern (concat (symbol-name cmd) "-repeat")))

(defun find-user-init-file-other ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (persp-switch "wikidata")
  (find-file-other-window (concat user-projects-directory "perso/wikidata/emacs.cat/org-config.cat/csb-config.page")))

(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (persp-switch "wikidata")
  (find-file (concat user-projects-directory "perso/wikidata/emacs.cat/org-config.cat/csb-config.page")))

(global-set-key (kbd "<f1>") 'find-user-init-file)
(global-set-key (kbd "<f2>") 'find-user-init-file-other)

(global-set-key (kbd "<f3>")
                #'(lambda ()
                    (interactive)
                    (cond ((string= major-mode 'cfw:calendar-mode)
                           (find-file (concat org-config-path
                                              "csb-calendar.page")))
                          ((string= major-mode 'vhdl-mode)
                           (find-file (concat org-config-path
                                              "csb-vhdl.page")))
                          ((string= major-mode 'org-mode)
                           (find-file (concat org-config-path
                                              "csb-orgmode.page")))
                          ((string= major-mode 'matlab-mode)
                           (find-file (concat org-config-path
                                              "csb-matlab.page")))
                          ((string= major-mode 'eww-mode)
                           (find-file (concat org-config-path
                                              "csb-www.page")))
                          ((string= major-mode 'python-mode)
                           (find-file (concat org-config-path
                                              "csb-python.page")))
                          ((string-prefix-p "mu4e" (symbol-name major-mode))
                           (find-file (concat org-config-path
                                              "csb-mail.page")))
                          ((string= major-mode 'elfeed-search-mode)
                           (find-file (concat org-config-path
                                              "csb-elfeed.page")))
                          ((string= major-mode 'matlab-mode)
                           (find-file (concat org-config-path
                                              "csb-matlab.page")))
                          ((derived-mode-p 'c-mode 'c++-mode)
                           (find-file (concat org-config-path
                                              "csb-c-mode.page")))
                          (t nil))
                    (csb/toggle-fringes)))

(global-set-key (kbd "<f4>")
                #'(lambda ()
                    (interactive)
                    (cond ((string= major-mode 'cfw:calendar-mode)
                           (find-file-other-window (concat org-config-path
                                                           "csb-calendar.page")))
                          ((string= major-mode 'org-mode)
                           (find-file-other-window (concat org-config-path
                                                           "csb-orgmode.page")))
                          ((string= major-mode 'eww-mode)
                           (find-file-other-window (concat org-config-path
                                                           "csb-www.page")))
                          ((string= major-mode 'vhdl-mode)
                           (find-file-other-window (concat org-config-path
                                                           "csb-vhdl.page")))
                          ((string= major-mode 'matlab-mode)
                           (find-file-other-window (concat org-config-path
                                                           "csb-matlab.page")))
                          ((string= major-mode 'eww-mode)
                           (find-file-other-window (concat org-config-path
                                                           "csb-www.page")))
                          ((string= major-mode 'python-mode)
                           (find-file-other-window (concat org-config-path
                                                           "csb-python.page")))
                          ((string-prefix-p "mu4e" (symbol-name major-mode))
                           (find-file-other-window (concat org-config-path
                                                           "csb-mail.page")))
                          ((string= major-mode 'elfeed-search-mode)
                           (find-file-other-window (concat org-config-path
                                                           "csb-elfeed.page")))
                          ((string= major-mode 'matlab-mode)
                           (find-file-other-window (concat org-config-path
                                                           "csb-matlab.page")))
                          ((derived-mode-p 'c-mode 'c++-mode)
                           (find-file-other-window (concat org-config-path
                                                           "csb-c-mode.page")))
                          (t nil))
                    (csb/toggle-fringes)))

;; (defmacro set-config-file (filename)
;;   (local-set-key (kbd "<f3>")
;;                  `(lambda()
;;                     (interactive)
;;                     (find-file-other-window (concat org-config-path ,filename))))
;;   (local-set-key (kbd "<f4>")
;;                  `(lambda()
;;                     (interactive)
;;                     (find-file (concat org-config-path ,filename)))))

;; (defmacro find-config-file-other-window (filename)
;;   "Edit the elfeed-mode config file, in another window."
;;   (interactive)
;;   (find-file-other-window (concat org-config-path filename)))

;; (defmacro find-config-file (filename)
;;   "Edit the elfeed-mode config file, in another window."
;;   (interactive)
;;   (find-file (concat org-config-path filename)))

(defun endless/round-quotes (italicize)
  "Insert “” and leave point in the middle.
  With prefix argument ITALICIZE, insert /“”/ instead
  \(meant for org-mode).
  Inside a code-block, just call `self-insert-command'."
  (interactive "P")
  (if (and (derived-mode-p 'org-mode)
	   (org-in-block-p '("src" "latex" "html")))
      (call-interactively #'self-insert-command)
    (if (looking-at "”[/=_\\*]?")
	(goto-char (match-end 0))
      (when italicize
	(if (derived-mode-p 'markdown-mode)
	    (insert "__")
	  (insert "//"))
	(forward-char -1))
      (insert "“”")
      (forward-char -1))))

(with-eval-after-load 'org
  (define-key org-mode-map "\"" #'endless/round-quotes))

;; (eval-after-load 'markdown-mode
;;   '(define-key markdown-mode-map "\""
;;      #'endless/round-quotes))

(defun endless/apostrophe (opening)
  "Insert ’ in prose or `self-insert-command' in code.
With prefix argument OPENING, insert ‘’ instead and
leave point in the middle.
Inside a code-block, just call `self-insert-command'."
  (interactive "P")
  (if (and (derived-mode-p 'org-mode)
           (org-in-block-p '("src" "latex" "html")))
      (call-interactively #'self-insert-command)
    (if (looking-at "['’][=_/\\*]?")
        (goto-char (match-end 0))
      (if (null opening)
          (insert "’")
        (insert "‘’")
        (forward-char -1)))))

(with-eval-after-load 'org
  (define-key org-mode-map "'" #'endless/apostrophe))

;; (eval-after-load 'markdown-mode
;;   '(define-key markdown-mode-map "'"
;;      #'endless/apostrophe))

(provide 'csb-miscellaneous)
;;; csb-miscellaneous.el ends here
