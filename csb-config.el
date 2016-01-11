;;; csb-config --- Summary
;;
;;; Commentary:
;;
;;; Code:
;;

(when running-os-is-linux
  (setq load-prefer-newer t)
  (require 'auto-compile)
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1)
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t))

;; Set user dir containing .emacs.d
(if running-os-is-linux
    (setq-default user-emacs-directory "~/.emacs.d/")
  (setq-default user-emacs-directory "~/.emacs.d/"))

;; Set user init file
(if running-os-is-linux
    (setq-default user-init-file (concat user-emacs-directory "init.el"))
  (setq-default user-init-file (concat user-emacs-directory "init.el")))

;; Set user dir containing "Projects" folder
(if running-os-is-linux
    (setq-default user-projects-directory '"~/Projects/")
  (setq-default user-projects-directory '"~/Projects/"))

;; Set user dir containing "Documents" folder
(if running-os-is-linux
    (setq-default user-documents-directory '"~/Documents/")
  (setq-default user-documents-directory '"~/Documents/"))

;; (add-to-list 'load-path (concat user-emacs-directory "elisp"))
(add-to-list 'load-path (concat user-emacs-directory "libraries/"))
(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

(when running-os-is-linux
  (add-to-list 'load-path "~/Projects/pinboard-api-el/"))

(defun csb/mode-line-off ()
  "switches the mode modeline off"
  (interactive)
  (when (not (equal mode-line-format nil))
    (setq mode-line-format-save mode-line-format)
    (setq mode-line-format nil))
  (redraw-display)
  (force-mode-line-update))

;; Update info path
(when running-os-is-linux
  (add-to-list 'Info-default-directory-list "/home/csantos/Documents/OrgProjects/readability-python-api/python-readability-api/docs/_build/texinfo"))
;;(add-to-list 'Info-directory-list
;;             "/home/csantos/Documents/OrgProjects/readability-python-api/python-readability-api/docs/_build/texinfo")

(add-to-list 'load-path (concat user-projects-directory "perso/helm-perso-wiki"))
(add-to-list 'load-path (concat user-projects-directory "hao-mode"))

;; Environment
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setenv "SHELL" (getenv "SHELL"))

(org-require 'csb-repositories csb/compile-org-require)

(defun csb/toggle-fullscreen ()
    ;; (cond ((and
    ;;         (> (count-windows ) 1)
    ;;         (null (frame-parameter nil 'fullscreen))) ;; not in full screen mode
    ;;        (toggle-frame-fullscreen))
    ;;       ((and
    ;;         (one-window-p)
    ;;         ;; (= (count-windows) 1)
    ;;         (equal (frame-parameter nil 'fullscreen) 'fullboth)) ;; in full screen mode
    ;;        (toggle-frame-fullscreen)))
    )

    (defmacro stante-after (feature &rest forms)
      `(,(if (or (not (boundp 'byte-compile-current-file))
                 (not byte-compile-current-file)
                 (if (symbolp feature)
                     (require feature nil :no-error)
                   (load feature :no-message :no-error)))
             'progn
           (message "stante-after: cannot find %s" feature)
           'with-no-warnings)
        (eval-after-load ',feature
          `(funcall (function ,(lambda () ,@forms))))))

(org-require 'csb-miscellaneous csb/compile-org-require)

(org-require 'csb-orgmode csb/compile-org-require)

(key-chord-mode 1)
;; (key-chord-describe)

(define-prefix-command 'launcher-map)
(define-key ctl-x-map "l" 'launcher-map)

(org-require 'csb-global-togglemap csb/compile-org-require)

;; http://muublog.blogspot.com.es/2014/10/keyboard-activated-favorites-menu-using.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Simple-Menu-Items.html
;; (defvar my-favorties-map (make-sparse-keymap "Favorites"))
;; (define-key global-map (kbd "C-l") my-favorties-map)
;; (define-key my-favorties-map (kbd "f")
;;   (cons "Find file"
;;      'find-file))
;; (define-key my-favorties-map (kbd "p")
;;   (cons "Save current buffer"
;;      'save-buffer))
;; (define-key my-favorties-map (kbd "i")
;;   (cons "Kill buffer"
;;      'kill-buffer))

(global-set-key (kbd "C-SPC") 'set-mark-command)
(transient-mark-mode 1)
(setq set-mark-command-repeat-pop t)
(define-prefix-command 'csb-mark-key)
(global-set-key (kbd "M-m") 'csb-mark-key)
(define-key csb-mark-key (kbd "s") 'mark-sexp)
(define-key csb-mark-key (kbd "d") 'mark-defun)
(define-key csb-mark-key (kbd "p") 'mark-paragraph)
(define-key csb-mark-key (kbd "P") 'mark-page)
(define-key csb-mark-key (kbd "h") 'mark-whole-buffer)

(org-require 'csb-completions csb/compile-org-require)
;; (Setq pos-tip-foreground-color "black")
;; (setq pos-tip-background-color "white")

(global-set-key (kbd "M-f") 'forward-word)
(global-set-key (kbd "M-b") 'backward-word)

(defun xah-forward-block (&optional φn)
  "Move cursor forward to the beginning of next text block.
A text block is separated by blank lines.
In most major modes, this is similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table."
  (interactive "p")
  (search-forward-regexp "\n[\t\n ]*\n+" nil "NOERROR" φn))

(defun xah-backward-block (&optional φn)
  "Move cursor backward to previous text block.
See: `xah-forward-block'"
  (interactive)
  (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR" φn))

(global-set-key (kbd "M-h") 'xah-forward-block)
(define-key org-mode-map (kbd "M-h") nil)
(global-set-key (kbd "M-l") 'xah-backward-block)

(global-set-key (kbd "M-h") #'forward-paragraph)
(global-set-key (kbd "M-l") #'backward-paragraph)

(global-smartscan-mode 1)
(stante-after smartscan
  (define-key smartscan-map (kbd "M-n") 'smartscan-symbol-go-forward)  ;; Forward symbol
  (define-key smartscan-map (kbd "M-p") 'smartscan-symbol-go-backward) ;; Backward symbol
  (define-key smartscan-map (kbd "M-'") 'smartscan-symbol-replace))    ;; Replace symbol

(global-set-key (kbd "M-i") 'avy-goto-word-or-subword-1)

(define-key global-map (kbd "S-<backspace>") 'ace-jump-mode)
(setq ace-jump-mode-gray-background t)
(setq ace-jump-mode-move-keys '(?d ?f ?g ?h ?j ?k ?r ?t ?y ?u ?i))

;; (global-set-key (kbd "C-x b") 'ace-jump-buffer)

(global-ace-isearch-mode -1)
(setq ace-isearch-submode 'ace-jump-word-mode) ;; one of ''ace-jump-word-mode or 'ace-jump-char-mode
(setq ace-isearch-use-ace-jump t) ;; When non-nil, invoke `ace-jump-mode' if the length of `isearch-string' is equal to 1.
(setq ace-isearch-input-idle-delay 0.3)
(setq ace-isearch-input-length 10)
(setq ace-isearch-funtion-from-isearch 'helm-swoop-from-isearch) ;; helm-occur-from-isearch
(setq ace-isearch-use-function-from-isearch t)

(with-eval-after-load 'speedbar
    (setq speedbar-default-position 'left)

    ;; (setq speedbar-frame-parameters)
    ;; (setq speedbar-tag-hierarchy-method)
    ;; (speedbar-prefix-group-tag-hierarchy speedbar-trim-words-tag-hierarchy)
    (setq speedbar-use-imenu-flag t)
    (setq speedbar-use-images t)
    (setq speedbar-sort-tags nil)
    ;;
    ;; speedbar mode map
    ;;
    (define-key speedbar-mode-map "r"
      (lambda () (interactive)
        (speedbar-change-initial-expansion-list
         speedbar-previously-used-expansion-list-name)))
    (define-key speedbar-mode-map "f"
      (lambda () (interactive)
        (speedbar-change-initial-expansion-list "files")))
    (define-key speedbar-mode-map "B"
      (lambda () (interactive)
        (speedbar-change-initial-expansion-list "quick buffers")))
    (define-key  speedbar-mode-map "b"
      (lambda () (interactive)
        (speedbar-change-initial-expansion-list "buffers")))
    (define-key speedbar-mode-map "i"
      (lambda () (interactive)
        (speedbar-change-initial-expansion-list "info")))
    (define-key speedbar-mode-map "P"
      (lambda () (interactive)
        (speedbar-change-initial-expansion-list "project")))
    (define-key speedbar-mode-map "E"
      (lambda () (interactive)
        (speedbar-change-initial-expansion-list "eieio")))
    (define-key speedbar-mode-map "t" 'speedbar-toggle-updates)
    (define-key speedbar-mode-map "g" 'speedbar-refresh)
    ;; Navigation.
    (define-key speedbar-mode-map "n" nil)
    (define-key speedbar-mode-map "p" nil)
    (define-key speedbar-mode-map "j" 'speedbar-next)
    (define-key speedbar-mode-map "k" 'speedbar-prev)
    (define-key speedbar-mode-map "\M-n" 'speedbar-restricted-next)
    (define-key speedbar-mode-map "\M-p" 'speedbar-restricted-prev)
    (define-key speedbar-mode-map "\C-\M-n" 'speedbar-forward-list)
    (define-key speedbar-mode-map "\C-\M-p" 'speedbar-backward-list)
    (define-key speedbar-mode-map (kbd "SPC") 'speedbar-scroll-up)
    (define-key speedbar-mode-map (kbd "DEL") 'speedbar-scroll-down)
    ;; (define-key my-speedbar-key "Q" 'kill-this-buffer)
    ;;
    ;; File map. File based commands.
    ;; [[file:/usr/share/emacs/24.4.50/lisp/speedbar.el.gz::(defvar%20speedbar-file-key-map][original]]
    ;;
    ;; (define-key speedbar-file-key-map "RET" 'speedbar-edit-line)
    (define-key speedbar-file-key-map "\C-m" 'speedbar-edit-line)
    (define-key speedbar-file-key-map "l" 'speedbar-expand-line)
    (define-key speedbar-file-key-map " " 'speedbar-toggle-line-expansion)
    (define-key speedbar-file-key-map "h" 'speedbar-contract-line)
    (define-key speedbar-file-key-map "[" 'speedbar-expand-line-descendants)
    (define-key speedbar-file-key-map "]" 'speedbar-contract-line-descendants)
    (define-key speedbar-file-key-map "=" nil)
    (define-key speedbar-file-key-map "+" nil)
    (define-key speedbar-file-key-map "-" nil)
    ;;
    (define-key speedbar-file-key-map "u" 'speedbar-up-directory)
    (define-key speedbar-file-key-map "I" 'speedbar-item-info)
    (define-key speedbar-file-key-map "B" 'speedbar-item-byte-compile)
    (define-key speedbar-file-key-map "L" 'speedbar-item-load)
    (define-key speedbar-file-key-map "c" 'speedbar-item-copy)
    (define-key speedbar-file-key-map "d" 'speedbar-item-delete)
    (define-key speedbar-file-key-map "O" 'speedbar-item-object-delete)
    (define-key speedbar-file-key-map "r" 'speedbar-item-rename)
    (define-key speedbar-file-key-map "m" 'speedbar-create-directory))

  ;; This variable is ignored if `speedbar-use-imenu-flag' is non-nil.
  ;; (setq speedbar-fetch-etags-parse-list
  ;;       '(;; Note that java has the same parse-group as c
  ;;         ;; ("^\\.emacs$\\|.\\(el\\|l\\|lsp\\)\\'" . "def[^i]+\\s-+\\(\\(\\w\\|[-_]\\)+\\)\\s-*\C-?")
  ;;         ("^\\.emacs$\\|.\\(el\\|l\\|lsp\\)\\'" . " *;; *\\(\\*\\)")
  ;;         ("\\.tex\\'" . speedbar-parse-tex-string)
  ;;         ("\\.p\\'" . "\\(\\(FUNCTION\\|function\\|PROCEDURE\\|procedure\\)\\s-+\\([a-zA-Z0-9_.:]+\\)\\)\\s-*(?^?")
  ;;         ("\\.org\\" . " *;; *\\(.*\\)")
  ;;         )
  ;;       )

  (define-prefix-command 'my-speedbar-key)
  (global-set-key (kbd "C-x C-a s") 'my-speedbar-key)
  (define-key my-speedbar-key "s" 'speedbar-get-focus)
  (define-key my-speedbar-key "a" 'speedbar-toggle-show-all-files)
  (define-key my-speedbar-key "q" (lambda () (interactive) (speedbar -1)))
  (define-key my-speedbar-key "o" (lambda () (interactive) (speedbar 1) (speedbar-refresh 1)))
  (define-key my-speedbar-key "t" (lambda () (interactive) (speedbar nil) (other-frame 1)))
  (define-key my-speedbar-key "f" (lambda () (interactive)
                                    (sr-speedbar-select-window)
                                    (speedbar-change-initial-expansion-list "files")))
  (add-hook 'speedbar-mode-hook #'speedbar-enable-update)

;; `sr-speedbar-open'                   Open `sr-speedbar' window.
;; `sr-speedbar-close'                  Close `sr-speedbar' window.
;; `sr-speedbar-toggle'                 Toggle `sr-speedbar' window.
;; `sr-speedbar-select-window'          Select `sr-speedbar' window.
;; `sr-speedbar-refresh-turn-on'        Turn on refresh speedbar content.
;; `sr-speedbar-refresh-turn-off'       Turn off refresh speedbar content.
;; `sr-speedbar-refresh-toggle'         Toggle refresh speedbar content.

;;  (define-key endless/toggle-map "s" #'sr-speedbar-toggle)
(setq sr-speedbar-default-width 20)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-max-width 40)
(setq sr-speedbar-width 30)

;; *** Minimap
;; (add-hook 'minimap-mode-hook (lambda ()
;;                                )
;;           )
(eval-after-load 'minimap
  '(progn
     (setq minimap-window-location 'right)
     (setq minimap-minimum-width 50)
     (setq minimap-highlight-line nil)
     (setq minimap-tag-only nil)
     (setq minimap-width-fraction 0.20)
     (setq minimap-recreate-window t)
     (setq minimap-always-recenter nil)
     (setq minimap-recenter-type 'middle)
     (setq minimap-automatically-delete-window t)
     (setq minimap-hide-scroll-bar t)
     (setq minimap-hide-fringes t)
     (setq minimap-dedicated-window nil)
     (setq minimap-enlarge-certain-faces 'always)
     (add-to-list 'minimap-major-modes 'vhdl-mode)
     (add-to-list 'minimap-major-modes 'matlab-mode)
     (add-to-list 'minimap-major-modes 'org-mode)))
;;
;; (minimap-kill-buffer)
;; (minimap-mode -1)
;; (minimap-mode 1)
;; (minimap-setup-hooks)
;;
;;                                     (interactive)
;;                                     (if minimap-bufname
;;                                         (minimap-mode -1)
;;                                       (minimap-mode 1))
;;                                     )
;;                 )

;; (global-set-key (kbd "C-x C-a n") 'minimap-toggle)

;; (put 'narrow-to-defun  'disabled nil)
;; (put 'narrow-to-page   'disabled nil)
;; (put 'narrow-to-region 'disabled nil)
(fancy-narrow-mode 1)
;; doesn't work otherwise
(advice-remove 'search-forward #'ad-Advice-search-forward)
;; (setq my-fancy-narrow-mode t) ;; t / nil
;; (global-set-key (kbd "C-x n n") (lambda (start end)
;;                                   "Disable highlight-changes and narrow region"
;;                                   (interactive "r")
;;                                   (setq highlight-changes-mode-tmp highlight-changes-mode)
;;                                   (highlight-changes-mode 0)
;;                                   (if my-fancy-narrow-mode
;;                                       (fancy-narrow-to-region start end)
;;                                     (narrow-to-region start end))))
;; (global-set-key (kbd "C-x n d") (lambda ()
;;                                   "Disable highlight-changes and narrow to defun"
;;                                   (interactive)
;;                                   (setq highlight-changes-mode-tmp highlight-changes-mode)
;;                                   (highlight-changes-mode 0)
;;                                   (if my-fancy-narrow-mode
;;                                       (fancy-narrow-to-defun)
;;                                     (narrow-to-defun))))
;; (global-set-key (kbd "C-x n p") (lambda ()
;;                                   "Disable highlight-changes and narrow to page"
;;                                   (interactive)
;;                                   (setq highlight-changes-mode-tmp highlight-changes-mode)
;;                                   (highlight-changes-mode 0)
;;                                   (if my-fancy-narrow-mode
;;                                       (fancy-narrow-to-page)
;;                                     (narrow-to-page))))
;; (global-set-key (kbd "C-x n w") (lambda ()
;;                                   "Disable flycheck and highlight-changes and widen"
;;                                   (interactive)
;;                                   (if my-fancy-narrow-mode
;;                                       (fancy-widen)
;;                                     (widen))
;;                                   (when highlight-changes-mode-tmp
;;                                     (highlight-changes-mode t)
;;                                     (highlight-changes-remove-highlight (point-min) (point-max)))))
;; C-x n b          org-fancy-narrow-to-block
;; C-x n e          org-fancy-narrow-to-element
;; C-x n s          org-fancy-narrow-to-subtree

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if you
         ;; don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))
;; (define-key csb/hydra-toggle/keymap (kbd "n") #'narrow-or-widen-dwim)

;; (define-key ctl-x-map "n" #'narrow-or-widen-dwim)
;; (eval-after-load 'latex
;;  '(define-key LaTeX-mode-map "\C-xn" nil))
;; (define-key ctl-x-map "n" #'narrow-or-widen-dwim)

(when running-os-is-linux (org-require 'csb-mail  csb/compile-org-require))

(stante-after ido
              ;; (require 'ido)
              ;; (setq-default ido-enable-flex-matching t)
              ;; (setq-default ido-everywhere t)
              ;; (ido-everywhere)
              ;; (ido-mode 0)
              ;; (local-set-key (kbd "C-x C-f") 'ido-find-file)
              (when running-os-is-linux
                (define-key ido-common-completion-map "\C-n" 'ido-next-match)
                (define-key ido-common-completion-map "\C-p" 'ido-prev-match)))



;; (defun toggle-mode-line () "toggles the modeline on and off"
 ;; (interactive)
 ;; (setq mode-line-format
  ;;  (if (equal mode-line-format nil)
    ;;    (default-value 'mode-line-format)) )
 ;; (redraw-display)
;;  (if (not header-line-format)
;;      (setq header-line-format mode-line-format
;;            mode-line-format nil)
;;    (setq mode-line-format header-line-format
;;          header-line-format nil))
;;  (set-window-buffer nil (current-buffer))
;; (redraw-display)
;; )
;; (add-hook 'after-change-major-mode-hook 'toggle-mode-line)
;; (global-set-key (kbd "M-y") (lambda () (interactive) (toggle-header-mode-line) (redraw-display)))

(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

;; (hidden-mode-line-mode 1) ;; Activate

;; If you want to hide the mode-line in all new buffers
;; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

;; Alternatively, you can paint your mode-line in White but then
;; you'll have to manually paint it in black again
;; (custom-set-faces
;;  '(mode-line-highlight ((t nil)))
;;  '(mode-line ((t (:foreground "white" :background "white"))))
;;  '(mode-line-inactive ((t (:background "white" :foreground "white")))))

;; Command to toggle the display of the mode-line as a header
;; (defvar-local header-line-format nil)

(defun mode-line-in-header ()
  (interactive)
  (if (not header-line-format)
      (setq header-line-format mode-line-format
            mode-line-format nil)
    (setq mode-line-format header-line-format
          header-line-format nil))
  (set-window-buffer nil (current-buffer))
(redraw-display)
)

;; Activate after smart mode line
;; (mode-line-in-header)
;; (global-set-key (kbd "<f7>") 'mode-line-in-header)

(defun csb/toggle-mode-line ()
  "toggles the mode modeline on and off"
  (interactive)
  (if (not (equal mode-line-format nil))
      (setq mode-line-format-save mode-line-format))
  (setq mode-line-format
        (if (equal mode-line-format nil)
            (eval 'mode-line-format-save)))
  (redraw-display)
  (force-mode-line-update))

(key-chord-define-global "tm" #'csb/toggle-mode-line)

;; Remove modeline minor modes
;; (require 'diminish)
;; (eval-after-load "eldoc" '(diminish 'eldoc-mode))
;; (eval-after-load "org" '(diminish 'orgstruct-mode))
;; (eval-after-load "isearch" '(diminish 'isearch-mode))
;; (eval-after-load "auto-fill-mode" '(diminish 'fill-mode))
;; (eval-after-load 'auto-fill-column '(diminish 'adaptive-fill-mode))
;; (diminish 'wrap-region-mode)
;; (diminish 'yas/minor-mode)

;; Rename Major mode Label
(defmacro rename-modeline (package-name mode new-name)
  "PACKAGE-NAME, MODE, NEW-NAME."
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))
;; The first argument is the package name, the second is the mode in question, and the third is the new lighter for the mode
;; (rename-modeline "lisp-mode" emacs-lisp-mode "elisp")

;; '(sml/active-background-color "color-16")
;; '(sml/inactive-background-color "color-248")
;; '(sml/numbers-separator "/")
;; '(sml/show-client nil)

(setq sml/no-confirm-load-theme t)
(setq sml/mode-width 'full)
(setq sml/line-number-format "%5l")
(setq sml/theme 'automatic)
;; (sml/apply-theme "powerline")
;; (setq sml/theme 'dark)

(require 'smart-mode-line)

(add-hook 'after-init-hook 'sml/setup)

(when running-os-is-linux
  (progn
    (add-to-list 'sml/replacer-regexp-list '("^~/.config" ":CFG:"))
    (add-to-list 'sml/replacer-regexp-list '("^~/Documents/OrgNotes/" ":OrgNotes:"))
    (add-to-list 'sml/replacer-regexp-list '("^~/Documents/OrgAgenda/" ":OrgAgenda:"))))
;; (setq sml/replacer-regexp-list '(("^~/.emacs.d/" "ed/")))
;; (add-to-list 'sml/replacer-regexp-list '("^~/Documents/OrgProjects/" "org-prj/"))
;; (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/myscripts/" "myscripts/"))
;; (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/scripts/" "scripts/"))
;; (add-to-list 'sml/replacer-regexp-list '("^~/Documents/ciemat/" "ciemat/"))
;; (add-to-list 'sml/replacer-regexp-list '("^~/Documents/ciemat/Digitalizador/" "Digi/"))
;; (add-to-list 'sml/replacer-regexp-list '("^/tmp/" "TMP/"))
;; (progn
;;  ;;(add-to-list 'sml/replacer-regexp-list '("^~/.config" ":CFG:"))
;;  )

(setq rm-blacklist '(" hl-p" " Fill" " OrgStruct" " SliNav" " AC" " Google" " Undo-Tree" " Helm" " Spell"
                     " ElDoc" " company" " yas" " Abbrev" " PgLn" " Paredit" " *" " Wrap" " WSC" " Fly"
                     " MRev" " +Chg" " Guide" " wg" " ||" " =>" " Projectile" " Ind" " GG"))

;; Set it to the top
;; (setq-default header-line-format mode-line-format
;;               header-line-format-save mode-line-format
;;               )
;; (setq-default mode-line-format header-line-format-save)
;; (setq-default mode-line-format nil)

;; Desable by default
;; (toggle-header-mode-line)
;; (defun toggle-header-mode-line ()
;;   "toggles the header modeline on and off"
;;   (interactive)
;;   (setq header-line-format
;;     (if (equal header-line-format nil)
;;         (default-value 'header-line-format)
;;       ) )
;;   (redraw-display)
;; )

(global-undo-tree-mode 1)
(eval-after-load 'undo-tree
  '(progn
     (define-key undo-tree-map (kbd "C-x U") 'undo-tree-visualize)
     (define-key undo-tree-map (kbd "C-x u") 'undo)
     (define-key undo-tree-map (kbd "C-x r U") nil)
     (define-key undo-tree-map (kbd "C-x r u") nil)))
;; undo-tree-undo
;; undo-tree-redo

(org-require 'csb-spell csb/compile-org-require)

;; Paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; Paren Mode
(show-paren-mode t)
(setq show-paren-style (quote expression))

;; SmartParens Mode
(require 'smartparens-config)
(show-smartparens-global-mode t)

;;  (require 'my_text_beautify)

(org-require 'csb-windows csb/compile-org-require)

(which-function-mode 1)
(setq which-func-unknown "n/a")
(setq-default header-line-format '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
      ;; We remove Which Function Mode from the mode line, because it's mostly
      ;; invisible here anyway.
      (assq-delete-all 'which-func-mode mode-line-misc-info))
;; (add-to-list 'which-func-modes 'ruby-mode)

(org-require 'csb-modal csb/compile-org-require)

(org-require 'csb-www-bookmarking csb/compile-org-require)

(global-set-key (kbd "C-x ;") 'iedit-mode)

;; (setq clippy-tip-show-function #'clippy-popup-tip-show)
;; (require 'clippy)

;; ** Fasd
;; (setq fasd-enable-initial-prompt nil)
;; (setq fasd-completing-read-function completing-read-function)
;; (global-fasd-mode 1)
;; (global-set-key (kbd "C-x /") 'fasd-find-file)

;; (zoom-frm-in)
;; (zoom-frm-out)
;; (zoom-frm-unzoom)

(eval-after-load "dash" '(dash-enable-font-lock))

;; https://github.com/atykhonov/iregister.el/blob/master/README.md

;; (org-babel-load-file "~/.emacs.d/elisp/my_markdown.org" nil)

;; when I edit a firefox form with C-i and pentadactyl in the stackexchange site, use markdown-mode
(add-to-list 'auto-mode-alist '("pentadactyl.emacs.stack\\(exchange\\|overflow\\)\\.com\\.*[a-z0-9]*\\.txt" . markdown-mode))
(add-to-list 'auto-mode-alist '("pentadactyl.stack\\(exchange\\|overflow\\)\\.com\\.*[a-z0-9]*\\.txt" . markdown-mode))
(add-to-list 'auto-mode-alist '("pentadactyl.github.com.txt" . markdown-mode))

(with-eval-after-load 'ham-mode
  (setq ham-mode-markdown-to-html-command (list "/home/csantos/Dropbox/cabal/bin/pandoc"
                                                "--from" "markdown"
                                                "--to" "html"
                                                'file)))
  (add-to-list 'auto-mode-alist
               '("[\\\\/]pentadactyl\\.groups\\.google\\..*\\.txt\\'" .
                 gmail-message-mode))

(add-to-list 'auto-mode-alist
             '("[\\\\/]pentadactyl\\.groups\\.google\\..*\\.txt\\'" .
               gmail-message-mode))

(with-eval-after-load 'hao-mode
  (setq hao-mode-html-to-markdown-command (list "/home/csantos/Dropbox/cabal/bin/pandoc"
                                                "--from" "html"
                                                "--to" "org"
                                                'file))

  (setq hao-mode-markdown-to-html-command (list "/home/csantos/Dropbox/cabal/bin/pandoc"
                                                "--from" "org"
                                                "--to" "html"
                                                'file)))

(stante-after aurel
     (setq aurel-aur-user-name "csantosb")
     (setq aurel-download-directory temporary-file-directory))

(setenv "GPG_AGENT_INFO" (car (split-string (shell-command-to-string "echo $GPG_AGENT_INFO") "\n")))
(setenv "SSH_AUTH_SOCK" "/home/csantos/.gnupg/S.gpg-agent.ssh")

(when running-os-is-linux
  (add-to-list 'load-path (concat user-projects-directory "perso/password-store/contrib/emacs"))
  (autoload 'password-store-get "password-store" "" t)
  (stante-after password-store
                (setenv "PASSWORD_STORE_DIR" "/home/csantos/sb/.password-store")
                (setenv "PASSWORD_STORE_X_SELECTION" "primary")))

;; (org-babel-load-file "~/.emacs.d/elisp/my-cheats.org" nil)

;; (setq tramp-default-method "ssh")
;; (setq tramp-default-host "188.226.230.93")
;; (setq tramp-default-user "csantos")
;; /csantos@188.226.230.93#55524:/home/csantos/mytest
;; /ssh:do_csantosb:/home/csantos/mytest
;; (eval-after-load 'tramp '(setenv "SHELL" "/usr/bin/bash"))
;; (setq tramp-verbose 4)

;; /usr/share/emacs/site-lisp/dotemacs
;; these lines enable the use of gnuplot mode
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
;; this line automatically causes all files with the .gp extension to
;; be loaded into gnuplot mode
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
;; This line binds the function-9 key so that it opens a buffer into gnuplot mode
;; (global-set-key [(f9)] 'gnuplot-make-buffer)

(when running-os-is-linux (org-require 'csb-irc))

(org-require 'csb-outshine csb/compile-org-require)

(org-require 'csb-vc csb/compile-org-require)

(define-key (current-global-map)
  [remap async-shell-command] 'with-editor-async-shell-command)
(define-key (current-global-map)
  [remap shell-command] 'with-editor-shell-command)
;; (add-hook 'shell-mode-hook  #'(lambda()(with-editor-export-editor "EDITOR")))
;; (add-hook 'term-mode-hook   #'(lambda()(with-editor-export-editor "EDITOR")))
;; (add-hook 'eshell-mode-hook #'(lambda()(with-editor-export-editor "EDITOR")))

(org-require 'csb-fringes csb/compile-org-require)

;; (defvar bzg-big-fringe-mode nil)

;; (define-minor-mode bzg-big-fringe-mode
;;   "Minor mode to hide the mode-line in the current buffer."
;;   :init-value nil
;;   :global t
;;   :variable bzg-big-fringe-mode
;;   :group 'editing-basics
;;   (if (not bzg-big-fringe-mode)
;;       (set-fringe-style nil)
;;     (set-fringe-mode
;;      (/ (- (frame-pixel-width)
;;           (* 200 (frame-char-width)))
;;       2)
;;    )))

;; (defun bzg-big-fringe-mode ()
;;  "Minor mode to hide the mode-line in the current buffer."
;;  (if (not bzg-big-fringe-mode)
;;      (set-fringe-style nil)
;;    (set-fringe-mode
;;     (/ (- (frame-pixel-width)
;;          (* 100 (frame-char-width)))
;;      2)
;;   )))
;; (window-body-width)

;; Now activate this global minor mode
;; (bzg-big-fringe-mode 1)

;; Set the color of the fringe
;; (custom-set-faces '(fringe ((t (:background "white")))))

(org-require 'csb-project-management csb/compile-org-require)

;; (org-babel-load-file "~/.emacs.d/elisp/my_workspace.org" nil)

;; (autoload 'notmuch "notmuch" "notmuch mail" t)

;; (require 'sunrise-commander)
;; (add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))

(org-require 'csb-minimal csb/compile-org-require)

(org-require 'csb-help csb/compile-org-require)

(require 'popup)

(org-require 'csb-dired csb/compile-org-require)

;; Sources: https://github.com/legoscia/emacs-jabber
;; Manual: http://emacs-jabber.sourceforge.net/manual-0.8.0/
;; http://www.emacswiki.org/emacs/JabberEl
;; (setq jabber-account-list
;;      '(("cayetano.santos@inventati.org"
;;         (:network-server . "jabber.autistici.org")
;;         (:connection-type . starttls)
;;         (:port . 5222)
;;         (:password . "")
;;         )))
;; (setq jabber-account-list
;;      '(("csantosb@csantosb.ddns.net"
;;         (:network-server . "csantosb.ddns.net")
;;         (:connection-type . starttls)
;;         (:port . 5222)
;;         (:password . "")
;;         )))
;;  TODO: password
;; (setq jabber-account-list
;;      '(("csantosb@jabber.fr"
;;         (:network-server . "jabber.fr")
;;         (:connection-type . starttls)
;;         (:port . 5222)
;;         (:password . "'n,p/d4/~A C69{:$[r>J")
;;         )))
;; (require 'jabber)

(defun csb/set-font ()
  (let* ((csb/myfont "Inconsolata")  ;; "Symbola-12", "DejaVu Sans Mono-11", "Inconsolata"
         (csb/font-size (cond ((eq (display-pixel-width) 1680) 12)
                              ((eq (display-pixel-width) 1920) 16)
                              (t 16))))
    (setq csb/font (format "%s-%i" csb/myfont csb/font-size))
    (set-frame-font csb/font)
    (set-face-font 'default csb/font)
    (set-default-font csb/font)))

(add-hook 'before-make-frame-hook #'csb/set-font)
(add-hook 'persp-switch-hook #'csb/set-font)

(require 'uniquify)
(setq-default uniquify-buffer-name-style 'post-forward-angle-brackets)

(org-require 'csb-backup csb/compile-org-require)

;; (which-key-mode -1)
(stante-after which-key
              ;; Set the time delay (in seconds) for the which-key popup to appear.
              (setq which-key-idle-delay 1.0)

              ;; Set the maximum length (in characters) for key descriptions (commands or
              ;; prefixes). Descriptions that are longer are truncated and have ".." added
              (setq which-key-max-description-length 27)

              ;; Set the separator used between keys and descriptions. Change this setting to
              ;; an ASCII character if your font does not show the default arrow. The second
              ;; setting here allows for extra padding for Unicode characters. which-key uses
              ;; characters as a means of width measurement, so wide Unicode characters can
              ;; throw off the calculation.
              (setq which-key-separator " → " )
              (setq which-key-unicode-correction 3)

              ;; Set the special keys. These are automatically truncated to one character and
              ;; have which-key-special-key-face applied. Set this variable to nil to disable
              ;; the feature
              (setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))

              ;; Show the key prefix on the left or top (nil means hide the prefix). The
              ;; prefix consists of the keys you have typed so far. which-key also shows the
              ;; page information along with the prefix.
              (setq which-key-show-prefix 'left)

              ;; Set to t to show the count of keys shown vs. total keys in the mode line.
              (setq which-key-show-remaining-keys nil))

(load-theme 'hc-zenburn t)

;;(require 'ein)
;; (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

;; (org-babel-load-file "~/.emacs.d/elisp/my_twittering.org" nil)

(when running-os-is-linux (org-require 'csb-calendar csb/compile-org-require))

(org-require 'csb-www csb/compile-org-require)

(require 'mm-url)
(defun th-get-tinyurl ()
  "Grabs the url at point and echos the equivalent tinyurl in the
minibuffer to ease cutting and pasting."
  (interactive)
  (let* ((long-url (thing-at-point 'url))
         (tinyurl
          (save-excursion
            (with-temp-buffer
              (mm-url-insert
               (concat "http://tinyurl.com/api-create.php?url=" long-url))
              (kill-ring-save (point-min) (point-max))
              (buffer-string)))))
    (message "Tinyurl: %s" tinyurl)
    (replace-regexp-in-string "\\n" "" tinyurl)
    ;; csb
    (let ((bounds (bounds-of-thing-at-point 'url)))
      (kill-region (car bounds) (cdr bounds)))
    (insert tinyurl)
    ;; csb
))

(defun th-get-metamark ()
  "Grabs the url at point and echos the equivalent metamark url
in the minibuffer to ease cutting and pasting."
  (interactive)
  (let* ((long-url (thing-at-point 'url))
         (url (concat "http://metamark.net/api/rest/simple?"
                      (mm-url-encode-www-form-urlencoded
                       (list (cons "long_url" long-url)))))
         (short-url
          (save-excursion
            (with-temp-buffer
              (mm-url-insert url)
              (kill-ring-save (point-min) (point-max))
              (buffer-string)))))
    (message "Metamark: %s" short-url)
    (replace-regexp-in-string "\\n" "" short-url)))

(defun th-replace-url-with-short-url ()
  "Grabs the url at point and replaces it with the equivalent
tinyurl or metamark url."
  (interactive)
  (let* ((long-url (thing-at-point 'url))
         (type (completing-read "What type? " '("metamark" "tinyurl")))
         (short-url
          (if (string= type "metamark")
              (th-get-metamark)
            (th-get-tinyurl))))
    (let ((bounds (bounds-of-thing-at-point 'url)))
      (kill-region (car bounds) (cdr bounds)))
    (insert short-url)))

;; (require 'bitly)
(with-eval-after-load 'bitly
  (setq bitly-access-token (password-store-get "www/bitly-token")))

(org-require 'csb-terminal nil)



;; Neds to rm popup.el? and copy the right one not to disturb the rest
;; '(auto-completion-delay (quote ( . 1)))
;; '(auto-completion-min-chars (quote ( . 3)))
;; i) loading ___________
;; (require 'predictive)
;; (add-to-list 'load-path "~/Dropbox/.emacs.d/el-get/predictive/")
;; (add-to-list 'load-path "~/Dropbox/.emacs.d/el-get/predictive/latex/")
;; (add-to-list 'load-path "~/Dropbox/.emacs.d/el-get/predictive/texinfo/")
;; (add-to-list 'load-path "~/Dropbox/.emacs.d/el-get/predictive/html/")
;; (autoload 'predictive-mode "~/Dropbox/.emacs.d/el-get/predictive/predictive" "Turn on Predictive Completion Mode." t)
;; (predictive-setup-texinfo 1) ???
;; ii) Keys : act / deact
;; (defun my_predictive_activate()
;;   "Activate predictive mode locally.
;; Sets default language to English"
;;   (interactive)
;;   (predictive-mode t)
;;   (turn-on-predictive-mode)
;;   ;; (setq auto-completion-min-chars (quote ( . 3)))
;;   ;; (setq completion-auto-show-delay (quote ( . 0.5)))
;;   ;; (setq auto-completion-delay (quote ( . 0.5)))
;; )

;; (defun my_predictive_deactivate()
;;   "Deactivates predictive mode locally."
;;   (interactive)
;;   (predictive-mode nil))

;; (global-set-key (kbd "C-x p a") 'my_predictive_activate)

;; (global-set-key (kbd "C-x p d") 'my_predictive_deactivate)
;; (add-hook 'predictive-mode-hook 'my_predictive)

(setq-default bookmark-save-flag t)
(setq-default bookmarnit+ak-default-file '"~/.emacs.d/.bookmarks")
(setq-default bmkp-last-as-first-bookmark-file "~/.emacs.d/.bookmarks")
(setq-default bookmark-default-file "~/.emacs.d/.bookmarks")
(require 'bookmark+)

;; ** bookmark bmenu mode
;; kill the direx window
(add-hook 'bookmark-bmenu-mode-hook (lambda () (define-key bookmark-bmenu-mode-map (kbd "Q") 'kill-this-buffer)))

;; (setq gnus-select-method '(nntp "news.gmane.org"))

;; (require 'my_transparency)

;; (require 'my_elscreen)

;; (require 'midnight)
;; (setq-default midnight-period 60)
;; (add-hook 'midnight-hook 'org-mobile-push)
;; (midnight-delay-set 'midnight-delay 0)
;; ;; (midnight-next)
;; (cancel-timer)
;; (timer-activate)
;; (midnight-timer)
;; ;; (timer-list)

(setq ivy-display-style 'fancy)
;;advise swiper to recenter on exit
(defun bjm-swiper-recenter (&rest args)
  "recenter display after swiper"
  (recenter))
(advice-add 'swiper :after #'bjm-swiper-recenter)

(org-require 'csb-helm nil)

(org-require 'csb-search csb/compile-org-require)

(org-require 'csb-deft csb/compile-org-require)

(with-eval-after-load 'dash
  (dash-enable-font-lock))

(global-set-key (kbd "C-c d") 'define-word-at-point)
(global-set-key (kbd "C-c D") 'define-word)

(when running-os-is-linux (org-require 'csb-pdf csb/compile-org-require))

(when running-os-is-linux (org-require 'csb-docview))

;; (require 'imenu+)
(setq-default imenu-auto-rescan t)
(setq imenu-space-replacement ".")
(setq imenu-level-separator ":")
(setq imenu-sort-function nil)
;; (setq-default imenu-generic-expression)
;; (setq-default imenu-sort-function 'imenu--sort-by-name)

;; (org-babel-load-file "~/.emacs.d/elisp/my_ecb.org" nil)

(when running-os-is-linux
  (global-ede-mode 0)
  (require 'semantic/sb)

  (require 'cedet)
  (require 'speedbar)
  (require 'ede))
;; (setq  semantic-symref-tool 'detect)
;;(defconst my-speedbar-buffer-name "SPEEDBAR")

(semantic-mode 1)

(set-default 'semantic-case-fold t)

(when running-os-is-linux
  (setq debug-on-error t)
  (setq debug-on-message t))
(org-require 'csb-debug csb/compile-org-require)

(beacon-mode 1)
(with-eval-after-load 'beacon
  (setq beacon-color 0.5
        beacon-blink-delay 0.3
        beacon-blink-duration 0.3
        beacon-blink-when-point-moves-horizontally 2
        beacon-blink-when-point-moves-vertically 2))

(org-require 'csb-apps csb/compile-org-require)

;;  (add-hook 'special-mode-hook (lambda () (local-set-key (kbd "Q") 'kill-this-buffer)))

(add-hook 'text-mode-hook
          (lambda ()
            (auto-fill-mode)
            (turn-on-auto-fill)
            (setq-local show-trailing-whitespace nil)
            (flyspell-mode 1)
            (require 'company-ispell)
            ;; make `company-backends' local is critcal
            ;; or else, you will have completion in every major mode, that's very annoying!
            (make-local-variable 'company-backends)
            ;; company-ispell is the plugin to complete words
            (add-to-list 'company-backends 'company-ispell)))

(add-hook 'prog-mode-hook
          (lambda ()
            (highlight-indentation-mode t)  ;; ||
            (font-lock-comment-annotations)
            (font-lock-doc-annotations)
            (auto-fill-mode)
            ;; (ispell-minor-mode)
            (turn-on-auto-fill)
            (flyspell-prog-mode)
            (csb/mode-line-off)
            (rainbow-delimiters-mode)
            (redraw-display)
            ;; `(define-key ,major-mode (kbd "C-c C-p")
            ;;    #'(lambda()(interactive)
            ;;        (re-search-backward outline-regexp)
            ;;        (beginning-of-line)))
            ))
;; (when (projectile-project-p)
;;   (projectile-cache-current-file)
;;   (message "File added to cache of %s" (projectile-project-root)))
;; (turn-on-orgstruct++)
;; (outline-minor-mode)

(defun is-buffer-file-temp ()
  (interactive)
  "If (buffer-file-name) is nil or a temp file or HTML file converted from org file"
  (message "is-buffer-file-temp called")
  (let ((f (buffer-file-name))
        org
        (rlt t))
    (cond
     ((not f)
      (setq rlt t)
      (message "(buffer-file-name) is nil"))
     ((string-match (concat "^" temporary-file-directory) f)
      (setq rlt t)
      (message "%s is from temp dir %s" temporary-file-directory temporary-file-directory))
     ((and (string-match "\.html$" f)
           (file-exists-p (setq org (replace-regexp-in-string "\.html$" ".org" f))))
      (setq rlt t)
      (message "This files is created from %s" org))
     (t
      (setq rlt nil)))
    rlt))

;; when using asd this criteria is not adequate
;; (add-hook 'prog-mode-hook #'is-buffer-file-temp)

(stante-after prog-mode (define-key prog-mode-map (kbd "M-#") 'outorg-edit-as-org))

(add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; (add-to-list 'auto-mode-alist '("\\.cheat\\'" . conf-unix-mode))
;; (add-to-list 'auto-mode-alist '("\\.offlineimaprc\\'" . conf-unix-mode))
;; (add-to-list 'auto-mode-alist '("/muttrc" . conf-unix-mode))
;; (add-to-list 'auto-mode-alist '("/muttrc_bindings" . conf-unix-mode))
;; (add-to-list 'auto-mode-alist '("/muttrc_macros" . conf-unix-mode))
;; (add-to-list 'auto-mode-alist '("/.lbdbrc" . conf-unix-mode))
;; (add-to-list 'auto-mode-alist '(".ucf" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.gws" . conf-unix-mode))
(add-hook 'conf-unix-mode-hook (lambda ()
                                 ;; (font-lock-comment-annotations)
                                 ;; (font-lock-doc-annotations)
                                 ;; (turn-on-orgstruct++)
                                 ;; (outline-minor-mode)
                                 (setq-local orgstruct-heading-prefix-regexp " *## ")
                                 ;; (add-to-list 'imenu-generic-expression '(nil  "^## *\\(.*\\)" 1))
                                 (setq-mode-local conf-unix-mode imenu-generic-expression
                                                  '(("" "^ *# +\\([*]+ .*\\)" 1)
                                                    ("" "^\\([^#]\w*.+\\) = " 1)
                                                    ;; ("" "^## *\\(.*\\)" 1)
                                                    ;; ("" "^[   ]*\\(.+?\\)[    ]*=" 1)
                                                    (nil "^[    ]*\\[[  ]*\\(.+\\)[     ]*\\]" 1)
                                                    (nil "^[    ]*\\([^=:{}     \n][^=:{}\n]+\\)[   \n]*{" 1)))))

;; TODO: In shell-script mode ??
;; [[file:/usr/share/emacs/24.4.50/lisp/progmodes/sh-script.el.gz::%3B%3B%3B%20sh-script.el%20---%20shell-script%20editing%20commands%20for%20Emacs%20-*-%20lexical-binding:t%20-*-][here]]
(add-to-list 'auto-mode-alist '("\\.conkyrc\\'" . sh-mode))
(add-hook 'sh-mode-hook (lambda () (define-key sh-mode-map (kbd "C-c C-o") nil)
                          (define-key sh-mode-map (kbd "C-c C-l") nil)
                          (setq-local orgstruct-heading-prefix-regexp " *## ")
                          (add-to-list 'imenu-generic-expression '(nil  "^## *\\(.*\\)" 1))))

(org-require 'csb-c-mode csb/compile-org-require)

;; ** ess [julia] mode
;; (add-to-list 'load-path (concat user-emacs-directory "libraries/julia/contrib"))
;; (when running-os-is-linux
;;   (org-babel-load-file "~/.emacs.d/elisp/my_ess.org" nil))

;; ;; ** Qt
;; ;; Qt base directory, meaning the directory where the 'Qt' directory can be found.
;; ;; Adapt accordingly.
;; (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
;; ;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; ;; (add-hook 'c++-mode-hook 'ecb-mode)
;; ;; (add-hook 'c++-mode-hook 'ecb-activate)
;; (setq qt4-base-dir "/usr/include/qt4")
;; (setq qt4-gui-dir (concat qt4-base-dir "/QtGui"))
;; ;;
;; (add-to-list 'auto-mode-alist '("/usr/include/qt4" . c++-mode))
;; (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
;; ;;
;; (semantic-add-system-include "/usr/include/qt4" 'c++-mode)
;; (semantic-add-system-include qt4-base-dir 'c++-mode)
;; (semantic-add-system-include qt4-gui-dir 'c++-mode)
;; ;;
;; (defvar semantic-lex-c-preprocessor-symbol-file '())
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-large.h"))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-dist.h"))

(when running-os-is-linux
  (setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-hook 'lua-mode-hook (lambda () (define-key lua-mode-map (kbd "C-c C-l") nil)
                             ;; derived from prog-mode
                             (setq-local orgstruct-heading-prefix-regexp " *--- ")
                             (add-to-list 'imenu-generic-expression '(nil  " *--- *\\(.*\\)" 1))
                             (require 'helm-dash)
                             (setq-local helm-dash-common-docsets '("Lua"))
                             (setq-local helm-dash-docsets '("Lua"))
                             (helm-dash-activate-docset "Lua"))))

(org-require 'csb-elisp csb/compile-org-require)

(org-require 'csb-python csb/compile-org-require)

(org-require 'csb-vhdl csb/compile-org-require)

(org-require 'csb-julia csb/compile-org-require)

;; (org-require 'csb-octave)

(prefer-coding-system 'utf-8)
;; Enabling it changes the definition of words such that symbols characters are treated
;; as parts of words: e.g., in ‘superword-mode’, "this_is_a_symbol" counts as one word.
(global-superword-mode -1)
(global-eldoc-mode 1)
(visual-line-mode 1)
;; (global-column-enforce-mode 1)
(electric-indent-mode 1)            ;; ref @ http://emacsredux.com/blog/2014/01/19/a-peek-at-emacs-24-dot-4-auto-indentation-by-default
(electric-pair-mode 1)              ;; ref @ http://ergoemacs.org/emacs/emacs_insert_brackets_by_pair.html
(global-prettify-symbols-mode 1)    ;; ref @ http://ergoemacs.org/emacs/emacs_pretty_lambda.html and http://emacsredux.com/blog/2014/08/25/a-peek-at-emacs-24-dot-4-prettify-symbols-mode/
(global-page-break-lines-mode t)    ;; ref @ https://github.com/purcell/page-break-lines (C-q C-l)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)  ;; Also auto refresh dired, but be quiet about it
(setq auto-revert-verbose nil)
(global-hungry-delete-mode 1)
(global-hl-line-mode 1)
(blink-cursor-mode -1)
(tooltip-mode 0)  ;; help text (e.g. for buttons and menu items that you put the mouse on)
(menu-bar-mode 0)
(tool-bar-mode 0)
(column-number-mode 1)
(scroll-bar-mode 0)
;; (add-to-list 'which-func-modes 'emacs-lisp-mode)
(global-font-lock-mode 1)
(variable-pitch-mode -1)
(global-aggressive-indent-mode t)  ;; mode-line indicator =>
(indent-guide-global-mode -1)      ;; show vertical lines to guide indentation; mode-line indicator ||
(setq indent-guide-recursive t)

;; http://batsov.com/articles/2011/11/25/emacs-tip-number-3-whitespace-cleanup/
(global-whitespace-cleanup-mode 1)  ;; Remove useless whitespaces before saving a file
;; (add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(setq-default default-justification 'left)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default cursor-type '(hbar . 2))
(setq mode-line-misc-info (assq-delete-all 'which-func-mode mode-line-misc-info))
(setq-default fill-column 80)
;; (setq initial-frame-alist
;;       '((menu-bar-lines . 0)
;;         (tool-bar-lines . 0)))
;; (setq initial-frame-alist
;;      '((top . 65) (left . 300)
;;        (width . 135) (height . 40)
;;        )
;;      )
;; (setq initial-major-mode 'org-mode)

(setq x-stretch-cursor 1
      inhibit-startup-message t
      ;; makes new windows tile horizontally
      split-width-threshold 0
      initial-scratch-message nil
      confirm-kill-emacs nil
      inhibit-startup-screen t
      make-pointer-invisible t
      indent-tabs-mode nil
      echo-keystrokes 0.1
      use-dialog-box nil
      apropos-do-all t
      inhibit-startup-echo-area-message "csantos")

(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))
(setq-default highlight-tabs t)
(setq-default tab-width 8)
(setq-default visible-bell t)
(setq-default major-mode 'text-mode)
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)
(setq global-hl-line-sticky-flag t)
(setq x-select-enable-clipboard t)  ;; after copy Ctrl+c in X11 apps, you can paste by `yank' in emacs
(setq x-select-enable-primary t)    ;; after mouse selection in X11, you can paste by `yank' in emacs
(setq-default c-basic-offset 4)

;; (global-set-key (kbd "C-c s") 'yank-pop)
(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-set 0.3)))
(global-set-key (kbd "C-+") (lambda () (interactive) (text-scale-increase 0.5)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-decrease 0.5)))
(global-set-key (kbd "C-x C-0") 'text-scale-adjust)
;; (kbd "C-x C-a") is for gud
(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-c:" 'uncomment-region)
(global-set-key (kbd "C-x C-u") 'undo)
;; (global-set-key (kbd "C-x 1") 'just-one-space)
(global-set-key (kbd "C-x 1") 'cycle-spacing)
(global-set-key (kbd "C-x G") 'goto-line)
(global-set-key (kbd "<f2>") 'rename-buffer)
(global-set-key (kbd "C-x ,") 'rename-buffer)
(global-set-key (kbd "C-x r s") 'replace-string)
;; (global-set-key "\M-x" 'execute-extended-command)
;; (global-set-key (kbd "M-!") ' ???)

(global-unset-key (kbd "C-;"))
(global-unset-key (kbd "M-:"))
(global-unset-key "\C-x\C-v" )    ;; buffer-menu
(global-unset-key (kbd "C-M-f"))

(defadvice helm-display-mode-line (after undisplay-header activate)
  (setq header-line-format nil))

(provide 'csb-config)
;;; csb-config.el ends here
