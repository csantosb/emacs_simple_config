
;; * Generic
(defvar running-os-is-linux (eq system-type 'cygwin)
  "OS used as bootloader for emacs")

(defun server-ensure-safe-dir (dir) "Noop" t)

(if running-os-is-linux
    (setq-default user-emacs-directory '"~/.emacs.d/")
  (setq-default user-emacs-directory '"~/.emacs.d/"))

;; Set user init file
(if running-os-is-linux
    (setq-default user-init-file (concat user-emacs-directory "init.el"))
  (setq-default user-init-file (concat user-emacs-directory "init.el")))

;; Set user dir containing "Projects" folder
(if running-os-is-linux
    (setq-default user-projects-directory '"~/Projects/perso/")
  (setq-default user-projects-directory '"~/Projects/perso/"))

;; Set user dir containing "Documents" folder
(if running-os-is-linux
    (setq-default user-documents-directory '"~/Documents/")
  (setq-default user-documents-directory '"~/Documents/"))

;; Environment
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setenv "SHELL" (getenv "SHELL"))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path (concat user-projects-directory "helm-perso-wiki"))
(add-to-list 'load-path (concat user-projects-directory "emacs_config_simple"))

(defmacro rename-modeline (package-name mode new-name)
  "PACKAGE-NAME, MODE, NEW-NAME."
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
	(setq mode-name ,new-name))))

(setq custom-file "~/.emacs.d/emacs-custom")

(defun csb/toggle-fullscreen ()   )

(package-initialize)

;; * Convenience

;; ** Dedicated Maps

;; *** Launcher Map
(define-prefix-command 'launcher-map)
(define-key ctl-x-map "l" 'launcher-map)

;; * Packges
(require 'csb-repositories)

;; * Misc

(require 'csb-miscellaneous)

(defun csb/mode-line-off ()
  "switches the mode modeline off"
  (interactive)
  (when (not (equal mode-line-format nil))
    (setq mode-line-format-save mode-line-format)
    (setq mode-line-format nil))
  (redraw-display)
  (force-mode-line-update))

;; * Stante
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

;; * Libraries
;; ** Helm
(require 'csb-helm)

;; ** vc
(when (string= system-name "apcpc152")
  (defun server-ensure-safe-dir (dir) "Noop" t))
(require 'csb-vc)

;; * Completion
(require 'csb-completions)

;; * Outshine
(require 'outshine)
(add-hook 'outline-minor-mode-hook 'outshine-hook-function)
(setq outshine-use-speed-commands t
      outshine-startup-folded-p t)
(defvar outline-minor-mode-prefix "\M-#")

(with-eval-after-load 'outshine
  (define-key outline-minor-mode-map (kbd "<backtab>") 'outshine-cycle-buffer)

  (outshine-define-key-with-fallback
   outline-minor-mode-map (kbd "M-<up>")
   (outline-move-subtree-up) (outline-on-heading-p))

  (outshine-define-key-with-fallback
   outline-minor-mode-map (kbd "M-<down>")
   (outline-move-subtree-down) (outline-on-heading-p))

  (outshine-define-key-with-fallback
   outline-minor-mode-map (kbd "M-<left>")
   (outline-demote) (outline-on-heading-p))

  (outshine-define-key-with-fallback
   outline-minor-mode-map (kbd "M-<right>")
   (outline-promote) (outline-on-heading-p)))

;; * window
(require 'csb-windows)

;; * Programming Modes

;; ** prog mode
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
	    (redraw-display)))

;; ** Emacs Lisp

(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-prog-mode))))

(with-eval-after-load 'emacs-lisp
  (define-key emacs-lisp-mode-map (kbd "M-j") 'forward-sexp)
  (define-key emacs-lisp-mode-map (kbd "M-k") 'backward-sexp))

;; Navigate - Slime
;; Default Keys
;; [[file:~/.emacs.d/el-get/package/elpa/elisp-slime-nav-20130613.1109/elisp-slime-nav.el::(defvar%20elisp-slime-nav-mode-map][Defaults]]
;; These shadow ctags
(with-eval-after-load 'elisp-slime-nav-mode-map
  (define-key elisp-slime-nav-mode-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)
  (define-key elips-slime-nav-mode-map (kbd "M-,") 'pop-tag-mark)
  (global-set-key (kbd "C-c C-d C-d") nil))

;; Hook
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; (setq-local orgstruct-heading-prefix-regexp "^ *;; *\\(\\**.*\\)")
            ;; (add-to-list 'imenu-generic-expression '(" "  " *;; *\\(.*\\)" 1))
            ;; (add-to-list 'imenu-generic-expression '(nil "^\\(?:[*]+\\).*$" 0))
            ;; (add-to-list 'imenu-generic-expression '(nil "^ *;; *\\(\\*.*\\)" 1))
            ;; (add-to-list 'imenu-generic-expression '(nil "^ *;;; +\\([*]+ .*\\)" 1))
	    (outline-minor-mode 1)
	    (nameless-mode-from-hook)
            (setq-local imenu-generic-expression
                        '(("" "^ *;; +\\([*]+ .*\\)" 1)
                          ("" "^\\s-*(\\(cl-def\\(?:ine-compiler-macro\\|macro\\|subst\\|un\\)\\|def\\(?:advice\\|generic\\|ine-\\(?:compil\\(?:ation-mode\\|er-macro\\)\\|derived-mode\\|g\\(?:\\(?:eneric\\|lobal\\(?:\\(?:ized\\)?-minor\\)\\)-mode\\)\\|m\\(?:ethod-combination\\|inor-mode\\|odify-macro\\)\\|s\\(?:etf-expander\\|keleton\\)\\)\\|m\\(?:acro\\|ethod\\)\\|s\\(?:etf\\|ubst\\)\\|un\\*?\\)\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)
                          ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\)\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)
                          ("Variables" "^\\s-*(defvar\\s-+\\(\\(\\sw\\|\\s_\\)+\\)[[:space:]\n]+[^)]" 1)
                          ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(\\sw\\|\\s_\\)+\\)" 2)))
            (elisp-slime-nav-mode)
            ;; (remove-elc-on-save) done in package auto-compile
            (enable-paredit-mode)
            (rename-modeline "lisp-mode" emacs-lisp-mode "elisp")
            (require 'helm-dash)
            (setq-local helm-dash-common-docsets '("Emacs Lisp"))
            (setq-local helm-dash-docsets '("Emacs Lisp"))
            ;; Company
            (require 'company-elisp)
            ;; make `company-backends' local is critcal
            ;; or else, you will have completion in every major mode, that's very annoying!
            (make-local-variable 'company-backends)
            ;; company-elisp is the plugin to complete words
            (add-to-list 'company-backends 'company-elisp)
            ;;
            ;; (make-local-variable 'company-backends)
            ;; (add-to-list 'company-backends 'company-elisp)
            ;; (add-to-list 'company-backends 'company-capf)
            (helm-dash-activate-docset "Emacs Lisp")))

(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (enable-paredit-mode)))

(add-hook 'ielm-mode-hook 'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

;; * vhdl mode
(require 'csb-vhdl)

;; * perspective
(with-eval-after-load 'perspective
  ;; swithching
  (define-key persp-mode-map (kbd "C-x x S") 'persp-switch-quick)
  (define-key persp-mode-map (kbd "C-¿") 'persp-switch-quick)
  (define-key persp-mode-map (kbd "C-¡") 'persp-switch)
  (define-key persp-mode-map (kbd "C-x x s") 'persp-switch)
  (define-key persp-mode-map (kbd "C-x x n") 'persp-next)
  (define-key persp-mode-map (kbd "C-x x p") 'persp-prev)
  (define-key persp-mode-map (kbd "C-_") #'persp-switch)

  ;; killing
  (defun csb/projectile-perspective-kill()
    ""
    (interactive)
    (mapc (lambda(arg) (kill-buffer arg)) (persp-buffers persp-curr))
    (persp-kill (persp-name persp-curr)))
  (define-key persp-mode-map (kbd "C-x x k") #'persp-kill)
  (define-key persp-mode-map (kbd "C-x x K") #'csb/projectile-perspective-kill)
  (key-chord-define persp-mode-map (kbd "xk") #'csb/projectile-perspective-kill)

  (define-key persp-mode-map (kbd "C-x x A") 'persp-set-buffer)
  (define-key persp-mode-map (kbd "C-x x a") 'persp-add-buffer)
  (define-key persp-mode-map (kbd "C-x x d") 'persp-remove-buffer)
  (define-key persp-mode-map (kbd "C-x x i") 'persp-import-buffers)
  (define-key persp-mode-map (kbd "C-x x ,") 'persp-rename)

  ;; (define-key persp-mode-map (kbd "C-x x w") 'persp-save-state-to-file)
  ;; (define-key persp-mode-map (kbd "C-x x l") 'persp-load-state-from-file)
  (define-key persp-mode-map (kbd "C-x x <left>") nil)
  (define-key persp-mode-map (kbd "C-x x <right>") nil)
  (define-key persp-mode-map (kbd "C-x x c") nil))

(add-hook 'after-init-hook #'(lambda()(persp-mode t)))

(with-eval-after-load 'perspective
  (setq-default persp-show-modestring 'header)
  (setq persp-interactive-completion-function 'ido-completing-read)
  (setq persp-initial-frame-name "init")
  (setq-default persp-modestring-dividers '(" " " " " ")))

;; * Projects
(require 'csb-project-management)

(require 'csb-fringes)

(require 'csb-backup)

;; * Theme
(load-theme 'hc-zenburn t)

;; * Font
(defun csb/set-font ()
  (let* ((csb/myfont "Courier")  ;; "Symbola-12", "DejaVu Sans Mono-11", "Inconsolata"
	 (csb/font-size (cond ((eq (display-pixel-width) 1680) 14)
			      ((eq (display-pixel-width) 1920) 16)
			      (t 16))))
    (setq csb/font (format "%s-%i" csb/myfont csb/font-size))
    (set-frame-font csb/font)
    (set-face-font 'default csb/font)
    (set-default-font csb/font)
    ;; (setq default-frame-alist '((font . csb/font)))
    (add-hook 'before-make-frame-hook (lambda () (set-frame-font csb/font)))))
(csb/set-font)

;; * Global

;; ** Modes

(prefer-coding-system 'utf-8)
;; Enabling it changes the definition of words such that symbols characters are treated
;; as parts of words: e.g., in ‘superword-mode’, "this_is_a_symbol" counts as one word.
(global-superword-mode -1)
;; (global-eldoc-mode 1)
(visual-line-mode 1)
;; (global-column-enforce-mode 1)
(electric-indent-mode 1)            ;; ref @ http://emacsredux.com/blog/2014/01/19/a-peek-at-emacs-24-dot-4-auto-indentation-by-default
(electric-pair-mode 1)              ;; ref @ http://ergoemacs.org/emacs/emacs_insert_brackets_by_pair.html
(global-prettify-symbols-mode 1)    ;; ref @ http://ergoemacs.org/emacs/emacs_pretty_lambda.html and http://emacsredux.com/blog/2014/08/25/a-peek-at-emacs-24-dot-4-prettify-symbols-mode/
;; (global-page-break-lines-mode t)    ;; ref @ https://github.com/purcell/page-break-lines (C-q C-l)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)  ;; Also auto refresh dired, but be quiet about it
(setq auto-revert-verbose nil)
;; (global-hungry-delete-mode 1)
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

;; ** Variables
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

;; ** Keys
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

;; * Beacon
(beacon-mode 1)
(with-eval-after-load 'beacon
  (setq beacon-color 0.5
	beacon-blink-delay 0.3
	beacon-blink-duration 0.3
	beacon-blink-when-point-moves-horizontally 2
	beacon-blink-when-point-moves-vertically 2))
;; * Completion
(add-hook 'after-init-hook 'global-company-mode)
(global-company-mode t)

;; * popwin
(require 'popwin)
(popwin-mode 1)
(setq-default display-buffer-function 'popwin:display-buffer)

;; * dired
(require 'dired-x)
(require 'csb-dired)

;; * Files
(add-to-list 'auto-mode-alist '("\\.page\\'" . org-mode))

;; * Custom
(load custom-file 'noerror)
(put 'dired-find-alternate-file 'disabled nil)
