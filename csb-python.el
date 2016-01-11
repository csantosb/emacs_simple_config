;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(add-to-list 'auto-mode-alist '("\\.pyw\\'" . python-mode))

(defvar python_version_to_use "3"
  " this variable declares the python flavour to use
must be declared before accessing to the python major mode" )

(with-eval-after-load 'python
  (setq python-shell-interpreter
        (if (string= python_version_to_use "3")
            "ipython"
          "ipython2")
        python-indent-offset 4
        python-shell-interpreter-args "-i --no-banner"
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion"
        python-shell-completion-module-string-code
        "';'.join(module_completion('''%s'''))\n"
        python-shell-completion-string-code
        "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

(with-eval-after-load 'python

    (defun python-add-breakpoint ()
      "Add a break point"
      (interactive)
      (newline-and-indent)
      (insert "import ipdb; ipdb.set_trace()")
      (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

    (defun python-interactive ()
      "Enter the interactive Python environment"
      (interactive)
      (progn
        (insert "!import code; code.interact(local=vars())")
        (move-end-of-line 1)
        (comint-send-input)))

;;    (define-key python-mode-map (kbd "C-c C-b") 'python-add-breakpoint)
;;    (define-key python-mode-map (kbd "C-c C-i") 'python-interactive)
)

(with-eval-after-load 'python

  (defun csb/toggle-python-shell(arg)
    (interactive "P")
    (cond ((string= "inferior-python-mode" major-mode)
           ;; recover window config
           (jump-to-register :python-shell-fullscreen))
          ((get-buffer-window (get-buffer "*Python*"))
           (elpy-shell-switch-to-shell))
          (t
           (progn
             ;; store current window config
             (window-configuration-to-register :python-shell-fullscreen)
             (let ((height (/ (window-total-height) 3))
                   (parent (if (buffer-file-name)
                               (file-name-directory (buffer-file-name))
                             default-directory)))
               (split-window-vertically (- height))
               (other-window 1)
               (if ;; check if a matlab-shell-term already open in current persp
                   (let ((python-shell-exists))
                     (catch 'error
                       (dolist (thisbuffer (persp-buffers persp-curr))
                         (when (and (buffer-name thisbuffer)
                                    (string-prefix-p "*Python" (buffer-name thisbuffer)))
                           (setq python-shell-exists t)
                           (throw 'error t))))
                     python-shell-exists)
                   (switch-to-buffer "*Python*")
                 (progn
                   (run-python)
                   (switch-to-buffer "*Python*")))
               ;; cd to current dir
               (insert (format "cd %s" parent))
               (move-end-of-line 1)
               (comint-send-input))
             (cond ((equal arg '(4))
                    (tiling-tile-up))
                   ((equal arg '(16))
                    (delete-other-windows))
                   (t ))))))

  (define-key python-mode-map (kbd "C-x t s") #'csb/toggle-python-shell))

(with-eval-after-load 'python

  (defun python-shell-send-line()
    "Move point to the beginning of the buffer;
         leave mark at previous position."
    (interactive)
    (save-excursion
      (move-beginning-of-line nil)
      (set-mark-command nil)
      (move-end-of-line nil)
      (call-interactively 'python-shell-send-region)))

  (define-key python-mode-map (kbd "C-c C-l") 'python-shell-send-line)      ;; send line
  (define-key python-mode-map (kbd "C-c C-s") 'python-shell-send-string)    ;; send string command
  (define-key python-mode-map (kbd "C-c C-b") 'python-shell-send-buffer))   ;; send buffer

(with-eval-after-load 'python
  (defun flycheck-info-error ()
    "Search current error message at pylint-messages site.
"
    (interactive)
    (let* ((mes (flycheck-error-message (car (flycheck-overlay-errors-at (point)))))
           (first_par (substring mes -6 -1)))
      (w3m-browse-url (concat "http://pylint-messages.wikidot.com/messages:" first_par))))
  (define-key flycheck-mode-map (kbd "C-c ! /") 'flycheck-info-error))

(with-eval-after-load 'python

     ;; C-c C-p         run-python
     ;; C-c C-v         python-check
     ;;
     ;; remove some conflicting keys
     ;; (dolist (key '("\C-c\C-z" "\C-c s" "\C-c\C-r" "\C-c C-s" "\C-c\C-c" "\C-c C-l" "\C-c C-d"))
     ;;  (local-unset-key key))

     ;; Navigation
     ;; Blocks
     ;; (define-key python-mode-map (kbd "M-a") 'python-nav-beginning-of-block)
     ;; (define-key python-mode-map (kbd "M-e") 'python-nav-end-of-block)
     ;; (define-key python-mode-map (kbd "M-p") 'python-nav-backward-block)
     ;; (define-key python-mode-map (kbd "M-n") 'python-nav-forward-block)
     ;; (define-key python-mode-map (kbd "M-d") 'python-nav-up-list)
     ;; (define-key python-mode-map (kbd "M-u") 'python-nav-backward-up-list)
     ;; (define-key python-mode-map (kbd "M-e") 'python-nav-end-of-defun)
     ;; (define-key python-mode-map (kbd "M-u") 'python-nav-forward-sexp)

     ;; Statements
     ;; (define-key python-mode-map (kbd "M-u") 'python-nav-beginning-of-statement)
     ;; (define-key python-mode-map (kbd "M-u") 'python-nav-end-of-statement)
     ;; (define-key python-mode-map (kbd "M-u") 'python-nav-backward-statement)
     ;; (define-key python-mode-map (kbd "M-u") 'python-nav-forward-statement)

     ;; Indent
     ;; C-c <           python-indent-shift-left
     ;; C-c >           python-indent-shift-right
     ;; C-M-q           prog-indent-sexp
     ;; :               python-indent-electric-colon
     ;; DEL             python-indent-dedent-line-backspace
     ;; <backtab>       python-indent-dedent-line

     ;; Templates
     ;; C-c C-t c       python-skeleton-class
     ;; C-c C-t d       python-skeleton-def
     ;; C-c C-t f       python-skeleton-for
     ;; C-c C-t i       python-skeleton-if
     ;; C-c C-t t       python-skeleton-try
     ;; C-c C-t w       python-skeleton-while
     ;; (define-key python-mode-map (kbd "C-c C-t e") 'python-skeleton--else)
     ;; (define-key python-mode-map (kbd "C-c C-t e") 'python-skeleton--except)
     ;; (define-key python-mode-map (kbd "C-c C-t e") 'python-skeleton--finally)

  )

(add-hook 'python-mode-hook (lambda ()
                              ;; derived from prog-mode plus the following
                              ;; (setq-local orgstruct-heading-prefix-regexp "# ")
                              ;; (add-to-list 'imenu-generic-expression '(nil  "^# *\\(.*\\)" 1))
                              (jedi-mode 1)
                              (setq-local helm-dash-docsets '("Python_3"))
                              (flycheck-select-checker 'python-pylint)))

(add-hook 'inferior-python-mode-hook
          (lambda ()
            (setq-local show-trailing-whitespace nil)
            (define-key inferior-python-mode-map (kbd "C-x t s") #'csb/toggle-python-shell)
            (smartscan-mode 0)
            (define-key inferior-python-mode-map (kbd "C-c C-i") 'python-interactive)))

;; (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
;; (add-hook 'jedi-mode-hook 'jedi-direx:setup)

;; once for every jedi update
;; (eval-after-load 'company '(company-jedi-install))
(with-eval-after-load 'python
  (require 'jedi)
  (require 'jedi-direx))
  ;; (jedi:install-server) <-- after updating jedi

(with-eval-after-load 'jedi
  (define-key jedi-mode-map (kbd "C-x c j") #'helm-jedi-related-names)
  (define-key jedi-mode-map (kbd "C-c C-j r") #'helm-jedi-related-names)
  (define-key jedi-mode-map (kbd "C-c C-j d") #'jedi:show-doc)
  (define-key jedi-mode-map (kbd "C-c C-j j") #'jedi:goto-definition)
  (define-key jedi-mode-map (kbd "C-c C-j b") #'jedi:goto-definition-pop-marker)
  (define-key jedi-mode-map (kbd "C-c C-j x") #'jedi-direx:pop-to-buffer)
  (define-key jedi-mode-map (kbd "C-c C-j s") #'jedi-direx:switch-to-buffer)
  ;;
  (define-key jedi-mode-map (kbd "C-c ?") nil)
  (define-key jedi-mode-map (kbd "C-c ,") nil)
  (define-key jedi-mode-map (kbd "C-c .") nil)
  (define-key jedi-mode-map (kbd "C-c /") nil)
  (define-key jedi-mode-map (kbd "C-c ?") nil))

(autoload 'pylookup-lookup "pylookup" "Lookup SEARCH-TERM in the Python HTML indexes." t)
(autoload 'pylookup-update "pylookup" "Run pylookup-update and create the database at `pylookup-db-file'." t)

(with-eval-after-load 'python

  (setq pylookup-dir "/home/csantos/Dropbox/emacs.d/el-get/pylookup"
        pylookup-html-locations '("http://docs.python.org/3"
                                  "http://docs.scipy.org/doc/numpy/genindex.html"
                                  "http://docs.scipy.org/doc/scipy/reference/genindex.html"
                                  "http://matplotlib.sourceforge.net/genindex.html"
                                  "http://pyqt.sourceforge.net/Docs/PyQt5/genindex.html"
                                  "http://docs.myhdl.org/en/latest/genindex.html")
        pylookup-search-options '("--insensitive" "0" "--desc" "0")  ;; set search option if you want
        pylookup-return-window-config (current-window-configuration)
        pylookup-program  ;; executable file
        (if (string= python_version_to_use "3")
            (concat pylookup-dir "/pylookup.py")
          (concat pylookup-dir "/pylookup.py"))
        pylookup-db-file ;; db file
        (if (string= python_version_to_use "3")
            (concat pylookup-dir "/pylookup_3.db")
          (concat pylookup-dir "/pylookup_2.db")))

  (add-to-list 'load-path pylookup-dir)
  (eval-when-compile (require 'pylookup))  ;; load pylookup when compile time

  (set-window-configuration pylookup-return-window-config))

(with-eval-after-load 'python

  (define-key python-mode-map (kbd "C-c M-l") 'pylookup-lookup)
  (define-key python-mode-map (kbd "C-c M-p") 'pylookup-lookup-at-point)

  (define-key pylookup-mode-map (kbd "SPC")
    #'(lambda()
        (interactive)
        (pylookup-mode-lookup-and-leave)
        (kill-buffer "*Pylookup Completions*")
        (switch-to-buffer "*eww*")))

  (define-key pylookup-mode-map (kbd "RET")
    #'(lambda()
        (interactive)
        (pylookup-mode-lookup-and-leave)
        (kill-buffer "*Pylookup Completions*")
        (switch-to-buffer "*eww*"))))

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-x c P") #'helm-pydoc))

(with-eval-after-load 'elpy
  (setq elpy-rpc-backend "jedi"
        elpy-rpc-python-command "python"
        elpy-modules '(elpy-module-company
                       elpy-module-eldoc
                       elpy-module-pyvenv
                       elpy-module-highlight-indentation
                       elpy-module-yasnippet
                       elpy-module-sane-defaults)))

(with-eval-after-load 'elpy
  ;; (define-key elpy-mode-map (kbd "C-c C-c") #'elpy-shell-send-region-or-buffer)
  (define-key elpy-mode-map (kbd "C-<return>") #'elpy-company-backend)
  ;; (define-key elpy-mode-map (kbd "C-c C-m") #'elpy-importmagic-add-import)
  ;; (define-key elpy-mode-map (kbd "C-c C-r") #'elpy-refactor)
  ;; (define-key elpy-mode-map (kbd "C-c C-d") #'elpy-doc)
  ;; (define-key elpy-mode-map (kbd "C-c C-f") #'elpy-find-file)
  ;;
  (define-key elpy-mode-map (kbd "C-c C-z") nil))  ;; elpy-shell-switch-to-shell

(with-eval-after-load 'python
  (when (require 'elpy nil t)
    (elpy-enable)
    (elpy-use-ipython)))

(with-eval-after-load 'python
  (helm-dash-activate-docset "Python 3"))

(with-eval-after-load 'python
  (setq org-babel-python-command "ipython --no-banner")
  (setq org-babel-python-eoe-indicator "'org_babel_python_eoe'"))

(provide 'csb-python)

;;; csb-python.el ends here
