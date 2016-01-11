;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains defaults for variables as well as global keystrokes

(define-key launcher-map "s"
  (defhydra csb/hydra-search (:color blue)
    "
GREP    _gg_  grep            with user-specified args                                                _q_uit
        _gr_  rgrep           recursively for REGEXP in FILES in directory tree rooted at DIR
        _gl_  lgrep           non-recursively for REGEXP in FILES in directory DIR
        _gz_  zgrep           recursively for REGEXP in gzipped FILES in tree rooted at DIR

OCCUR   _oo_  occur           in open buffer
        _om_  multi occur     searchs in the buffers you specify
        _ob_  multi occur     in matching buffers
        _oB_  multi occur     in buffers with this major mode
        _oO_  org occur       compact tree which shows all matches of REGEXP

     ⤷ _hg_  helm grep       C-x c M-g ...
     ⤷ _ho_  helm occur      C-x c o/O
     ⤷  _s_  helm swoop
     ⤷  _a_  helm ag                 "
    ("gg"  grep nil)
    ("gr"  rgrep nil)
    ("gl"  lgrep nil)
    ("gz"  zgrep nil)
    ("oo" occur nil)
    ("om" multi-occur nil)
    ("ob" multi-occur-in-matching-buffers nil)
    ("oB" multi-occur-in-this-mode nil)
    ("oO" org-occur nil)
    ("hg"  nil nil)
    ("ho"  nil nil)
    ("s"  nil nil)
    ("a"  nil nil)
    ("q" nil nil)))

(eval-after-load "isearch"
  '(defun isearch-del-fail-or-char ()
     "Delete failed isearch text, or if there is none, a single character."
     (interactive)
     (if (isearch-fail-pos)
         (delete-region (isearch-fail-pos) (point))
       (isearch-del-char))))

(eval-after-load "isearch"
  '(define-key isearch-mode-map (kbd "DEL") 'isearch-del-fail-or-char))

(with-eval-after-load "isearch"
  (define-key isearch-mode-map (kbd "M-i") 'avy-isearch))

(define-key isearch-mode-map (kbd "M-i")
  (defhydra csb/hydra-from-isearch (:color blue)
    "
Invoke ...                                                                     ... from isearch.        _q_uit
               _s_wipper          ‘isearch’ with an overview.
      helm-swoo_p_
helm-multi-swoo_P_                Apply all buffers to helm-multi-swoop
               helm-_o_ccur
    helm-multi-_O_ccur
      isearch-o_c_cur             ‘occur’ using the last search string as the regexp "
    ("s" swiper-from-isearch nil)
    ("p" helm-swoop-from-isearch nil)
    ("P" helm-multi-swoop-all-from-isearch nil)
    ("o" helm-occur-from-isearch nil)
    ("O" helm-multi-occur-from-isearch nil)
    ("c" isearch-occur nil)
    ("q" nil nil)))

(stante-after wgrep
  (define-key grep-mode-map (kbd "C-c C-c") 'wgrep-finish-edit)
  (define-key grep-mode-map (kbd "C-o") nil)) ;; free window management key

(defhydra csb/hydra-helm-grep (:color blue)
    "
HELM GREP                                             _q_uit   _<_ back
      helm-do-_g_rep           C-x c M-g g
      helm-do-_z_grep          C-x c M-g z
      helm-do-_p_dfgrep        C-x c M-g p
      helm-do-grep-_a_g        C-x c M-g a
      helm-grep-do-g_i_t-grep  C-x c M-g i"
    ("g" helm-do-grep nil)
    ("z" helm-do-zgrep nil)
    ("p" helm-do-pdfgrep nil)
    ("a" helm-do-grep-ag nil)
    ("i" helm-grep-do-git-grep nil)
    ("<" csb/hydra-search/body nil)
    ("q" nil nil))
  (define-key csb/hydra-search/keymap (kbd "hg") #'csb/hydra-helm-grep/body)

  (define-key helm-grep-mode-map (kbd "<return>") 'helm-grep-mode-jump-other-window)
  (define-key helm-grep-mode-map (kbd "n")        'helm-grep-mode-jump-other-window-forward)
  (define-key helm-grep-mode-map (kbd "p")        'helm-grep-mode-jump-other-window-backward)

(define-key helm-command-map (kbd "M-g g") 'helm-do-grep)
(define-key helm-command-map (kbd "M-g z") 'helm-do-zgrep)
(define-key helm-command-map (kbd "M-g p") 'helm-do-pdfgrep)
(define-key helm-command-map (kbd "M-g i") 'helm-grep-do-git-grep)

(define-key helm-grep-mode-map (kbd "<return>") 'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")        'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")        'helm-grep-mode-jump-other-window-backward)

(defhydra csb/hydra-helm-ag (:color blue)
  "
HELM AG                                           _q_uit   _<_ back
            helm-do-_a_g                 ask for directory and dynamic results
            helm-ag-_t_his-file
         helm-do-ag-_b_uffers
 helm-do-ag-project-_r_oot

               helm-_A_g
            helm-ag-_T_his-file
            helm-ag-_B_uffers
    helm-ag-project-_R_oot  "
  ("a" helm-do-ag nil)
  ("t" helm-do-ag-this-file nil)
  ("b" helm-do-ag-buffers nil)
  ("r" helm-do-ag-project-root nil)
  ("A" helm-ag nil)
  ("T" helm-ag-this-file nil)
  ("B" helm-ag-buffers nil)
  ("R" helm-ag-project-root nil)
  ("<" csb/hydra-search/body nil)
  ("q" nil nil))
(define-key csb/hydra-search/keymap (kbd "a") #'csb/hydra-helm-ag/body)

(require 'replace+)
(stante-after occur
              (define-key occur-mode-map (kbd "C-o") nil) ;; free window management key
              (define-key occur-mode-map (kbd "RET") 'occur-mode-goto-occurrence)
              (define-key occur-mode-map (kbd "o") 'occur-mode-goto-occurrence-other-window)
              ;; (define-key occur-mode-map (kbd "F") 'next-error-follow-minor-mode)
              )
(add-hook 'occur-hook
          (lambda () (interactive)
            (occur-rename-buffer)
            (smartscan-mode -1)))
;; (global-set-key (kbd "C-x O") 'multi-occur)

(define-key occur-edit-mode-map (kbd "C-c C-c") 'occur-cease-edit)
;; (global-set-key (kbd "C-x o") 'occur)
;; (global-set-key (kbd "C-x O") 'multi-occur)

(defhydra csb/hydra-helm-occur (:color blue)
    "
      helm-_o_ccur         C-x c o                     _q_uit   _<_ back
      helm-_m_ulti-occur   C-x c O                     "
    ("o" helm-occur nil)
    ("m" helm-multi-occur nil)
    ("<" csb/hydra-search/body nil)
    ("q" nil nil))

  (define-key csb/hydra-search/keymap (kbd "ho") #'csb/hydra-helm-occur/body)

(define-key helm-command-map (kbd "o") 'helm-occur)
(define-key helm-command-map (kbd "O") 'helm-multi-occur)
(define-key helm-command-map (kbd "M-s o") 'nil)

(eval-when-compile
  (require 'cl))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(defhydra csb/hydra-helm-swoop (:color blue)
    "
SWOOP                                                                              _q_uit   _<_ back
                helm-swoo_p_        C-x c p
          helm-multi-swoo_P_        C-x c P     Select buffers and do ‘helm-swoop‘
        helm-multi-swoop-_a_ll                  Apply 'helm-multi-swoop' to all buffers
        helm-multi-swoop-_o_rg                  Applies all org-mode buffers to helm-multi-swoop
helm-multi-swoop-current-_m_ode                 Applies to all buffers with same mode as current "
    ("p"  helm-swoop nil)
    ("P"  helm-multi-swoop nil)
    ("a"  helm-multi-swoop-all nil)
    ("o"  helm-multi-swoop-org nil)
    ("m"  helm-multi-swoop-current-mode nil)
    ("<"  csb/hydra-search/body nil)
    ("q"  nil nil))

  (define-key csb/hydra-search/keymap (kbd "s") #'csb/hydra-helm-swoop/body)

(define-key helm-command-map (kbd "p") #'helm-swoop)
(define-key helm-command-map (kbd "P") #'helm-multi-swoop)

(with-eval-after-load 'helm-swoop
  (setq helm-swoop-pre-input-function (lambda () nil)))

(with-eval-after-load 'helm-swoop

  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-w") #'helm-multi-swoop-all-from-helm-swoop)

  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)

  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows t)

  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-vertically)

  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color t)

  ;; Go to the opposite side of line from the end or beginning of line
  (setq helm-swoop-move-to-line-cycle t)

  ;; Face name is `helm-swoop-line-number-face`
  (setq helm-swoop-use-line-number-face t))

(with-eval-after-load 'helm-swoop

  ;; helm swoop edit
  (define-key helm-swoop-map (kbd "C-x t r") #'helm-swoop-edit)
  (define-key helm-swoop-edit-map (kbd "C-c C-k") #'helm-swoop--edit-cancel)
  (define-key helm-swoop-edit-map (kbd "C-c C-c") #'helm-swoop--edit-complete)
  ;; remove defaults
  (define-key helm-swoop-edit-map (kbd "C-x C-s") nil)
  (define-key helm-swoop-edit-map (kbd "C-c C-g") nil)

  (define-key helm-swoop-map (kbd "C-c C-e") nil) ;; helm-swoop-edit

  ;; helm multi-swoop
  (define-key helm-multi-swoop-map (kbd "C-x t r") #'helm-multi-swoop-edit)
  (define-key helm-multi-swoop-edit-map (kbd "C-c C-k") #'helm-swoop--edit-cancel)
  (define-key helm-multi-swoop-edit-map (kbd "C-c C-c") #'helm-swoop--edit-complete)
  ;; remove defaults
  (define-key helm-multi-swoop-edit-map (kbd "C-x C-s") nil)
  (define-key helm-multi-swoop-edit-map (kbd "C-c C-g") nil)
  (define-key helm-multi-swoop-map (kbd "C-c C-e") nil))  ;; helm-multi-swoop-edit

(provide 'csb-search)

;;; csb-search.el ends here
