(with-eval-after-load 'help-mode
     (define-key help-mode-map (kbd "k") 'scroll-down-line)
     (define-key help-mode-map "j" 'scroll-up-line)
     (define-key help-mode-map "j" 'scroll-up-line))
(add-hook 'help-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))

;; C-c              Prefix Command
;; TAB              forward-button
;;   (that binding is currently shadowed by another mode)
;; RET              help-follow
;; ESC              Prefix Command
;; SPC              scroll-up-command
;; -                negative-argument
;; 0 .. 9           digit-argument
;; <                beginning-of-buffer
;; >                end-of-buffer
;; ?                describe-mode
;; Q                kill-this-buffer
;; g                revert-buffer
;; h                describe-mode
;; j                scroll-up-line
;; k                scroll-down-line
;; l                help-go-back
;; o                ace-link-help
;; q                quit-window
;; r                help-go-forward
;; DEL              scroll-down-command
;; S-SPC            scroll-down-command
;; <XF86Back>       help-go-back
;; <XF86Forward>    help-go-forward
;; <backtab>        backward-button
;; <mouse-2>        help-follow-mouse
;; <remap>          Prefix Command

;; C-c C-b          help-go-back
;; C-c C-c          help-follow-symbol
;; C-c C-f          help-go-forward

(require 'help+)
;;(require 'help-mode+)
;; (require 'help-fns+)

(setq apropos-sort-by-scores t)

(defhydra hydra-apropos (:color red
                           :hint nil
                           :exit t)
    "
_q_        _h_elm-apropos   _c_ommand
^ ^        _d_ocumentation  _l_ibrary
^ ^        _v_ariable       _u_ser-option
^ ^    valu_e_              _a_propos"
    ("h" helm-apropos)
    ("d" apropos-documentation)
    ("v" apropos-variable)
    ("c" apropos-command)
    ("l" apropos-library)
    ("u" apropos-user-option)
    ("e" apropos-value)
    ("a" apropos)
    ("q" hydra-help/body))

(with-eval-after-load 'info

  ;; help
  (define-key Info-mode-map (kbd "?") 'hydra-iinfo/body)
  (define-key Info-mode-map (kbd "a") 'info-apropos)
  (define-key Info-mode-map (kbd "m") 'Info-menu)
  (define-key Info-mode-map (kbd "i") 'Info-index)

  ;; search
  (define-key Info-mode-map (kbd "s") 'Info-search)
  (define-key Info-mode-map (kbd "S") 'Info-search-case-sensitively)

  ;; navigation up
  (define-key Info-mode-map (kbd "u") 'Info-up)
  (define-key Info-mode-map (kbd "U") 'Info-top-node)
  (define-key Info-mode-map (kbd "M-u") 'Info-directory)

  ;; (define-key Info-mode-map (kbd "G") 'Info-final-node)

  ;; navigation forward /backward
  (define-key Info-mode-map (kbd "f") 'Info-next)
  (define-key Info-mode-map (kbd "b") 'Info-prev)

  (define-key Info-mode-map (kbd "n") 'Info-forward-node)
  (define-key Info-mode-map (kbd "p") 'Info-backward-node)
  ;;
  (define-key Info-mode-map (kbd "g") 'Info-goto-node)

  ;; scrolling
  (define-key Info-mode-map (kbd "<") 'beginning-of-buffer)
  (define-key Info-mode-map (kbd ">") 'end-of-buffer)

  (define-key Info-mode-map (kbd "SPC") 'Info-scroll-up)
  (define-key Info-mode-map (kbd "M-SPC") 'Info-scroll-down)

  (define-key Info-mode-map (kbd "j") 'scroll-up-line)
  (define-key Info-mode-map (kbd "k") 'scroll-down-line)

  ;; history
  (define-key Info-mode-map (kbd "H") 'Info-history-back)
  (define-key Info-mode-map (kbd "L") 'Info-history-forward)
  (define-key Info-mode-map (kbd "M-h") 'Info-history)
  (define-key Info-mode-map (kbd "F") 'Info-follow-reference)
  (define-key Info-mode-map (kbd "T") 'Info-toc)
  (define-key Info-mode-map (kbd "t") nil)

  ;; removed keys
  (define-key Info-mode-map (kbd "]") nil)
  (define-key Info-mode-map (kbd "[") nil)
  (define-key Info-mode-map (kbd "d") nil)

  ;; quit
  (define-key Info-mode-map (kbd "q") 'Info-exit)
  (define-key Info-mode-map (kbd "Q") 'kill-this-buffer))

(defhydra hydra-iinfo (:color blue
                      :hint nil)
      "

  ^^_f_ forward  (same level)          _H_ last (←)      _u_p (↑)                         _F_ollow reference       ^ ^
  ^^_b_ backward (same level)          _L_ next (→)      _U_pper                          _i_ndex                  ^ ^
  ^^_n_ext (logical node)              _M-h_istory       _M-u_ Info dir                   _,_ next index item      _c_opy node name
  ^^_p_rev (logical node)              ^   ^             _m_enu (↓) (C-u for new window)  virtual _I_ndex          _C_lone buffer
  regex _s_earch (_S_ case sensitive)  _>_ final         _g_oto (C-u for new window)          ^^                   _a_propos
  ^^             ^^                    ^ ^               _T_OC

  _1_ .. _9_ Pick first .. ninth item in the node's menu. "
      ("n"   Info-forward-node)
      ("p"   Info-backward-node)
      ("f"   Info-next)
      ("b"   Info-prev)
      ("s"   Info-search)
      ("S"   Info-search-case-sensitively)

      ;; history
      ("H"   Info-history-back)
      ("L"   Info-history-forward)
      ("M-h" Info-history)

      ;; navigating
      ("m"   Info-menu)
      ("u"   Info-up)
      ("M-u" Info-directory)
      ("U"   Info-top-node)
      ("g"   Info-goto-node)
      ("T"   Info-toc)

      (">"   Info-final-node)

      ;;
      ("<"   beginning-of-buffer)
      (">"   end-of-buffer)

      ("F"   Info-follow-reference)
      ("i"   Info-index)
      (","   Info-index-next)
      ("I"   Info-virtual-index)

      ("c"   Info-copy-current-node-name)
      ("C"   clone-buffer)
      ("a"   info-apropos)

      ("1"   Info-nth-menu-item)
      ("2"   Info-nth-menu-item)
      ("3"   Info-nth-menu-item)
      ("4"   Info-nth-menu-item)
      ("5"   Info-nth-menu-item)
      ("6"   Info-nth-menu-item)
      ("7"   Info-nth-menu-item)
      ("8"   Info-nth-menu-item)
      ("9"   Info-nth-menu-item)

      ("?"   Info-summary "Info summary")
      ("h"   Info-help "Info help")
      ("q"   Info-exit "Info exit")
      ("C-g" nil "cancel" :color blue))

(add-hook 'Info-mode-hook
          (lambda ()
            (company-quickhelp-mode -1)
            (csb/mode-line-off)
            (setq-mode-local info-mode show-trailing-whitespace nil)
            (setq-local show-trailing-whitespace nil)))

(with-eval-after-load 'info
  (require 'info-look)
  (info-lookup-add-help
   :mode 'python-mode
   :regexp "[[:alnum:]_]+"
   :doc-spec
   '(("(python)Index" nil ""))))

(defun info-helm ()
  (interactive)
  (helm :sources '((name . "Info manuals")
                   (candidates . helm-default-info-index-list )
                   (action . (("Open" . (lambda(candidate)
                                          (call-interactively
                                           (intern (format "helm-info-%s" candidate))))))))
        :buffer "*helm info*"))

(defun info-mu4e()
  (interactive)
  (require 'mu4e)
  (if (get-buffer "*Mu4e Info*")
      (switch-to-buffer "*Mu4e Info*")
    (info "mu4e" "*Mu4e Info*")))

(defadvice info-display-manual (after info-display-manual-adv activate)
  (kill-this-buffer)
  (let ((bufname (format "*info-%s*" manual)))
    (if (get-buffer bufname)
        (switch-to-buffer bufname)
      (info manual bufname))))

(defhydra hydra-info (:color red
                        :hint nil
                        :exit t)
    "
     _h_elm-info          - browse tags within info node    info-_O_ther-window   - info in another window
     _H_elm-info-at-point - browse tag at point            info-m_u_4e            - mu4e node
info-_l_ookup-symbol      - symbol lookup                   info-_m_anual-display - display several manual nodes
     _i_nfo               - info                       helm-info-_o_rg            - helm info org "
    ("h" info-helm)
    ("H" helm-info-at-point)
    ("l" info-lookup-symbol)
    ("O" info-other-window)
    ("i" info)
    ("o" helm-info-org)
    ("u" info-mu4e)
    ("m" info-display-manual)
    ("q" hydra-help/body))

(defhydra hydra-describe (:color red
                                   :hint nil
                                   :exit t)
    "
_q_     _k_ey         - key          C-h k
^ ^     _b_indings    - bindings     C-h b
^ ^     _m_ode        - mode         C-h m
^ ^     _M_ajor       - my major     C-h C-m
^ ^     _f_unction    - function     C-h f
^ ^     _v_ariable    - variable     C-h v "
    ("k" describe-key)
    ("b" describe-bindings)
    ("m" describe-mode)
    ("M" discover-my-major)
    ("f" describe-function)
    ("v" describe-variable)
    ("q" hydra-help/body))

  ;; (global-set-key (kbd "C-h d") #'hydra-describe/body)

(defhydra hydra-help-find (:color red
                                   :hint nil
                                   :exit t)
    "
_q_        _f_unction      - function      C-h F
^ ^        _k_ey           - key           C-h W
^ ^        _l_ibrary       - library       C-h l
^ ^        _w_here is      - where is      C-h w "
    ("f" find-function)
    ("k" find-function-on-key)
    ("l" find-library)
    ("w" where-is)
    ("q" hydra-help/body))

(defhydra hydra-help (:color red
                             :hint nil
                             :exit t )
  "
      _i_nfo          - info
      _a_propos       - apropos
      _d_escribe      - describe
      _f_ind          - find
  "
  ("i" hydra-info/body)
  ("a" hydra-apropos/body)
  ("d" hydra-describe/body)
  ("f" hydra-help-find/body))

(global-set-key (kbd "C-h h") #'hydra-help/body)

(global-set-key (kbd "C-h m") 'describe-mode)
(global-set-key (kbd "C-h f") 'describe-function)
(global-set-key (kbd "C-h v") 'describe-variable)
;;
(global-set-key (kbd "C-h F") 'find-function)
(global-set-key (kbd "C-h W") 'find-function-on-key)
(global-set-key (kbd "C-h l") 'find-library)
;;
(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-x C-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-h w") 'where-is)
;; (global-set-key (kbd "C-h d") 'elisp-slime-nav-describe-elisp-thing-at-point)
(global-set-key (kbd "C-h C-h") 'help-for-help)
(global-set-key (kbd "C-x C-c") nil)
;; (global-set-key (kbd "C-h b") 'describe-bindings) ;; overloaded by helm-descbinds

(provide 'csb-help)
;;; csb-help.el ends here
