(with-eval-after-load 'ggtags
  (define-key ggtags-mode-prefix-map "\M-C" #'ggtags-create-tags)
  (define-key ggtags-mode-prefix-map "\M-x" #'ggtags-reload)
  (define-key ggtags-mode-prefix-map "\M-u" nil)
  (define-key ggtags-mode-prefix-map "\M-U" #'ggtags-update-tags))

(with-eval-after-load 'ggtags
  (define-key ggtags-mode-prefix-map "\M-r" #'ggtags-find-reference)
  (define-key ggtags-mode-prefix-map "\M-d" #'ggtags-find-definition))

(with-eval-after-load 'ggtags
  (define-key ggtags-navigation-mode-map "\M-?"
    (defhydra csb/ggtags-navigation (:color blue)
      "
   Manage              ^^Error             ^^^Misc
------------------------------------------------------------------------
_M-n_  next          _M->_  last       _M-o_    visible mode     _M-*_  abort       _q_uit
_M-p_  prev          _M-<_  firts      _M-s s_  isearch          _RET_  done
_M-}_  next file
_M-{_  prev file
_M-=_  orig file
  "
      ("M-n" next-error nil)
      ("M-p" previous-error nil)
      ("M-*" ggtags-navigation-mode-abort nil)
      ("M->" ggtags-navigation-last-error nil)
      ("M-<" first-error nil)
      ("RET" ggtags-navigation-mode-done nil)
      ("M-}" ggtags-navigation-next-file nil)
      ("M-{" ggtags-navigation-previous-file nil)
      ("M-=" ggtags-navigation-start-file nil)
      ("M-o" ggtags-navigation-visible-mode nil)
      ("M-s s" ggtags-navigation-isearch-forward nil)
      ("q" nil nil))))

(setq csb/ggtags-header-line '((" "
              ((:propertize "M-n" face mode-line-buffer-id)
               ("/"
                (:propertize "p" face mode-line-buffer-id)))
              "")
             (" "
              ((:propertize "M-}" face mode-line-buffer-id)
               ("/"
                (:propertize "{" face mode-line-buffer-id)))
              " file")
             (" "
              ((:propertize "M-=" face mode-line-buffer-id)
               (" orig "
                (:propertize "M-*" face mode-line-buffer-id)))
              " abort")
             (" "
              ((:propertize "M-<" face mode-line-buffer-id)
               ("/"
                (:propertize ">" face mode-line-buffer-id)))
              " first/last")
             (" "
              ((:propertize "M-o" face mode-line-buffer-id))
              " Visible")
             (" "
              ((:propertize "M-s s" face mode-line-buffer-id))
              "earch")
             (" "
              ((:propertize "RET" face mode-line-buffer-id))
              " Done")
             (" "
              ((:propertize "M-?" face mode-line-buffer-id))
              " Hydra")))

(defun csb/ggtags-toggle-header-line ()
  "toggles the mode modeline on and off"
  (interactive)
  ;; check
          (if (not (equal header-line-format csb/ggtags-header-line))
          (setq header-line-format-save header-line-format))
      (setq header-line-format
            (if (equal header-line-format csb/ggtags-header-line)
                (eval 'header-line-format-save)
              csb/ggtags-header-line))
      (redraw-display)
      (force-mode-line-update))

(add-hook 'ggtags-navigation-mode-hook
          'csb/ggtags-toggle-header-line)

(defun csb/ggtags-find-tag-continue ()
  (interactive)
  (when (and (buffer-live-p ggtags-global-last-buffer)
             (with-current-buffer ggtags-global-last-buffer
               (derived-mode-p 'ggtags-global-mode)))
  (csb/ggtags-toggle-header-line))
  (ggtags-find-tag-continue first-time))

(with-eval-after-load 'ggtags
  (define-key ggtags-navigation-mode-map "\M-*" #'ggtags-navigation-mode-abort)
  ;; (define-key ggtags-mode-map (kbd "C-c M-c") #'csb/tags-loop-continue)
  (define-key ggtags-mode-map (kbd "C-c M-c") #'ggtags-find-tag-continue)) ;; xref-find-definitions

(with-eval-after-load 'ggtags
  (define-key ggtags-mode-map (kbd "C-c M-_") #'ggtags-toggle-project-read-only)
  (define-key ggtags-mode-map (kbd "C-c M-j") nil)
  ;;(define-key ggtags-mode-map (kbd "C-c M-") #'ggtags-show-definition)
  )

(with-eval-after-load 'ggtags
  (defun csb/ggtags-browse (arg)
    (interactive "P")
    (ggtags-update-tags)
    (when (equal arg '(4))
      (ggtags-with-current-project (ggtags-process-string "htags")))
    (call-interactively 'ggtags-browse-file-as-hypertext))
  (define-key ggtags-mode-map (kbd "C-c M-t") 'csb/ggtags-browse)
  (define-key ggtags-mode-map (kbd "C-c M-b") nil))

(with-eval-after-load 'ggtags
  (define-key ggtags-mode-prefix-map "\M-?"
    (defhydra csb/hydra-ggtags (:color blue)
      "
   Manage           Find           Tag Mark                     Misc
--------------------------------------------------------------------------------------------------
_M-C_  create       _M-f_  file        _M-n_  nex       _M-t_  html browse     _M-c_  resume from RET      _q_uit
_M-u_  update       _M-g_  grep        _M-p_  prev      _M-k_  kill buffers    _M-h_  tag history
_M-<delete>_        _M-i_  idutils     ^   ^            _M-j_  query replace   _M-/_  search history
_M-x_  reload       _M-._  dwim        _M-,_  pop       _M-R_  prj ro
                  ^^_M-r_  ref         ^   ^
                  ^^_M-d_  def         ^   ^             "
      ("M-C" ggtags-create-tags nil)
      ("M-h" ggtags-view-tag-history nil)
      ("M-/" ggtags-view-search-history nil)
      ("M-u" ggtags-update-tags nil)
      ("M-<delete>" ggtags-delete-tags nil)
      ("M-x" ggtags-reload nil)
      ("M-f" ggtags-find-file nil)
      ("M-g" ggtags-grep nil)
      ("M-i" ggtags-find-idutils nil)
      ("M-." ggtags-dwim nil)
      ("M-r" ggtags-find-reference nil)
      ("M-d" ggtags-find-definition nil)
      ("M-n" ggtags-next-mark nil)
      ("M-p" ggtags-prev-mark nil)
      ("M-," pop-tag-mark nil)
      ("M-R" ggtags-toggle-project-read-only nil)
      ("M-t" csb/ggtags-browse nil)
      ("M-k" ggtags-kill-file-buffers nil)
      ("M-j" ggtags-query-replace nil)
      ;; ("M-c" csb/tags-loop-continue nil) ;; xref-find-definitions
      ("M-c" csb/ggtags-find-tag-continue nil) ;; xref-find-definitions
      ("q" nil nil))))

(add-hook 'ggtags-mode-hook
          (lambda()
            (interactive)
            (require 'company-gtags)
            ;; make `company-backends' local is critcal
            ;; or else, you will have completion in every major mode, that's very annoying!
            (make-local-variable 'company-backends)
            ;; company-ggtags is the plugin to complete words
            (add-to-list 'company-backends 'company-gtags)))

(provide 'csb-project-management-ggtags)
;;; csb-project-management-ggtags.el ends here
