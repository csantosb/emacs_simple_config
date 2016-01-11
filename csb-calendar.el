;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains ...
;;
;;; Code:

(with-eval-after-load 'calfw

    ;; Default setting
    (setq cfw:fchar-junction ?+
          cfw:fchar-vertical-line ?|
          cfw:fchar-horizontal-line ?-
          cfw:fchar-left-junction ?+
          cfw:fchar-right-junction ?+
          cfw:fchar-top-junction ?+
          cfw:fchar-top-left-corner ?+
          cfw:fchar-top-right-corner ?+
          cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap)

    (custom-set-faces
     '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
     '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
     '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
     '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
     '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
     '(cfw:face-grid ((t :foreground "DarkGrey")))
     '(cfw:face-default-content ((t :foreground "#bfebbf")))
     '(cfw:face-periods ((t :foreground "cyan")))
     '(cfw:face-day-title ((t :background "grey10")))
     '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
     '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
     '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
     '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
     '(cfw:face-today ((t :background: "grey10" :weight bold)))
     '(cfw:face-select ((t :background "#2f2f2f")))
     '(cfw:face-toolbar ((t :foreground "steelblue1" :background "gray0")))
     '(cfw:face-toolbar-button-off ((t :foreground "gray50" :weight bold)))
     '(cfw:face-toolbar-button-on ((t :foreground "gray70" :weight bold)))))

(with-eval-after-load 'calfw

  (defmacro csb/cfw-macro (myname mycolor myagenda)
    (make-cfw:source
     :name myname
     :color mycolor
     :data `(lambda (begin end)
              (let ((org-agenda-files
                     (list (concat org-directory ,myagenda))))
                (cfw:org-schedule-period-to-calendar begin end)))))

  (setq csb/cfw-agenda-list (list
                             (csb/cfw-macro "perso" "PaleGoldenrod" "agenda_perso.org")
                             (csb/cfw-macro "reyes" "white" "agenda_reyes.org")
                             (csb/cfw-macro "apc" "OrangeRed" "agenda_apc.org")
                             (csb/cfw-macro "ebex" "BlueViolet" "agenda_ebex.org")
                             (csb/cfw-macro "lisa" "yellow" "agenda_lisa.org")
                             (csb/cfw-macro "qubic" "chartreuse4" "agenda_qubic.org")
                             (csb/cfw-macro "compton" "DeepSkyBlue" "agenda_compton.org")
                             (csb/cfw-macro "athena" "PaleVioletRed" "agenda_athena.org")
                             (csb/cfw-macro "cnrs" "DodgerBlue1" "agenda_cnrs.org"))))

(defun csb/cfw-calendar (arg)
  "Launch cfw-calendar.
With a prefix argument ARG, launch ."
  (interactive "P")
  (require 'calfw-org)
  (persp-switch "calendar")
  (if (not (get-buffer "*cfw-calendar*"))
      (if (equal arg '(4))
          ;;
          (cfw:open-calendar-buffer
           :contents-sources csb/cfw-agenda-list)
        ;;
        (save-excursion
          (let* ((cp (cfw:create-calendar-component-buffer
                      :view 'month
                      :contents-sources csb/cfw-agenda-list
                      :custom-map cfw:org-schedule-map
                      :sorter 'cfw:org-schedule-sorter)))
            (switch-to-buffer (cfw:cp-get-buffer cp))))))
  (persp-set-buffer "*cfw-calendar*")
  (switch-to-buffer "*cfw-calendar*")
  (csb/mode-line-off)
  (call-interactively 'cfw:refresh-calendar-buffer))

(define-key launcher-map "C" #'csb/cfw-calendar)

(with-eval-after-load 'calfw
  (defhydra csb/cfw-capture (:color blue)
    "
Selectc agenda -> _p_erso  _r_eyes  _a_pc  _l_isa  _e_bex  q_u_bic  _a_thena  c_o_mpton  _c_nrs  _q_uit
"
    ("p" (org-capture nil "rp") nil)
    ("r" (org-capture nil "rr") nil)
    ("a" (org-capture nil "ra") nil)
    ("l" (org-capture nil "rl") nil)
    ("e" (org-capture nil "re") nil)
    ("u" (org-capture nil "ru") nil)
    ("t" (org-capture nil "rt") nil)
    ("o" (org-capture nil "ro") nil)
    ("c" (org-capture nil "rc") nil)
    ("q" nil nil)))

(with-eval-after-load 'calfw
  (setq cfw:org-schedule-map
        (cfw:define-keymap
         '(("q"   . bury-buffer)
           ("a"   . cfw:org-open-agenda-day)
           ("C"   . csb/cfw-capture/body)
           ("Q"   . (lambda()
                      (interactive)
                      (kill-this-buffer)
                      (persp-kill (persp-name persp-curr))))
           ("SPC" . cfw:show-details-command)))))

(add-hook 'cfw:calendar-mode-hook
          (lambda ()
            (setq-local show-trailing-whitespace nil)
            (setq-local beacon-size 25)
            (setq-local beacon-blink-when-point-moves-horizontally 2)
            (setq-local beacon-blink-when-point-moves-vertically 2)))

(with-eval-after-load 'calendar

  (setq calendar-week-start-day 1
        calendar-time-display-form
        ;; mark-holidays-in-calendar t  ;; mark dates of holidays in the calendar
        ;; setq european-calendar-style t
        '(24-hours ":" minutes (and time-zone (concat " (" time-zone ")")))
        calendar-day-name-array ["Dimanche" "Lundi" "Mardi" "Mercredi"
                                 "Jeudi" "Vendredi" "Samedi"]
        calendar-month-name-array ["Janvier" "Février" "Mars" "Avril" "Mai"
                                   "Juin" "Juillet" "Août" "Septembre"
                                   "Octobre" "Novembre" "Décembre"])
  ;; calendar-day-name-array ["Domingo" "Lunes" "Martes" "Miércoles"
  ;;                       "Jueves" "Viernes" "Sábado"]
  ;; calendar-month-name-array ["Enero" "Febrero" "Marzo" "Abril" "Mayo"
  ;;                         "Junio" "Julio" "Agosto" "Septiembre"
  ;;                         "Octubre" "Noviembre" "Diciembre"]

  (setq calendar-date-display-form
        '((if dayname (concat dayname ", ")) day " " monthname " " year))

(setq csb/holiday-spanish-holidays
      `(
        ;; (holiday-fixed 1 1 "Año Nuevo")
        (holiday-fixed 1 6 "Reyes Magos - Epiphanie")
        (holiday-fixed 1 18 "Santo Tomás")
        (holiday-fixed 3 19 "San José")
        ;; (holiday-fixed 5 1 "1 de Mayo")
        (holiday-fixed 7 25 "Santiago")
        (holiday-fixed 8 15 "Asunción de la Virgen")
        (holiday-fixed 10 12 "El Pilar")
        ;; (holiday-fixed 11 1 "Todos los Santos")
        (holiday-fixed 12 6 "Dia de la Constitución")
        ;; (holiday-fixed 12 25 "Navidad")
        (holiday-fixed 12 8 "La Inmaculada")))

(defvar holiday-french-holidays nil "French holidays")

(setq holiday-french-holidays
        ;; fetes a date fixe
      `((holiday-fixed 1 1 "Jour de l'an")
        ;; (holiday-fixed 1 6 "Épiphanie")
        (holiday-fixed 2 2 "Chandeleur")
        (holiday-fixed 2 14 "Saint Valentin")
        (holiday-fixed 5 1 "Fête du travail")
        (holiday-fixed 5 8 "Armistice 1945")
        (holiday-fixed 6 21 "Fête de la musique")
        (holiday-fixed 7 14 "Fête nationale")
        ;; (holiday-fixed 8 15 "Assomption (Religieux)")
        (holiday-fixed 11 11 "Armistice 1918")
        (holiday-fixed 11 1 "Toussaint")
        (holiday-fixed 11 2 "Défunts")
        (holiday-fixed 12 25 "Noël")
        ;; fetes a date variable
        (holiday-easter-etc 0 "Pâques")
        (holiday-easter-etc 1 "Lundi de Pâques")
        (holiday-easter-etc 39 "Ascension")
        (holiday-easter-etc 49 "Pentecôte")
        (holiday-easter-etc -47 "Mardi gras")
        (holiday-float 5 0 4 "Fête des mères")
        ;; dernier dimanche de mai ou premier dimanche de juin si c'est le
        ;; même jour que la pentecôte TODO
        (holiday-float 6 0 3 "Fête des pères"))) ;; troisième dimanche de juin

(setq calendar-holidays holiday-french-holidays)
(setq calendar-holidays
        (append calendar-holidays csb/holiday-spanish-holidays )))

(provide 'csb-calendar)

;;; csb-calendar.el ends here
