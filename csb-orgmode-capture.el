;; package --- Summary
;;
;;; Commentary:
;;
;; This file contains defaults for variables as well as global keystrokes

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-default-notes-file (concat org-directory "todo.org"))
;; add a bookmark pointing at the last stored position
(setq org-capture-bookmark t)

(setq org-capture-templates nil)
(setq org-capture-templates-contexts nil)

(with-eval-after-load 'calfw

  ;; (defmacro csb/testo (key name)
  ;; (add-to-list 'org-capture-templates
  ;;              '(key "agenda toto"
  ;;                entry
  ;;                (file (concat org-directory "agenda_toto.org"))
  ;;                "* %?\n %(cfw:org-capture-day)"
  ;;                :empty-lines 1)))

  ;; (csb/testo "kiki" "toto")

  (add-to-list 'org-capture-templates
               '("ra" "agenda apc"
                 entry
                 (file (concat org-directory "agenda_apc.org"))
                 "* %?\n %(cfw:org-capture-day)"
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               '("rl" "agenda lisa"
                 entry
                 (file (concat org-directory "agenda_lisa.org"))
                 "* %?\n %(cfw:org-capture-day)"
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               '("re" "agenda ebex"
                 entry
                 (file (concat org-directory "agenda_ebex.org"))
                 "* %?\n %(cfw:org-capture-day)"
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               '("ru" "agenda qubic"
                 entry
                 (file (concat org-directory "agenda_qubic.org"))
                 "* %?\n %(cfw:org-capture-day)"
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               '("rt" "agenda athena"
                 entry
                 (file (concat org-directory "agenda_athena.org"))
                 "* %?\n %(cfw:org-capture-day)"
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               '("ro" "agenda compton"
                 entry
                 (file (concat org-directory "agenda_compton.org"))
                 "* %?\n %(cfw:org-capture-day)"
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               '("rc" "agenda cnrs"
                 entry
                 (file (concat org-directory "agenda_cnrs.org"))
                 "* %?\n %(cfw:org-capture-day)"
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               '("rp" "agenda perso"
                 entry
                 (file (concat org-directory "agenda_perso.org"))
                 "* %?\n %(cfw:org-capture-day)"
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               '("rr" "agenda reyes"
                 entry
                 (file (concat org-directory "agenda_reyes.org"))
                 "* %?\n %(cfw:org-capture-day)"
                 :empty-lines 1)))

(with-eval-after-load 'calfw
  (setq org-capture-templates-contexts
        '(("ra" ((in-mode . "cfw:calendar-mode")))
          ("rp" ((in-mode . "cfw:calendar-mode")))
          ("rc" ((in-mode . "cfw:calendar-mode")))
          ("ro" ((in-mode . "cfw:calendar-mode")))
          ("rt" ((in-mode . "cfw:calendar-mode")))
          ("ru" ((in-mode . "cfw:calendar-mode")))
          ("re" ((in-mode . "cfw:calendar-mode")))
          ("rl" ((in-mode . "cfw:calendar-mode"))))))

(add-to-list 'org-capture-templates
             '("j" "firefox bookmarks" entry
               (file
                (if (file-exists-p "~/Projects/perso/wikidata/bookmarks.cat/bookmarks.page")
                    "/home/csantos/Projects/perso/wikidata/bookmarks.cat/bookmarks.page"
                  "/tmp/bookmarks.page"))
               "* %:description  %^g
:PROPERTIES:
:Private: %^{private|Yes|No}
:ReadLater: %^{read later|Yes|No}
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00|5:00}
:END:\n\nDate:  %u  \\\\\nID:    [[file:/bookmarks.cat/%(substring (number-to-string (abs (/ (random) 100000000))) 0 6)][link]] \\\\\nTitle: %c \\\\\nURL:   %:link \\\\\n\nDescription:  %i\n\n%?"
               :empty-lines 1))

(with-eval-after-load 'eww

  (add-to-list 'org-capture-templates
               '("b" "eww bookmarks" entry
                 (file
                  (if (file-exists-p "~/Projects/perso/wikidata/bookmarks.cat/bookmarks.page")
                      "/home/csantos/Projects/perso/wikidata/bookmarks.cat/bookmarks.page"
                    "/tmp/bookmarks.page"))
                 "* %:description  %^g
:PROPERTIES:
:Private: %^{private|Yes|No}
:ReadLater: %^{read later|Yes|No}
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00|5:00}
:END:\n\nDate:  %u  \\\\\nID:    [[file:/bookmarks.cat/%(substring (number-to-string (abs (/ (random) 100000000))) 0 6)][link]] \\\\\nTitle: %c \\\\\nURL:   %:link \\\\\n\nDescription:  %i\n\n%?"
                 :empty-lines 1))

(add-to-list 'org-capture-templates-contexts '("b" ((in-mode . "eww-mode")))))

(add-to-list 'org-capture-templates
             '("t" "Perso Agenda / Afaire"
               entry
               (file+headline (concat org-directory "agenda_perso.org") "Afaire")
               "* TODO %?\n%i\n%a"
               :empty-lines 1
               :prepend ))

(provide 'csb-orgmode-capture)
;;; csb-orgmode-capture.el ends here
