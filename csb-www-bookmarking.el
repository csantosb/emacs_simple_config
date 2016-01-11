(with-eval-after-load "eww"
  (define-key eww-mode-map "c"
    (lambda()(interactive)
      (if (not (file-exists-p "~/Projects/perso/wikidata/bookmarks.cat/"))
          (message "Sorry, no Bookmarks file found.")
        (progn
          (require 'org-protocol)
          (if (ad-is-advised 'org-protocol-capture)
              (ad-deactivate 'org-protocol-capture))
          (if (ad-is-advised 'org-capture-finalize)
              (ad-deactivate 'org-capture-finalize))
          (if (ad-is-advised 'org-capture-kill)
              (ad-deactivate 'org-capture-kill))
          (setq-local  org-protocol-data-separator "--")
          (org-protocol-capture
           (if (and (region-active-p) (use-region-p))
               (format "b--%s--%s--%s"
                       (plist-get eww-data :url)
                       (plist-get eww-data :title)
                       (buffer-substring-no-properties (region-beginning)
                                                       (region-end)))
             (format "b--%s--%s"
                     (plist-get eww-data :url)
                     (plist-get eww-data :title))))
          (ad-activate 'org-capture-kill)
          (ad-activate 'org-capture-finalize)
          (ad-activate 'org-protocol-capture))))))

(setq org-capture-before-finalize-hook
      (lambda()
        ;; Only gets executed when bookmarking
        (when (search-backward-regexp "^URL:   " nil t)
          (require 'org-attach)
          ;; store web address to my_url
          (let ((current-point (+ 7 (point))))
            (goto-char (- (line-end-position) 3))
            (setq my_url (buffer-substring-no-properties current-point (point))))
          ;; store title to my_title
          (search-backward-regexp "^Title: " nil t)
          (search-forward "][" nil t)
          (let ((current-point (point)))
            (goto-char (- (line-end-position) 5))
            (setq my_title (buffer-substring-no-properties current-point (point))))
          ;; store ID to my_ID
          (search-backward-regexp "^ID:    " nil t)
          (let ((current-point (+ 29 (point))))
            (setq my_ID (buffer-substring-no-properties current-point (+ current-point 6))))
          ;; download contents to temp file my_filename
          ;; insert its contents as a new page, and attach it
          (let* ((my_extension (file-name-extension my_url))
                 (my_filename  (if my_extension
                                   (concat "/tmp/contents." my_extension)
                                 "/tmp/contents.html"))
                 (org-attach-directory
                  "/home/csantos/Projects/perso/wikidata/bookmarks.cat/.bookmarks.page.attachments/"))
            (when (file-exists-p my_filename)
              (delete-file my_filename))
            ;; download
            (url-copy-file my_url my_filename)
            ;; insert
            (with-temp-buffer
              (insert "---\n")
              (insert "format: HTML\n")
              (insert "categories: bookmarks\n")
              (insert "toc: yes\n")
              (insert (format "title: %s\n" my_title))
              (insert "content: \n")
              (insert "...\n\n")
              (insert-file-contents my_filename)
              (write-file (format "/home/csantos/Projects/perso/wikidata/bookmarks.cat/%s.page" my_ID)))
            ;; attach
            (org-attach-attach my_filename nil 'mv)))))

(provide 'csb-www-bookmarking)

;;; csb-www-bookmarking.el ends here
