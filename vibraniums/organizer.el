(defun my/download-pdf-and-file-organize ()
  (interactive)
  (let ((pdf-link (elfeed-entry-link (elfeed-search-selected :single t)))
        (tags (elfeed-meta tags)))
    (if (string-match-p "Medicine" tags)
        (setq destination-folder "~/Document/Article/Medicine/")
      (if (string-match-p "Science" tags)
          (setq destination-folder "~/Document/Article/Science/")
        (setq destination-folder "~/Document/Article/Personal/")))

    (unless (file-directory-p destination-folder)
      (make-directory destination-folder t))

    (let* ((pdf-filename (concat destination-folder
                                   (file-name-nondirectory pdf-link)))
           (entry (org-roam-bibtex-add-pdf-entry pdf-filename)))

      (message "Downloading %s to %s" pdf-link pdf-filename)
      (url-copy-file pdf-link pdf-filename)
      (citar-edit-entry entry))
    ))
(global-set-key (kbd "C-c z p") 'my/download-pdf-and-file-organize)

(provide 'organizer)
