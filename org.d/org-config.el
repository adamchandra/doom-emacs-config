;;;  -*- lexical-binding: t; -*-

(defun setup-org-notetaking (notes-root-dir)
  (interactive (list (projectile-project-root)))

  (let* (
         (notes-dir (expand-file-name notes-root-dir))
         (orgs-dir (expand-file-name "orgs" notes-dir))
         (bibs-dir (expand-file-name "bibs" notes-dir))
         (pdfs-dir (expand-file-name "pdfs" notes-dir))
         (master-bib (expand-file-name "master.bib" bibs-dir))
         (master-bib-notes (expand-file-name "master.bib.notes.org" orgs-dir))
         (default-notes (expand-file-name "notes.org" orgs-dir))
         (default-todo (expand-file-name "todo.org" orgs-dir))
         (default-journal (expand-file-name "journal.org" orgs-dir))
         (has-valid-dir-structure (and
                                   (f-dir-p orgs-dir)
                                   (f-dir-p bibs-dir)
                                   (f-dir-p pdfs-dir)
                                   ))
         )
    (progn
      (cond (has-valid-dir-structure
             (progn
               (message (concat "Setup Notetaking in: " notes-dir))
               (setq
                ;; org-mode
                org-directory orgs-dir
                org-default-notes-file default-notes
                org-journal-file default-journal

                ;; org-ref
                org-ref-notes-directory notes-dir
                org-ref-bibliography-notes master-bib-notes
                org-ref-default-bibliography master-bib
                org-ref-pdf-directory pdfs-dir

                ;; helm-bibtex
                helm-bibtex-bibliography master-bib
                helm-bibtex-library-path pdfs-dir
                helm-bibtex-notes-path master-bib-notes
                bibtex-completion-bibliography master-bib
                bibtex-completion-notes-path master-bib-notes
                )
               ))
            (t (message (concat "Invalid dir structure; cannot setup notetaking in: " notes-dir)))
            )
      )
    )
  )
