(use-package org
  :straight t
  :hook
  (org-mode . prettify-symbols-mode)
  (org-mode . org-indent-mode)
  (org-mode . (lambda () (display-line-numbers-mode -1)))
  (org-mode . buffer-face-mode)
  :custom
  (org-directory "~/org/")
  (org-ellipsis " ≡ ")
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-src-fontify-natively t)
  (org-highlight-latex-and-related '(latex script entities))
  (org-fontify-quote-and-verse-blocks t)
  :config
  (plist-put org-format-latex-options :scale 1.2))

(use-package org-superstar
  :straight t
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t))

(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
	org-appear-autoentities t))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-directory (file-truename "~/org/roam"))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (tyrant-def
    :states 'normal
    :keymaps 'override

    "n" '(:ignore t :which-key "Org")

    "nr" '(:ignore t :which-key "Org Roam")
    "nrf" 'org-roam-node-find
    "nr <SPC>" 'org-roam-buffer-toggle
    "nrc" 'org-roam-capture
    "nro" 'org-roam-ui-open
    "nrd" 'org-roam-db-sync)
  (tyrant-def
    :states '(normal visual)
    :keymaps 'override

    "nri" 'org-roam-node-insert
    "nrt" 'org-roam-tag-add)
  (setq org-roam-capture-templates '(("d" "default" plain "%?"
				      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							 "#+TITLE: ${title}\n#+SETUPFILE: setupfile.org")
				      :unnarrowed t))))

(use-package org-roam-ui
  :straight t
  :after (org-roam)
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-update-on-save t
	org-roam-ui-follow t
	org-roam-ui-open-on-start t))

(use-package org-ref
  :straight t
  :config
  (setq bibtex-completion-bibliography '("~/Google Drive/My Drive/Library/biblio.bib")
	bibtex-completion-display-formats '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
					    (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
					    (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
					    (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
					    (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
	bibtex-completion-pdf-open-function (lambda (fpath) (call-process "open" nil 0 nil fpath))))

(use-package svg-tag-mode
  :straight t
  :hook (org-mode . svg-tag-mode)
  :config
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
		(svg-lib-progress-bar (/ (string-to-number value) 100.0)
				      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
		(svg-lib-tag (concat value "%")
			     nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
	   (count (float (car seq)))
	   (total (float (cadr seq))))
      (svg-image (svg-lib-concat
		  (svg-lib-progress-bar (/ count total) nil
					:margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
		  (svg-lib-tag value nil
			       :stroke 0 :margin 0)) :ascent 'center)))
  (setq svg-tag-tags `(
		       ;; Org tags
		       (":\\([-_A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
		       (":\\([-_A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))
		       
		       ;; Task priority
		       ("\\[#[A-Z]\\]" . ( (lambda (tag)
					     (svg-tag-make tag :face 'org-priority 
							   :beg 2 :end -1 :margin 0))))

		       ;; Progress
		       ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
							   (svg-progress-percent (substring tag 1 -2)))))
		       ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
							 (svg-progress-count (substring tag 1 -1)))))
		       
		       ;; TODO / DONE
		       ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
		       ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


		       ("\\(\\[\\[cite:&[A-Za-z-]+:\\)" . ((lambda (tag)
							     (svg-tag-make tag
									   :inverse t
									   :beg 8
									   :end -1))))
		       ("\\[\\[cite:&[A-Za-z-]+:\\([0-9]+\\]\\]\\)" . ((lambda (tag)
									 (svg-tag-make tag
										       :end -2))))
		       
		       ;; Active date (with or without day name, with or without time)
		       (,(format "\\(<%s>\\)" date-re) .
			((lambda (tag)
			   (svg-tag-make tag :beg 1 :end -1 :margin 0))))
		       (,(format "\\(<%s \\)%s>" date-re day-time-re) .
			((lambda (tag)
			   (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
		       (,(format "<%s \\(%s>\\)" date-re day-time-re) .
			((lambda (tag)
			   (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

		       ;; Inactive date  (with or without day name, with or without time)
		       (,(format "\\(\\[%s\\]\\)" date-re) .
			((lambda (tag)
			   (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
		       (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
			((lambda (tag)
			   (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
		       (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
			((lambda (tag)
			   (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date)))))))


(provide 'init-org)
