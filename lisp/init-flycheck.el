(use-package flycheck
  :straight t
  :init
  (global-flycheck-mode 1))

(use-package langtool
  :straight t
  :config
  (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/5.7/libexec/languagetool-commandline.jar")
  (tyrant-def
    :states 'normal
    :keymaps 'override

    ";" '(:ignore t :which-key "Language Tool")
    "; c" 'langtool-check
    "; ;" 'langtool-correct-buffer
    "; f" 'langtool-check-done
    "; m" 'langtool-show-message-at-point
    "; K" 'langtool-server-stop)
  :init
  (setq langtool-default-language "en-US"))

(use-package langtool-ignore-fonts
  :straight t
  :hook
  (org-mode . langtool-ignore-fonts-minor-mode)
  (LaTeX-mode . langtool-ignore-fonts-minor-mode)
  :config
  (langtool-ignore-fonts-add 'latex-mode '(font-lock-comment-face
					   font-latex-math-face
					   font-latex-string-face))
  (langtool-ignore-fonts-add 'org-mode '(org-code
					 org-latex-and-related
					 org-property-value
					 org-meta-line
					 org-special-keyword)))

(provide 'init-flycheck)
