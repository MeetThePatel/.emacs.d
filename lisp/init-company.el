(use-package company
  :straight t
  :hook (after-init . global-company-mode))

(use-package company-box
  :straight t
  :after (company)
  :custom
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  :hook (company-mode . company-box-mode))

(use-package company-dict
  :straight t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-dict))

(provide 'init-company)
