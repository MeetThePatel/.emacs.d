(use-package ripgrep
  :straight t)

(use-package projectile
  :straight t
  :config
  (tyrant-def
    :states 'normal
    :keymaps 'override

    "p" '(:ignore t :which-key "Projectile")

    "pP" '(projectile-switch-project :which-key "Switch project")
    "po" '(projectile-switch-open-project :which-key "Switch open project")

    "pB" '(projectile-ibuffer :which-key "Project Ibuffer")
    "ps" '(projectile-save-project-buffers :which-key "Save buffers")
    "pK" '(projectile-kill-buffers :which-key "Kill buffers")
    "pg" '(projectile-ripgrep :which-key "Ripgrep")

    "pc" '(projectile-compile-project :which-key "Compile project")
    "pT" '(projectile-test-project :which-key "Test project")
    "p`" '(projectile-run-shell :which-key "Start shell")
    "p~" '(projectile-run-eshell :which-key "Start e-shell"))
  :init
  (setq projectile-indexing-method 'hybrid)
  (projectile-mode 1))

(use-package consult-projectile
  :straight t
  :after (projectile)
  :config
  (tyrant-def
    :states 'normal
    :keymaps 'override

    "pp" '(consult-projectile :which-key "Projectile")
    "pb" '(consult-projectile-switch-to-buffer :which-key "Switch to buffer")
    "pP" '(consult-projectile-switch-project :which-key "Switch project")
    "pr" '(consult-projectile-recentf :which-key "Recentf")
    "pd" '(consult-projectile-find-dir :which-key "Find directory")
    "pf" '(consult-projectile-find-file :which-key "Find file")))

(use-package org-projectile
  :straight t
  :after (projectile org)
  :custom
  (org-projectile-projects-file "~/org/TODO.org")
  :config
  (push (org-projectile-project-todo-entry) org-capture-templates))

(provide 'init-projectile)
