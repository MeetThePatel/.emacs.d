(use-package osx-trash
  :straight t
  :custom
  (delete-by-moving-to-trash t)
  :init
  (osx-trash-setup))

(use-package exec-path-from-shell
  :straight t
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  (setenv "SHELL" "/bin/zsh")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH")))

(provide 'init-macos)
