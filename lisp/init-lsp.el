(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "s-l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands
  (lsp lsp-deferred))

(provide 'init-lsp)
