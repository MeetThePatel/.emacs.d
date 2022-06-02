(local-leader-def
  :states '(normal visual)
  :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)

  "e" '(:ignore t :which-key "Evaluate")
  "er" '(eval-region :which-key "Evaluate Region")
  "eb" '(eval-buffer :which-key "Evaluate Buffer")
  "ed" '(eval-defun :which-key "Evaluate Defun")
  "es" '(eval-last-sexp :which-key "Evaluate last S-Expression"))

(provide 'init-elisp)
