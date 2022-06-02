(use-package evil
  :straight t
  :init
  (setq evil-want-integration nil
	evil-want-keybinding nil
	evil-respect-visual-line-mode t
	evil-want-fine-undo t
	evil-undo-system 'undo-tree)
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :after (evil)
  :init
  (evil-collection-init))

(use-package evil-surround
  :straight t
  :after (evil)
  :init
  (global-evil-surround-mode 1))

(use-package evil-embrace
  :straight t
  :after (evil evil-surround)
  :init
  (setq evil-embrace-show-help-p t)
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-commentary
  :straight t
  :after (evil)
  :init
  (evil-commentary-mode 1))

(use-package evil-goggles
  :straight t
  :after (evil)
  :init
  (evil-goggles-mode 1)
  :config
  (evil-goggles-use-diff-faces))

(use-package avy
  :straight t)

(use-package evil-avy
  :straight t
  :after (evil avy)
  :init
  (evil-avy-mode 1))

(use-package undo-tree
  :straight t
  :custom
  (undo-tree-history-directory-alist  '(("." . "~/.emacs.d/undo")))
  :init
  (global-undo-tree-mode 1))

(use-package general
  :straight t
  :config
  (general-auto-unbind-keys)
  (general-create-definer tyrant-def
    :states '(normal insert visual motion emacs)
    :keymaps 'override
    :prefix "SPC")
  (general-create-definer local-leader-def
    :states '(normal insert visual motion emacs)
    :prefix "SPC m")
  :init
  (general-evil-setup))

(tyrant-def
  :states 'normal
  :keymaps 'override

  "h" '(:ignore t :which-key "Help")

  "hf" 'describe-function
  "hF" 'describe-face
  "hv" 'describe-variable
  "hP" 'describe-package
  "hk" 'describe-key)

(use-package evil-tex
  :straight t
  :after (evil)
  :hook
  (LaTeX-mode . evil-tex-mode)
  (latex-mode . evil-tex-mode)
  (TeX-mode . evil-tex-mode)
  (tex-mode . evil-tex-mode))

(provide 'init-evil)
