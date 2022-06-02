(global-visual-line-mode 1)
(global-display-line-numbers-mode 1)
(blink-cursor-mode -1)

(column-number-mode 1)
(size-indication-mode 1)

(setq use-file-dialog nil
      use-dialog-box nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

(use-package doom-themes
  :straight t
  :init
  (doom-themes-visual-bell-config)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  (load-theme 'tbdet t))


(use-package doom-modeline
  :straight t
  :config
  (defun doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
    (if (s-contains-p org-roam-directory (or buffer-file-name ""))
	(replace-regexp-in-string
	 "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
	 "(\\1-\\2-\\3) "
	 (subst-char-in-string ?_ ?  buffer-file-name))
      (funcall orig-fun)))

  (advice-add 'doom-modeline-buffer-file-name :around #'doom-modeline--buffer-file-name-roam-aware-a)
  :init
  (setq doom-modeline-height 1
	doom-modeline-bar-width 5
	doom-modeline-buffer-encoding nil
	doom-modeline-modal-icon t
	doom-modeline-major-mode-icon t
	doom-modeline-major-mode-color-icon t)
  (doom-modeline-mode 1)
  (doom-modeline-def-modeline 'main
    '(bar modals window-number matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info checker input-method major-mode process vcs "  ")))

(use-package all-the-icons
  :straight t)

(use-package all-the-icons-completion
  :straight t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package vertico
  :straight t
  :custom
  (vertico-count 15)
  (vertico-resize t)
  (vertico-cycle nil)
  :config
  (advice-add #'vertico--format-candidate :around
            (lambda (orig cand prefix suffix index _start)
              (setq cand (funcall orig cand prefix suffix index _start))
              (concat
               (if (= vertico--index index)
                   (propertize "  " 'face 'vertico-current)
                 "  ")
               cand)))
  :init
  (vertico-mode))

(use-package vertico-directory
  :straight nil
  :after (vertico)
  :load-path "straight/repos/vertico/extensions/"
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-buffer
  :disabled t
  :straight nil
  :after (vertico)
  :load-path "straight/repos/vertico/extensions/"
  :hook (vertico-mode . vertico-buffer-mode))

(use-package vertico-mouse
  :straight nil
  :after (vertico)
  :load-path "straight/repos/vertico/extensions/"
  :hook (vertico-mode . vertico-mouse-mode))

(use-package marginalia
  :straight t
  :after (vertico)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (show-paren-mode 1))

(use-package which-key
  :straight t
  :custom
  (which-key-idle-delay 0.75)
  :init
  (which-key-setup-side-window-bottom)
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package solaire-mode
  :straight t
  :hook (after-init . solaire-global-mode))

(use-package winum
  :straight t
  :init
  (setq winum-auto-setup-mode-line t)
  :general
  ("s-1" 'winum-select-window-1)
  ("s-2" 'winum-select-window-2)
  ("s-3" 'winum-select-window-3)
  ("s-4" 'winum-select-window-4)
  ("s-5" 'winum-select-window-5)
  ("s-6" 'winum-select-window-6)
  ("s-7" 'winum-select-window-7)
  ("s-8" 'winum-select-window-8)
  ("s-9" 'winum-select-window-9)
  :init
  (winum-mode))

(tyrant-def
  :states 'normal
  :keymaps 'override
  "w" '(:ignore t :which-key "Window")
  "w SPC" 'delete-other-windows
  "wd" 'kill-buffer-and-window

  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wl" 'evil-window-right

  "w|" 'split-window-right
  "w-" 'split-window-below

  "w1" 'winum-select-window-1
  "w2" 'winum-select-window-2
  "w3" 'winum-select-window-3
  "w4" 'winum-select-window-4
  "w5" 'winum-select-window-5
  "w6" 'winum-select-window-6
  "w7" 'winum-select-window-7
  "w8" 'winum-select-window-8
  "w9" 'winum-select-window-9
  "w0" 'winum-select-window-0-or-10)

(use-package rainbow-mode
  :straight t
  :hook
  (org-mode . rainbow-mode)
  :config
  (defun rainbow-turn-off-words ()
    "Turn off word colours in rainbow-mode."
    (interactive)
    (font-lock-remove-keywords
     nil
     `(,@rainbow-x-colors-font-lock-keywords
       ,@rainbow-latex-rgb-colors-font-lock-keywords
       ,@rainbow-r-colors-font-lock-keywords
       ,@rainbow-html-colors-font-lock-keywords
       ,@rainbow-html-rgb-colors-font-lock-keywords))))

(use-package expand-region
  :straight t
  :general
  ("C-=" 'er/expand-region)
  ("C-+" 'er/contract-region))

(provide 'init-ui)
