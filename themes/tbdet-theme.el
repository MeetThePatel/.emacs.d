(deftheme tbdet "The Best Damn Emacs Theme.")

(let ((meet/monospace-font "Liga SFMono Nerd Font")
      (meet/sans-serif-font "SF Pro Rounded")
      (meet/prog-font "Iosevka Nerd Font")

      (meet/color-white "#FFFFFF")
      (meet/color-black "#000000")
      (meet/color-light-grey "#BDC3C7")
      (meet/color-dark-grey "#34495E")

      (meet/color-flat-red "#E74C3C")
      (meet/color-flat-light-red "#FFE4E4")
      (meet/color-flat-green "#2ECC71")
      (meet/color-flat-blue "#3498DB")
      (meet/color-flat-yellow "#F1C40F")
      (meet/color-flat-purple "#9B59B6"))

  (custom-theme-set-faces
   'tbdet
   `(default ((t (:foreground ,meet/color-black :background ,meet/color-white))))
   `(org-default ((t (:foreground ,meet/color-black :background ,meet/color-white :family meet/sans-serif-font :height 175 :weight normal))))
   `(region ((t (:background ,meet/color-flat-light-red))))

   `(show-paren-match ((t (:background ,meet/color-flat-light-red))))

   ;; Org Mode
   `(org-document-info-keyword ((t (:foreground ,meet/color-light-grey :background ,meet/color-white :height 125 :weight normal :family ,meet/monospace-font :width condensed))))
   `(org-document-title ((t (:foreground ,meet/color-black :background ,meet/color-white :height 350 :weight bold :underline (:color ,meet/color-black)))))

   `(org-level-1 ((t (:foreground ,meet/color-black :background ,meet/color-white :height 275 :weight semi-bold :family ,meet/sans-serif-font))))
   `(org-level-2 ((t (:foreground ,meet/color-black :background ,meet/color-white :height 275 :weight semi-bold :family ,meet/sans-serif-font))))
   `(org-level-3 ((t (:foreground ,meet/color-black :background ,meet/color-white :height 275 :weight semi-bold :family ,meet/sans-serif-font))))
   `(org-level-4 ((t (:foreground ,meet/color-black :background ,meet/color-white :height 275 :weight semi-bold :family ,meet/sans-serif-font))))
   `(org-level-5 ((t (:foreground ,meet/color-black :background ,meet/color-white :height 275 :weight semi-bold :family ,meet/sans-serif-font))))
   `(org-level-6 ((t (:foreground ,meet/color-black :background ,meet/color-white :height 275 :weight semi-bold :family ,meet/sans-serif-font))))
   `(org-level-7 ((t (:foreground ,meet/color-black :background ,meet/color-white :height 275 :weight semi-bold :family ,meet/sans-serif-font))))
   `(org-level-8 ((t (:foreground ,meet/color-black :background ,meet/color-white :height 275 :weight semi-bold :family ,meet/sans-serif-font))))

   `(org-drawer ((t (:foreground ,meet/color-light-grey :background ,meet/color-white :height 125 :weight normal :family ,meet/monospace-font :width condensed))))
   `(org-special-keyword ((t (:foreground ,meet/color-light-grey :background ,meet/color-white :height 125 :weight normal :family ,meet/monospace-font :width condensed))))
   `(org-property-value ((t (:foreground ,meet/color-light-grey :background ,meet/color-white :height 125 :weight normal :family ,meet/monospace-font :width condensed))))
   `(org-meta-line ((t (:foreground ,meet/color-light-grey :background ,meet/color-white :height 125 :weight normal :family ,meet/monospace-font :width condensed))))

   `(org-ellipsis ((t (:foreground ,meet/color-flat-red :overline nil :underline nil))))

   `(org-latex-and-related ((t (:foreground ,meet/color-dark-grey :background ,meet/color-white :family ,meet/monospace-font :height 150))))
   `(secondary-selection ((t (:foreground ,meet/color-white :background ,meet/color-dark-grey :family ,meet/monospace-font))))

   `(org-link ((t (:foreground ,meet/color-flat-red :background ,meet/color-white :underline t))))

   `(org-block-begin-line ((t (:foreground ,meet/color-black :background ,meet/color-flat-light-red :family ,meet/monospace-font :height 150))))
   `(org-block ((t (:family ,meet/prog-font :height 175))))
   `(org-block-end-line ((t (:foreground ,meet/color-black :background ,meet/color-flat-light-red :family ,meet/monospace-font :height 150))))

   `(org-ref-cite-face ((t (:foreground ,meet/color-black :background ,meet/color-white :underline nil))))
   
   `(mode-line ((t (:foreground ,meet/color-white :background ,meet/color-black :family ,meet/monospace-font :height 150 :weight medium))))
   `(mode-line-inactive ((t (:foreground ,meet/color-white :background ,meet/color-black :family ,meet/monospace-font :height 150 :weight medium))))

   `(doom-modeline ((t (:foreground ,meet/color-white :background ,meet/color-black))))
   `(doom-modeline-inactive ((t (:foreground ,meet/color-white :background ,meet/color-black))))
   `(doom-modeline-bar ((t (:background ,meet/color-flat-red))))
   `(doom-modeline-bar-inactive ((t (:background ,meet/color-flat-red))))
   `(doom-modeline-misc-info ((t (:foreground ,meet/color-white :background ,meet/color-white))))
   `(doom-modeline-modal-icon ((t (:background ,meet/color-black))))

   `(doom-modeline-project-dir ((t (:foreground ,meet/color-flat-blue :background ,meet/color-black :weight bold))))
   `(doom-modeline-buffer-path ((t (:foreground ,meet/color-light-grey :background ,meet/color-black))))
   `(doom-modeline-buffer-file ((t (:foreground ,meet/color-white :background ,meet/color-black :weight bold))))

   `(doom-modeline-buffer-major-mode ((t (:foreground ,meet/color-white :background ,meet/color-black :weight bold))))
   `(doom-modeline-spc-face ((t (:background ,meet/color-black))))
   `(doom-modeline-spc-inactive-face ((t (:background ,meet/color-black))))
   `(doom-modeline-vspc-face ((t (:background ,meet/color-black))))
   `(doom-modeline-vspc-inactive-face ((t (:background ,meet/color-black))))

   `(doom-modeline-evil-normal-state ((t (:foreground ,meet/color-flat-green :background ,meet/color-black))))
   `(doom-modeline-evil-insert-state ((t (:foreground ,meet/color-flat-red :background ,meet/color-black))))
   `(doom-modeline-evil-visual-state ((t (:foreground ,meet/color-flat-blue :background ,meet/color-black))))
   `(doom-modeline-evil-emacs-state ((t (:foreground ,meet/color-flat-yellow :background ,meet/color-black))))

   `(doom-modeline-lsp-success ((t (:foreground ,meet/color-flat-green :background ,meet/color-black))))
   `(doom-modeline-lsp-warning ((t (:foreground ,meet/color-flat-yellow :background ,meet/color-black))))
   `(doom-modeline-lsp-error ((t (:foreground ,meet/color-flat-red :background ,meet/color-black))))

   `(doom-modeline-urgent ((t (:foreground ,meet/color-flat-red :background ,meet/color-black))))
   `(doom-modeline-buffer-modified ((t (:foreground ,meet/color-flat-red :background ,meet/color-black :weight bold))))

   `(compilation-error ((t (:foreground ,meet/color-flat-red :background ,meet/color-black))))
   `(doom-modeline-debug ((t (:foreground ,meet/color-flat-red :background ,meet/color-black))))

   `(doom-modeline-info ((t (:foreground ,meet/color-flat-yellow :background ,meet/color-black))))
   `(doom-modeline-misc-info ((t (:foreground ,meet/color-white :background ,meet/color-black))))
   `(doom-modeline-emphasis ((t (:foreground ,meet/color-white :background ,meet/color-black))))

   `(doom-modeline-buffer-minor-mode ((t (:foreground ,meet/color-white :background ,meet/color-black))))

   `(line-number ((t (:foreground ,meet/color-black :background ,meet/color-white))))
   `(line-number-current-line ((t (:foreground ,meet/color-black :background ,meet/color-flat-light-red :weight bold))))

   `(company-tooltip ((t (:foreground ,meet/color-black :background ,meet/color-white))))
   `(company-tooltip-selection ((t (:foreground ,meet/color-black :background ,meet/color-flat-light-red))))
   `(company-box-scrollbar ((t (:background ,meet/color-flat-red))))

   `(minibuffer-prompt ((t (:foreground ,meet/color-flat-red :background ,meet/color-white))))
   `(vertico-current ((t (:foreground ,meet/color-black :background ,meet/color-flat-light-red))))
   `(marginalia-documentation ((t (:foreground ,meet/color-flat-red :background ,meet/color-white :italic t))))
   `(marginalia-size ((t (:foreground ,meet/color-flat-red :background ,meet/color-white :italic t))))
   `(marginalia-date ((t (:foreground ,meet/color-flat-red :background ,meet/color-white :weight bold))))

   `(fringe ((t (:foreground ,meet/color-black :background ,meet/color-white))))

   `(font-lock-comment-face ((t (:foreground ,meet/color-light-grey :background ,meet/color-white :italic t))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,meet/color-light-grey :background ,meet/color-white :italic t))))
   ))

(provide-theme `tbdet)

(provide 'tbdet-theme)
;;; tbdet-theme.el ends here
