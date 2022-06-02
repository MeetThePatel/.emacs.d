(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t
      default-directory "~/")

(when window-system (progn
		      (set-frame-size (selected-frame) 100 50)
		      (set-frame-position (selected-frame) 0 0)))

(set-face-attribute 'default nil
		    :family "Iosevka Nerd Font"
		    :height 175
		    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
		    :family "Iosevka Nerd Font"
		    :height 175
		    :weight 'medium)

(provide 'early-init-ui)
