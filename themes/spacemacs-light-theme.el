(add-to-list 'load-path "~/.emacs.d/themes/")
(require 'spacemacs-common)

(deftheme spacemacs-light "Spacemacs theme, the light version")

(create-spacemacs-theme 'light 'spacemacs-light)

(provide-theme 'spacemacs-light)
