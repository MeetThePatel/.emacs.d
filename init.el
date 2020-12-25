(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(use-package diminish)

(require 'org)
(org-babel-load-file (expand-file-name (concat user-emacs-directory "settings.org")))
