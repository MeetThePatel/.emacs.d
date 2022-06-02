(dolist (dir '("site-lisp" "lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook (lambda ()
				(setq gc-cons-threshold 80000
				      gc-cons-percentage 0.1)))

(recentf-mode 1)

(setq warning-minimum-level :error)

(setq mac-option-modifier 'meta
      mac-right-option-modifier 'meta
      mac-command-modifier 'super
      mac-right-command-modifier 'super)


(setq backup-directory-alist '(("." . "~/.emacs.d/.backups")))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(add-hook 'emacs-startup-hook
	  '(lambda ()
	     (message "Emacs ready in %s with %d garbage collections."
		      (format "%.2f seconds"
			      (float-time
                               (time-subtract after-init-time before-init-time)))
		      gcs-done)))

(require 'early-init-ui)
