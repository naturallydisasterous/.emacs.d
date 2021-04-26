(require 'cl)
(require 'package)

;; set core packages. Note, this package list should not be edited. These are the core toaster dependencies, and removing these could jepardize modules. Rather, use the package install system available in the 'custom.el' file
;; Another hint worth noting: No language related packages are installed here. Rather, they are individually installed in their language modules

(setq toaster-core-packages '(
			      ;; UI:
			      doom-themes
			      doom-modeline
			      dashboard
			      all-the-icons
			      
			      ;; Modes:
			      evil
			      org

			      ;; Tools
			      projectile
			      magit
			     ))

(defun toaster-install-core-packages ()
  (let ((pkgs (remove-if #'package-installed-p toaster-core-packages )))
    (when pkgs
      (toaster-log "Toaster refreshinging Emacs packages database...")
      (package-refresh-contents)
      (toaster-log "Package refresh complete.")
      (dolist (p toaster-core-packages)
	(package-install p)))))


;; prepare emacs package archives:
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(toaster-log "Added GNU ELPA package archive.")

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(toaster-log "Added MELPA package archive.")

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(toaster-log "Added MELPA-Stable package archive.")

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(toaster-log "Added ORG package archive.")

;; initialize emacs packages:
(package-initialize)

;; Install toaster packages

(toaster-install-core-packages)
