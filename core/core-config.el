;; This file exists to parse the variables in the user config file for Toaster

;; First up, set the theme:
(let ((theme (or (car-safe custom-enabled-themes) toaster-theme)))
      (when theme
	      (mapc #'disable-theme custom-enabled-themes))
          (load-theme toaster-theme 'noconfirm))
