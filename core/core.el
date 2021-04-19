;; First, we're gonna set some variables to govern Toaster's state

(defconst toaster-version "0.0.1"
	  "current version of Toaster Emacs.")

(defvar toaster-init-p nil
  "Non-nil if Toaster has been initialized")

(defvar toaster-init-time nil
  "time, in seconds, that it took for Toaster to initialize.")

(defconst toaster-interactive-p (not noninteractive)
	  "if non-nil, Emacs is in interactive mode")

;; set some vars to be used later, specifically to check if certain plugins/modules will work

(defconst EMACS27+   (> emacs-major-version 26))
(defconst EMACS28+   (> emacs-major-version 27))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

;; Unix tools look for HOME, but this is normally not defined on Windows.
(when (and IS-WINDOWS (null (getenv-internal "HOME")))
    (setenv "HOME" (getenv "USERPROFILE"))
      (setq abbreviated-home-dir nil))

;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please
;; The clipboard's on Windows could be in a wider encoding than utf-8 (likely
;; utf-16), so let Emacs/the OS decide what encoding to use there.
(unless IS-WINDOWS
    (setq selection-coding-system 'utf-8)) ; with sugar on top

;; don't ask to spell out "yes"
(fset 'yes-or-no-p 'y-or-n-p)
 
;; disable backup
(setq backup-inhibited t)
 
;; no noise
(setq ring-bell-function 'ignore)


;; now we set up some fancy variables of our own. In past experiences, I find that debugging lisp is pretty miserable, so this helps us out just a little
(define-error 'toaster-error "error in Toaster Emacs core")
(define-error 'toaster-hook-error "error in Toaster's startup hook" 'toaster-error)
(define-error 'toaster-autoload-error "error in Toaster's autoload system" 'toaster-error)
(define-error 'toaster-module-error "error in Toaster modules" 'toaster-error)
(define-error 'toaster-private-error "error in private configs" 'toaster-error)
(define-error 'toaster-package-error "error in packages" 'toaster-error)


;; Directory variables, makes our collective lives a lot easier later

(defconst toaster-init-dir
	  (cond ((boundp 'user-emacs-directory)
		 user-emacs-directory)
		((boundp 'user-init-directory)
		 user-init-directory)
		(t "~/.emacs.d")))

(defun toaster-execute-file (file)
  (interactive "f")
  "load and execute a file in the current user's configuration directory"
  (load-file (expand-file-name file toaster-init-dir)))


(defun toaster-initialize (&optional force-p)
  "Bootstrap Toaster"
  
  ;; Basically this function is the jumping off point that loads all scripts, gets all modules running, and generally just makes Toaster run

  ;;only initialize Toaster if toaster has not already been initialized:
  (when (or force-p (not toaster-init-p))
    (setq toaster-init-p t)
    
    (toaster-log (concat "Welcome to Toaster " toaster-version ". See github.com/Aaron-Mann/toaster-emacs for more information."))
    
    ;; Load up all the core functions. This basically just means initializing every core/*.el script, in the following order:
    
    ;; 1. Package Manager: External scripts/packages are required to make basically everything else work
    ;; 2. Modules: Get all of our external scripts/packages installed/configured up
    ;; 3. Languages: In toaster, our language system is extensible. See github documentation. Essentially, a user can create a config file for each language, and those languages are loaded.
    ;; 4. Custom: Launch any user-created content.
    ;; 5. Config: Toaster provides a config.el script for users to customize their toaster experience.
    ;; 6. Cleanup: After everything is loaded up, we can go ahead and make everything normal again. IE: enable garbage collection, etc.
    ;; With all that clarified, bombs away!
    (toaster-log "Initializing Package Manager...")
    (toaster-execute-file "core/core-package.el")
    
    (toaster-log "Initializing modules...")
    (toaster-execute-file "core/core-modules.el")

    (toaster-log "Initializing languages...")
    (toaster-execute-file "core/core-languages.el")

    (toaster-log "Loading custom user scripts...")
    (toaster-execute-file "custom.el")

    (toaster-log "Loading Toaster config...")
    (toaster-execute-file "core/core-defaults.el")
    (toaster-execute-file "config.el")
    (toaster-execute-file "core/core-config.el")
    
    (toaster-log "Setup complete. Cleaning up...")
    (toaster-execute-file "core/core-cleanup.el")
    
    (toaster-log "Cleanup complete. Enjoy your Toaster experience! Documentation available at github.com/Aaron-Mann/toaster-emacs.")
    )
)
