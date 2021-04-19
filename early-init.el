;; url: https://github.com/Aaron-Mann/toaster-emacs

;;enable Toaster logging message

(defun toaster-log (txt)
  (message (concat "Toaster: " txt))
)

(toaster-log "Initializing...")

;; garbage collection

(toaster-log "Raising garbage collection threshold...")

(defun toaster-raise-gc-threshold ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun toaster-normal-gc-threshold ()
  (setq gc-cons-threshold 800000))

(toaster-raise-gc-threshold)

(toaster-log "Done.")

;; prioritize non-byte-compiled source files to prevent the use of stale byte-code

(setq load-prefer-newer noninteractive)

;; ensure Toaster is running from the correct file directory
(setq user-emacs-directory (file-name-directory load-file-name))

;; load up the core Toaster libraries

(toaster-log "Loading core libraries...")

(load (concat user-emacs-directory "core/core") nil 'nomessage)
