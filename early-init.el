;; url: https://github.com/Aaron-Mann/.emacs.d

;; garbage collection

(setq gc-cons-threshold most-positive-fixnum)

;; prioritize non-byte-compiled source files to prevent the use of stale byte-code

(setq load-prefer-newer noninteractive)

;; ensure Toaster is running from the correct file directory
(setq user-emacs-directory (file-name-directory load-file-name))
