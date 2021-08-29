;; url: https://github.com/Aaron-Mann/.emacs.d

;; define funct used to load configuration modules

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;; garbage collection

(setq gc-cons-threshold most-positive-fixnum)

;; prioritize non-byte-compiled source files to prevent the use of stale byte-code

(setq load-prefer-newer noninteractive)

;; ensure Toaster is running from the correct file directory
(setq user-emacs-directory (file-name-directory load-file-name))
