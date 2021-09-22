;; so long story short, certain versions of emacs (I'm looking at you Cemacs) tend to load 'init.el' before 'early-init.el'. This makes sure that 'early-init.el' is explicitly loaded first. Also, this keeps me one step ahead of users who choose to load this file directly:
(load (concat (file-name-directory load-file-name) "early-init"))

;; load all modules

(load-user-file "packages.el")

(load-user-file "defaults.el")

(load-user-file "keyboard.el")

;; load all lisp/* submodules

(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))
                 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))

;;(load-directory "~/.emacs.d/lisp/")

;; now that setup is complete, we can put GC back to normal
(setq gc-cons-threshold 800000)
