;; so long story short, certain versions of emacs (I'm looking at you Cemacs) tend to load 'init.el' before 'early-init.el'. This makes sure that 'early-init.el' is explicitly loaded first. Also, this keeps Toaster one step ahead of users who choose to load this file directly:
(load (concat (file-name-directory load-file-name) "early-init"))

;; with libraries prepared, we are ready to initialize Toaster. I'd say "let 'er rip!" in true doom fashion, but our mascot is a toaster.... So it has to be said. "Let 'er Toast!"

(toaster-log "Libraries loaded. Starting core...")

(toaster-initialize)

;; Anything past this line was autofilled by emacs under (custom-set-variables)
;; ===========================================================================
