;; Literally, this file exists to load all our fancy modules

(toaster-log "Loading Toaster modules...")

(toaster-execute-file "modules/evil/evil.el")
(toaster-execute-file "modules/dashboard/dashboard.el")
(toaster-execute-file "modules/modeline/modeline.el")
(toaster-execute-file "modules/clipboard/clipboard.el")
(toaster-execute-file "modules/buffers/buffers.el")

(toaster-log "Modules successfully loaded!")
