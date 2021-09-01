;; This file, in no particular order, fixes all the annoying defaults of emacs

;; Dialog Popups
(setq use-dialog-box nil)

;; Don't dump on my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Contrary to what many Emacs users have in their configs, you don’t need more than this to make UTF-8 the default coding system:
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ;; pretty
(prefer-coding-system 'utf-8)            ;; pretty
(setq locale-coding-system 'utf-8)       ;; please

;; System Clipboard
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;; Remove useless UI elements
(menu-bar-mode -1)

(toggle-scroll-bar -1)

(tool-bar-mode -1)

;; use frickin line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; This is tricky. file backups are helpful, but i have having my folders fulllll of them, so this creates an emacs backup file directory and sends all backups to that dir
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
backup-by-copying t    ; Don't delink hardlinks
version-control t      ; Use version numbers on backups
delete-old-versions t  ; Automatically delete excess backups
kept-new-versions 20   ; how many of the newest versions to keep
kept-old-versions 5    ; and how many of the old
)

;; splash screen
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)


(setq initial-scratch-message
      (concat

       ";; EEEEEEEEEEEEEEEEEEEEEEMMMMMMMM               MMMMMMMM               AAA                  CCCCCCCCCCCCC   SSSSSSSSSSSSSSS\n" 
       ";; E::::::::::::::::::::EM:::::::M             M:::::::M              A:::A              CCC::::::::::::C SS:::::::::::::::S\n" 
       ";; E::::::::::::::::::::EM::::::::M           M::::::::M             A:::::A           CC:::::::::::::::CS:::::SSSSSS::::::S\n" 
       ";; EE::::::EEEEEEEEE::::EM:::::::::M         M:::::::::M            A:::::::A         C:::::CCCCCCCC::::CS:::::S     SSSSSSS\n" 
       ";;   E:::::E       EEEEEEM::::::::::M       M::::::::::M           A:::::::::A       C:::::C       CCCCCCS:::::S\n" 
       ";;   E:::::E             M:::::::::::M     M:::::::::::M          A:::::A:::::A     C:::::C              S:::::S\n"
       ";;   E::::::EEEEEEEEEE   M:::::::M::::M   M::::M:::::::M         A:::::A A:::::A    C:::::C               S::::SSSS\n"
       ";;   E:::::::::::::::E   M::::::M M::::M M::::M M::::::M        A:::::A   A:::::A   C:::::C                SS::::::SSSSS\n"
       ";;   E:::::::::::::::E   M::::::M  M::::M::::M  M::::::M       A:::::A     A:::::A  C:::::C                  SSS::::::::SS\n"
       ";;   E::::::EEEEEEEEEE   M::::::M   M:::::::M   M::::::M      A:::::AAAAAAAAA:::::A C:::::C                     SSSSSS::::S\n"
       ";;   E:::::E             M::::::M    M:::::M    M::::::M     A:::::::::::::::::::::AC:::::C                          S:::::S\n"
       ";;   E:::::E       EEEEEEM::::::M     MMMMM     M::::::M    A:::::AAAAAAAAAAAAA:::::AC:::::C       CCCCCC            S:::::S\n"
       ";; EE::::::EEEEEEEE:::::EM::::::M               M::::::M   A:::::A             A:::::AC:::::CCCCCCCC::::CSSSSSSS     S:::::S\n"
       ";; E::::::::::::::::::::EM::::::M               M::::::M  A:::::A               A:::::ACC:::::::::::::::CS::::::SSSSSS:::::S\n"
       ";; E::::::::::::::::::::EM::::::M               M::::::M A:::::A                 A:::::A CCC::::::::::::CS:::::::::::::::SS\n"
       ";; EEEEEEEEEEEEEEEEEEEEEEMMMMMMMM               MMMMMMMMAAAAAAA                   AAAAAAA   CCCCCCCCCCCCC SSSSSSSSSSSSSSS\n"
       "\n"
       ";; This buffer is for text that is not saved, and for Lisp evaluation.\n"
       ";; To evaluate elisp, navigate to end of buffer, then C-x C-e.\n"
       ";;\n"
       ";; Shortcuts:\n"
       ";;\n"
       ";; Open File:        C-x C-f\n"
       ";; Open Terminal:    C-x C-t\n"
       ";; Run Command:      M-x\n"
       ";; Cancel Operation: C-g\n"
       ";; Find in File:     C-c h o\n"
       ";; Project Tree:     C-x t t\n"
       ";;\n"
       ";; Windows:\n"
       ";;\n"
       ";; New Window:       C-w C-n\n"
       ";; Vertical Split:   C-w C-v\n"
       ";; Horizontal Split: C-w C-s\n"
       ";; Close Window:     C-w C-c\n"
       ";; Navigate Windows: C-w hjkl\n"
       ";; Cycle Windows:    C-w C-w\n"
       ";;\n"
       ";; Buffers:\n"
       ";;\n"
       ";; Next Buffer:      C-x -->\n"
       ";; Previous Buffer:  C-x <--\n"
       ";; View Buffers:     C-x C-b\n"
       ";; Switch Buffer:    C-x b\n"
       ))

(show-paren-mode 1)
(setq show-paren-delay 0)

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)

(setq visible-bell t)
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq custom-safe-themes t)
(column-number-mode t)
(setq tab-width 2)
(setq tramp-default-method "ssh")
(setq tramp-syntax 'simplified)
