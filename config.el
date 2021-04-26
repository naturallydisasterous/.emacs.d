;; If your like me, you can't pick a colorscheme to save your life. This is fine! go ahead and change the Toaster colorscheme with this variable. Available themes:

(setq toaster-theme 'doom-one)

;; Toaster uses EVIL (Extesible VI Layer) by default, cuz vi keybindings are 1. easier and 2. don't give you RSI. However, if you still prefer the default emacs keybindings, we don't blame you! Just go ahead and comment this line:
(evil-mode 1)

;; Toaster provides an optional ergomacs integration. Enable it here by uncommenting the following line. (Note, EVIL mode must be disabled)


;; Enable/Disable Line Numbers:
(global-display-line-numbers-mode 1)

;; This entire operation is about limiting the distance between mind and matter, meaning that the less one has to use the mouse, the better. That being said, if you wish to use the default emacs toolbar, menubar, or scrollbar, they can be activated by uncommenting any of these lines:

;; (menu-bar-mode)

;; (toggle-scroll-bar)

;; (tool-bar-mode)

