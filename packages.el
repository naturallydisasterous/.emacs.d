;; Leave this here, or package.el will just add it again.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; --------

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package evil
  :ensure t
  :config 
  (evil-mode 1)
  )

(use-package all-the-icons
  :ensure t
  :defer t)

(use-package all-the-icons-dired
  :ensure t
  :defer t)

(use-package dired
  :defer t
  :config
  (require 'dired-x)
  (setq dired-omit-files "^\\.?#\\|^\\.[^.].*")

  (defun air-dired-buffer-dir-or-home ()
    "Open dired to the current buffer's dir, or $HOME."
    (interactive)
    (let ((cwd (or (file-name-directory (or (buffer-file-name) ""))
                   (expand-file-name "~"))))
      (dired cwd)))

  (add-hook 'dired-mode-hook (lambda ()
                               (dired-omit-mode t)
                               (all-the-icons-dired-mode t)))
  (define-key dired-mode-map (kbd "RET")     'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^")       (lambda () (interactive) (find-alternate-file "..")))
  (define-key dired-mode-map (kbd "C-.")     'dired-omit-mode)
  (define-key dired-mode-map (kbd "c")       'find-file)
  (define-key dired-mode-map (kbd "/")       'counsel-grep-or-swiper)
  (define-key dired-mode-map (kbd "?")       'evil-search-backward)
  (define-key dired-mode-map (kbd "C-c C-c") 'dired-toggle-read-only))

(use-package elpy
  :ensure t
  :mode "\\.py\\'"
  :config
  (elpy-enable))


(use-package web-mode
  :ensure t
  :mode "\\(?:\\(?:\\.\\(?:html\\|twig\\)\\)\\)\\'"
  :config
  (setq web-mode-attr-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-indent-style 2
        web-mode-markup-indent-offset 2
        web-mode-sql-indent-offset 2)

  (setq web-mode-ac-sources-alist
        '(("php" . (ac-source-php-extras
                    ac-source-yasnippet
                    ac-source-gtags
                    ac-source-abbrev
                    ac-source-dictionary
                    ac-source-words-in-same-mode-buffers))
          ("css" . (ac-source-css-property
                    ac-source-abbrev
                    ac-source-dictionary
                    ac-source-words-in-same-mode-buffers))))

  (add-hook 'web-mode-hook
            (lambda ()
              (setq web-mode-style-padding 2)
              (yas-minor-mode t)
              (emmet-mode)
              (flycheck-add-mode 'html-tidy 'web-mode)
              (flycheck-mode)))

  (add-hook 'web-mode-before-auto-complete-hooks
            '(lambda ()
               (let ((web-mode-cur-language (web-mode-language-at-pos)))
                 (if (string= web-mode-cur-language "php")
                     (yas-activate-extra-mode 'php-mode)
                   (yas-deactivate-extra-mode 'php-mode))
                 (if (string= web-mode-cur-language "css")
                     (setq emmet-use-css-transform t)
                   (setq emmet-use-css-transform nil))))))


(use-package helm
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  :demand t
  :bind (("M-x" . helm-M-x)
         ("C-c h o" . helm-occur)
         ("<f1> SPC" . helm-all-mark-rings) ; I modified the keybinding 
         ("M-y" . helm-show-kill-ring)
         ("C-c h x" . helm-register)    ; C-x r SPC and C-x r j
         ("C-c h g" . helm-google-suggest)
         ("C-c h M-:" . helm-eval-expression-with-eldoc)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)      ; *<major-mode> or /<dir> or !/<dir-not-desired> or @<regexp>
         :map helm-map
         ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
         ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
         ("C-z" . helm-select-action) ; list actions using C-z
         :map shell-mode-map
         ("C-c C-l" . helm-comint-input-ring) ; in shell mode
         :map minibuffer-local-map
         ("C-c C-l" . helm-minibuffer-history))
  :init
  (setq helm-command-prefix-key "C-c h")
  (setq recentf-save-file "~/.emacs.d/misc/recentf" ; customize yours
        recentf-max-saved-items 50)
  (require 'helm-eshell)        ; question
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history))) 
  (defun spacemacs//helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  :config
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)
  (setq helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match    t)
  (setq helm-locate-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-lisp-fuzzy-completion t)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  ;;(require 'helm-descbinds)
  ;;(helm-descbinds-mode)
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (require 'helm-config)
  (setq helm-split-window-in-side-p         t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)
  (add-hook 'helm-minibuffer-set-up-hook
            'spacemacs//helm-hide-minibuffer-maybe)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (helm-mode 1)
  )(use-package helm-org
  :ensure t
  :commands helm-org-agenda-files-headings)

(use-package company
  :init
  (setq company-backends '((company-files company-keywords company-capf company-dabbrev-code company-etags company-dabbrev)))

  :config
  (global-company-mode 1))




(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :config
  (add-hook 'emmet-mode-hook
            (lambda ()
              (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-l") 'emmet-next-edit-point)
              (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-h") 'emmet-prev-edit-point))))

(use-package flycheck
  :ensure t
  :commands flycheck-mode)

(use-package helm-projectile
  :ensure t
  :commands (helm-projectile helm-projectile-switch-project))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "pandoc --from markdown_github-hard_line_breaks --to html")
  (define-key markdown-mode-map (kbd "<C-return>") 'markdown-insert-list-item)
  (define-key markdown-mode-map (kbd "C-c '")      'fence-edit-code-at-point)
  (define-key markdown-mode-map (kbd "C-c 1")      'markdown-insert-header-atx-1)
  (define-key markdown-mode-map (kbd "C-c 2")      'markdown-insert-header-atx-2)
  (define-key markdown-mode-map (kbd "C-c 3")      'markdown-insert-header-atx-3)
  (define-key markdown-mode-map (kbd "C-c 4")      'markdown-insert-header-atx-4)
  (define-key markdown-mode-map (kbd "C-c 5")      'markdown-insert-header-atx-5)
  (define-key markdown-mode-map (kbd "C-c 6")      'markdown-insert-header-atx-6)

  (add-hook 'markdown-mode-hook (lambda ()
                                  (visual-line-mode t)
                                  (set-fill-column 80)
                                  (yas-minor-mode-on)
                                  (hugo-minor-mode t)
                                  (turn-on-auto-fill)
                                  ;; Don't wrap Liquid tags
                                  (setq auto-fill-inhibit-regexp (rx "{" (? "{") (1+ (or "%" "<" " ")) (1+ letter)))
                                  (flyspell-mode))))

(use-package yaml-mode
  :ensure t
  ;; .yaml or .yml
  :mode "\\(?:\\(?:\\.y\\(?:a?ml\\)\\)\\)\\'")

(use-package yasnippet
  :ensure t
  :defer t
  :config
  ;;(yas-reload-all)
  (setq tab-always-indent 'complete)
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))

(use-package yasnippet-snippets
  :ensure t
  :defer t)

(use-package projectile
  :ensure t
  :defer t
  :config
  (projectile-mode)
  (add-to-list 'projectile-globally-ignored-directories "*node_modules")
  (setq projectile-enable-caching t)
  (setq projectile-mode-line
        '(:eval
          (format " Proj[%s]"
                  (projectile-project-name)))))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (add-hook 'magit-mode-hook
            (lambda ()
              (define-key magit-mode-map (kbd ",o") 'delete-other-windows)))
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package vterm
  :ensure t
  :bind (("C-x C-t" . vterm))
)  
