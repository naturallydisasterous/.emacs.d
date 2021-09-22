(require 'cl)
;; emacs will auto-add this if not included:
(require 'package)

;; evil settings before we load evil:
(setq evil-want-keybinding nil)
(setq evil-want-integration t) ;; optional, defaults to true



(setq user-packages '(
                      ;; UI
                      evil
                      evil-collection
                      web-mode
                      helm
                      company
                      magit
                      doom-modeline
		      doom-themes
                      vterm
                      lsp-mode
                      treemacs
                      treemacs-evil
                      ))

(defun install-user-packages () (
                                 let ((pkgs (remove-if #'package-installed-p user-packages )))
                                 (when pkgs
                                   (package-refresh-contents)
                                   (dolist (p user-packages)
                                     (package-install p)))))

;; add package archives:

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; init emacs packages
(package-initialize)

;; install packages
(install-user-packages)

;; doom-themes

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
(doom-themes-org-config)

;; evil: see L5

(evil-mode 1)
(evil-collection-init)



;; dired

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
(define-key dired-mode-map (kbd "C-c C-c") 'dired-toggle-read-only)

;; web-mode

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
                   (setq emmet-use-css-transform nil)))))

;; helm

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-h o") 'helm-occur)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

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

;; company

(setq company-backends '((company-files company-keywords company-capf company-dabbrev-code company-etags company-dabbrev)))

(global-company-mode 1)

;; magit

(setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (add-hook 'magit-mode-hook
            (lambda ()
              (define-key magit-mode-map (kbd ",o") 'delete-other-windows)))
(add-hook 'git-commit-mode-hook 'evil-insert-state)

(global-set-key (kbd "C-x m") 'magit-status)

;; doom-modeline

(doom-modeline-mode 1)

;; vtem

(global-set-key (kbd "C-<return>") 'vterm)

;; lsp-mode

(setq lsp-keymap-prefix "C-c l")

