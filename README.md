# My Emacs Config

![Minimum Emacs version supported: 26.3 ](https://img.shields.io/badge/Supports-Emacs_26.3+-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)
[![GitHub issues](https://img.shields.io/github/issues/Aaron-Mann/.emacs.d)](https://github.com/Aaron-Mann/.emacs.d/issues)
[![GitHub forks](https://img.shields.io/github/forks/Aaron-Mann/.emacs.d)](https://github.com/Aaron-Mann/.emacs.d/network)
[![GitHub stars](https://img.shields.io/github/stars/Aaron-Mann/.emacs.d)](https://github.com/Aaron-Mann/.emacs.d/stargazers)
[![GitHub license](https://img.shields.io/github/license/Aaron-Mann/.emacs.d)](https://github.com/Aaron-Mann/.emacs.d/blob/main/LICENSE)

A Simple, Fully Featured, and EVIL (Extensible VI Layer) Emacs Config. Inspired by what Doom Emacs got right, what Spacemacs got wrong, and with some new ideas and features that strive to make your life easier.

```
(require 'cl)
   (require 'package)

   ;; set core packages. Note, this package list should not be edited. These are the core toaster dependencies, and removing these could jepardize modules. Rather, use the package install system available in the 'custom.el' file
   ;; Another hint worth noting: No language related packages are installed here. Rather, they are individually installed in their language modules

   (setq toaster-core-packages '(
				  ;; UI:
				  doom-themes
				  doom-modeline
				  dashboard
				  all-the-icons

				  ;; Modes:
				  evil
				  org

				  ;; Tools
				  projectile
				  magit
				  ))

   (defun toaster-install-core-packages ()
     (let ((pkgs (remove-if #'package-installed-p toaster-core-packages )))
	(when pkgs
	  (package-refresh-contents)
	  (dolist (p toaster-core-packages)
	    (package-install p)))))


   ;; prepare emacs package archives:
   (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)

   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

   (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

   (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

   ;; initialize emacs packages:
   (package-initialize)

   ;; Install toaster packages

   (toaster-install-core-packages)
```

# Prerequisites

#### Git 2.23+
Ubuntu: `sudo apt install git`

Arch: `sudo pacman -S git`

#### Emacs 26.3 (although 27.2 is recommended)
Ubuntu: `sudo apt install emacs`

Arch: `sudo pacman -S emacs`

# Install

Navigate to your home directory, and issue the following command:
`git clone --depth 1 https://github.com/Aaron-Mann/.emacs.d ~/.emacs.d`

Now go ahead and run emacs! You should see a white screen with ELPA information across the bottom upon your first startup. No need to worry, this is just emacs configuring during its first startup. After processing for ~20 seconds, all packages should be installed and ready to operate. Just one last thing before emacs is ready...

`M-x all-the-icons-install-fonts` (M-x: ALT+x)

Enjoy your emacs experience!

# Roadmap

As following is the current list of features to be implemented:
- **TreeSelect:** Implement a reasonable, fast, and useful treeselect module.

# Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square)](http://makeapullrequest.com) 
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple?style=flat-square)](https://github.com/bbatsov/emacs-lisp-style-guide)

All Functions, errors, and messages added by this config are seperated from vanilla emacs functions with the name "Toaster", ie: `toaster-log`. This naming convention should be used for any new functionality. 

ETH: `0x0F4A5595FC74a0279Dfe0aab8f5823B63400E15a`

BEAM: `3205ba4fa07fb2c332079a1d725f9d8f5a79d9b8042d560343a2e6981419253d966`
