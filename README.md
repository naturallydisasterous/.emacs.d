# My Emacs Config

![Minimum Emacs version supported: 26.3 ](https://img.shields.io/badge/Supports-Emacs_26.3+-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)
[![GitHub issues](https://img.shields.io/github/issues/Aaron-Mann/.emacs.d)](https://github.com/Aaron-Mann/.emacs.d/issues)
[![GitHub forks](https://img.shields.io/github/forks/Aaron-Mann/.emacs.d)](https://github.com/Aaron-Mann/.emacs.d/network)
[![GitHub stars](https://img.shields.io/github/stars/Aaron-Mann/.emacs.d)](https://github.com/Aaron-Mann/.emacs.d/stargazers)
[![GitHub license](https://img.shields.io/github/license/Aaron-Mann/.emacs.d)](https://github.com/Aaron-Mann/.emacs.d/blob/main/LICENSE)

A Simple, Fully Featured, and Evil Emacs Config. Inspired by what Doom Emacs got right, what Spacemacs got wrong, and with some new ideas and features that strive to make your life easier.

# Design Goals and Features

- **Configurable:** Speaking of end user configs, I provide excellent tools that make tweaking the environment as easy as changing a line! This includes both a `config.el` file for simple variable changes, and a `custom.el` file specifically tailored for building on top of the pre-existing ecosystem.
- ***FUTURE FEATURE: Selective Language Support:*** This system allows a file to be created for each language you wish to use. Don't want to touch Javascript? Remove it's script. Disgusted by whitespace? `rm -rf .emacs.d/languages/python.el`. Interested in an unsupported language? Create `cobol.el`.

# Prerequisites
- Git 2.23+
- Emacs 26.3 (although 27.2 is recommended)

# Install

Navigate to your home directory, and issue the following command:
`git clone --depth 1 https://github.com/Aaron-Mann/.emacs.d ~/.emacs.d`

Now go ahead and run emacs! You should see a white screen with ELPA information across the bottom upon your first startup. No need to worry, this is just emacs configuring during its first startup. After processing for ~20 seconds, all packages should be installed and ready to operate. Just one last thing before emacs is ready...
`M-x all-the-icons-install-fonts`

Enjoy your emacs experience!

# Roadmap

As following is the current list of features to be implemented:
- **TreeSelect:** Implement a reasonable, fast, and useful treeselect module.
- **Languages:** Implement *Selective Language Support*, with the support for the following key languages: `C/C++`, `Python`, `JavaScript`, `Rust`, `Go`, and `Org`.
- **Magit:** What else can I say? Get Magit up and running smoothly.
- **Documentation:** The ongoing fight to keep projects useable...

# Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square)](http://makeapullrequest.com) 
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple?style=flat-square)](https://github.com/bbatsov/emacs-lisp-style-guide)

All Functions, errors, and messages added by this config are seperated from vanilla emacs functions with the name "Toaster", ie: `toaster-log`. This naming convention should be used for any new functionality. 

ETH: `0x0F4A5595FC74a0279Dfe0aab8f5823B63400E15a`

BEAM: `3205ba4fa07fb2c332079a1d725f9d8f5a79d9b8042d560343a2e6981419253d966`
