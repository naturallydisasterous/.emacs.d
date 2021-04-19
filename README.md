# Toaster Emacs

![Minimum Emacs version supported: 26.3 ](https://img.shields.io/badge/Supports-Emacs_26.3+-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)
[![GitHub issues](https://img.shields.io/github/issues/Aaron-Mann/.emacs.d)](https://github.com/Aaron-Mann/.emacs.d/issues)
[![GitHub forks](https://img.shields.io/github/forks/Aaron-Mann/.emacs.d)](https://github.com/Aaron-Mann/.emacs.d/network)
[![GitHub stars](https://img.shields.io/github/stars/Aaron-Mann/.emacs.d)](https://github.com/Aaron-Mann/.emacs.d/stargazers)
[![GitHub license](https://img.shields.io/github/license/Aaron-Mann/.emacs.d)](https://github.com/Aaron-Mann/.emacs.d/blob/main/LICENSE)

A Simple, Fully Featured, and Evil Emacs Config.

### Table of Contents

- [Features](#features)
- [Prerequisites](#prerequisites)
- [Install](#install)
- [Roadmap](#roadmap)
- [Getting help](#getting-help)
- [Contribute](#contribute)

# Features

Design goals and functionalities that set Toaster Emacs apart from other emacs distributions:
- **Fast Load Times:** An excellent package manager, garbage collection tweaks, and other performance enhancements make Toaster load faster and run smoother than traditional emacs. This makes emacs comparable to editors like Vim for quick edits of config files, and superb at balancing hundreds of buffers in a large project.
- **Package Management:** Toaster Emacs makes package management a breeze! A carefully built set of packaging tools make load times incredibly quick, and allow the end user to easily leverage these performance boosts in their own configs.
- **Configurable:** Speaking of end user configs, Toaster emacs provides excellent tools that make tweaking the environment as easy as changing a line! Toaster provides both a `config.el` file for simple variable changes, and a `custom.el` file specifically tailored for building on top of the toaster ecosystem.
- ***FUTURE FEATURE: Selective Language Support:*** This system allows a file to be created for each language you wish to use. Don't want to touch Javascript? Remove it's script. Disgusted by whitespace? `rm -rf .emacs.d/languages/python.el`. Interested in an unsupported language? Create `cobol.el`.

# Prerequisites
- Git 2.23+
- Emacs 26.3 (although 27.2 is recommended)

# Install

Navigate to your home directory, and issue the following command:
`git clone --depth 1 https://github.com/Aaron-Mann/toaster-emacs ~/.emacs.d`

Now go ahead and run emacs! You should see a white screen with ELPA information across the bottom upon your first startup. No need to worry, this is just Toaster configuring during its first startup. After processing for ~20 seconds, all packages should be installed and ready to operate. Just one last thing before Toaster is ready...
`M-x all-the-icons-install-fonts`

Enjoy your Toaster experience!
