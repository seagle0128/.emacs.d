Personal Emacs Configuration
============================

[![Build Status](https://travis-ci.org/seagle0128/.emacs.d.svg?branch=master)](https://travis-ci.org/seagle0128/.emacs.d)

This is an Emacs distribution that aims to enhance the default
Emacs experience. It alters a lot of the default settings,
bundles a plethora of additional packages and adds its own core
library to the mix. The final product offers an easy to use Emacs
configuration for Emacs newcomers and lots of additional power for
Emacs power users.

It's able to run on Windows, GNU Linux and macOS. It is compatible **ONLY with
GNU Emacs 24.4 and above**. In general you're advised to always run with the
latest Emacs - currently **25.1**.

# Quick Start

```sh
git clone https://github.com/seagle0128/.emacs.d.git ~/.emacs.d
```
Then start emacs. Enjoy!

# Customization
Copy custom-example.el to custom.el and set the variables. Exit Emacs and delete
all elc in ~/.emacs.d/lisp, then start Emacs.

```elisp
(setq my-full-name "user name")           ; user full name
(setq my-mail-address "user@email.com")   ; email address
(setq my-ac-method 'auto-complete)        ; company or auto-complete
(setq my-completion-method 'helm)         ; helm, ivy or ido
(setq my-package-archives 'emacs-china)   ; melpa, emacs-china or tsinghua
(setq my-benchmark-enabled t)             ; t or nil
(setq my-profiler-enabled t)              ; t or nil
```
