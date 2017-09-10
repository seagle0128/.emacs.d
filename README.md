Centaur Emacs Configuration
============================

[![Build
Status](https://travis-ci.org/seagle0128/.emacs.d.svg?branch=master)](https://travis-ci.org/seagle0128/.emacs.d)

![Centaur Emacs](images/logos/logo.png)

This is an Emacs distribution that aims to enhance the default
Emacs experience. It alters a lot of the default settings,
bundles a plethora of additional packages and adds its own core
library to the mix. The final product offers an easy to use Emacs
configuration for Emacs newcomers and lots of additional power for
Emacs power users.

It's able to run on Windows, GNU Linux and macOS. It is compatible **ONLY with
GNU Emacs 24.4 and above**. In general you're advised to always run with the
latest stable release - currently **25.x**.

# Quick Start

```sh
    git clone https://github.com/seagle0128/.emacs.d.git ~/.emacs.d
```

Then start emacs. That's it. Enjoy!

*Note: You may need to wait for a several minutes to install packages at the first startup.*

# Customization
Copy custom-example.el to custom.el and set the variables. For example:

```elisp
(setq my-logo nil)                        ; logo path or nil (official logo)
(setq my-full-name "user name")           ; user full name
(setq my-mail-address "user@email.com")   ; email address
(setq my-proxy "127.0.0.1:1080")          ; network proxy
(setq my-package-archives 'emacs-china)   ; melpa, emacs-china or tsinghua
(setq my-theme 'dark)                     ; default, dark, light or daylight
(setq my-benchmark-enabled t)             ; t or nil
(setq my-profiler-enabled t)              ; t or nil
```

Restart Emacs. Enjoy!

# Screenshots

![Centaur Emacs](images/screenshots/main.png)
![Centaur Emacs](images/screenshots/programming.png)
![Centaur Emacs](images/screenshots/org.png)
![Centaur Emacs](images/screenshots/search.png)
