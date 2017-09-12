Centaur Emacs
============================

[![Build Status](https://travis-ci.org/seagle0128/.emacs.d.svg?branch=master)](https://travis-ci.org/seagle0128/.emacs.d)

![Centaur Emacs](images/logos/logo.png)

This is an Emacs distribution that aims to enhance the default
Emacs experience. It alters a lot of the default settings,
bundles a plethora of additional packages and adds its own core
library to the mix. The final product offers an easy to use Emacs
configuration for Emacs newcomers and lots of additional power for
Emacs power users.

It's able to run on Windows, GNU Linux and macOS. It is compatible **ONLY with
GNU Emacs 24.4 and above**. In general you're advised to always run with the
latest stable release - currently **25.2**.

# Features

- Pure GNU Emacs experience.
- Out of box.
- Clean and Fast.
- Quick fuzzy search (via `ivy`, `ag`, `rg`).
- Better Org support.
- Support multiple programming languages
  - Emacs-lisp
  - C/C++/C#
  - Ruby/Python/Perl/PHP/Shell/Powershell
  - Javascript/Typescript/JSON/YAML
  - HTML/CSS/XML
  - Golang/Siwft
  - Markdown
  - ...
- Auto completion.
- Syntax check.
- Fly spell check.
- Git/SVN integration.
- Projectile integration.
- Pomodor integration.
- Support Chinese fonts and calendar.
- Support Pinyin search.

# Prerequiste

## OS

- GNU Linux
- macOS
- Windows (Cygwin)

## GNU Emacs

Please refer to ![Installing Emacs](http://wikemacs.org/index.php/Installing_Emacs).

## Optional

Recommend to use ![oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh) or
![dotfiles](https://github.com/seagle0128/dotfiles).

# Quick Start

## Installation

```shell
    git clone https://github.com/seagle0128/.emacs.d.git ~/.emacs.d
```

or download the ![zip
package](https://github.com/seagle0128/.emacs.d/archive/master.zip) directly and
extract to `~.emacs.d`.

Then start emacs. Wait for a while to install packages at the first startup.
Enjoy!

## Update

Run `M-x update-config` in Emacs, or

``` shell
cd ~/.emacs.d && git pull
```

# Customization

Copy `custom-example.el` to `custom.el` and change the configurations, then restart.

```emacs-lisp
(setq my-logo nil)                        ; Logo file or nil (official logo)
(setq my-full-name "user name")           ; User full name
(setq my-mail-address "user@email.com")   ; Email address
(setq my-proxy "127.0.0.1:1080")          ; Network proxy
(setq my-package-archives 'emacs-china)   ; Package repo: melpa, emacs-china or tsinghua
(setq my-theme 'dark)                     ; Color theme: default, dark, light or daylight
(setq my-benchmark-enabled t)             ; Enable initialization benchmark: t or nil
(setq my-profiler-enabled t)              ; Enable startup profiler: t or nil

```

# Screenshots

![Main](images/screenshots/main.png)
![Programming](images/screenshots/programming.png)
![Org](images/screenshots/org.png)
![Search](images/screenshots/search.png)
![Magit](images/screenshots/magit.png)
