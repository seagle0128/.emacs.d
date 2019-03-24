# Centaur Emacs

[![Build Status](https://travis-ci.org/seagle0128/.emacs.d.svg?branch=master)](https://travis-ci.org/seagle0128/.emacs.d)
[![Release Tag](https://img.shields.io/github/tag/seagle0128/.emacs.d.svg?label=release)](https://github.com/seagle0128/.emacs.d/releases/latest)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

![Centaur Emacs](logo.png)

This is an Emacs distribution that aims to enhance the default
Emacs experience. It alters a lot of the default settings,
bundles a plethora of additional packages and adds its own core
library to the mix. The final product offers an easy to use Emacs
configuration for Emacs newcomers and lots of additional power for
Emacs power users.

It's able to run on Windows, GNU Linux and macOS. It is compatible **ONLY with
GNU Emacs 25.1 and above**. In general you're advised to always run with the
latest stable release - currently **26.1**.

## Features

- Out of box.
- Clean and Fast.
- Quick fuzzy search.
- Better Org/Markdown support.
- Support multiple programming languages
  - C/C++/Object-C/C#/Java
  - Python/Ruby/Perl/PHP/Shell/Powershell
  - Javascript/Typescript/JSON/YAML
  - HTML/CSS/XML
  - Golang/Swift/Rust
  - ...
- Auto completion.
- Fly syntax check.
- Fly spell check.
- Git integration.
- Project/Workspace integration.
- Pomodor integration.
- Better Chinese support:
  - Chinese calendar
  - Youdao dictionary
  - Pinyin search

## Prerequiste

### OS

- GNU Linux
- macOS
- Windows (Cygwin/msys)

### GNU Emacs

Please refer to [Installing Emacs](http://wikemacs.org/index.php/Installing_Emacs).

### Dotfiles

Recommend to use [Centaur Dotfiles](https://github.com/seagle0128/dotfiles).

## Quick Start

### Install

``` shell
mv ~/.emacs.d ~/.emacs.d.bak
git clone --depth 1 https://github.com/seagle0128/.emacs.d.git ~/.emacs.d
```

or download the [zip
package](https://github.com/seagle0128/.emacs.d/archive/master.zip) directly and
extract to `~/.emacs.d`.

Then start emacs. Wait for a while to install packages at the first startup.
Enjoy!

### Update

``` emacs-lisp
# Update Centaur Emacs (then restart), including configurations and packages
M-x centaur-update
M-x centaur-update-and-restart

# Update Emacs configurations only
M-x centaur-update-config

# Update ~/.dotfiles if it exists
M-x centaur-update-dotfiles

# Update packages only (then restart)
M-x centaur-update-packages
M-x centaur-update-packages-and-restart

# Update all including configurations, packages and dotfiles
M-x centuar-update-all
```

### Docker

``` shell
cd ~/.emacs.d/Dockerfile
docker build -t centaur/emacs .
docker run -it centaur/emacs bash
```

## Customization

### Customize-group

`M-x customize-group` and select `centaur`. Set and save the configurations,
then restart Emacs.

### Manual

Copy `custom-template.el` to `custom.el` and change the configurations, then
restart Emacs.

For Example:

``` emacs-lisp
(setq centaur-logo nil)                        ; Logo file or nil (official logo)
(setq centaur-full-name "user name")           ; User full name
(setq centaur-mail-address "user@email.com")   ; Email address
(setq centaur-proxy "127.0.0.1:1080")          ; Network proxy
(setq centaur-package-archives 'emacs-china)   ; Package repo: melpa, melpa-mirror, emacs-china, netease or tuna
(setq centaur-theme classic)                   ; Color theme: default, classic, dark, light or daylight
(setq centaur-dashboard nil)                   ; Use dashboard at startup or not: t or nil
(setq centaur-lsp nil)                         ; Set LSP client: lsp-mode, eglot or nil
(setq centaur-benchmark t)                     ; Enable initialization benchmark or not: t or nil
```

The default pacakge archives is `melpa`. You can change it in `custom.el`, or
switch manually via `M-x switch-package-archives` anytime.

For the personal configurations, you could put to `~/.emacs.d/custom-post.el`.

## Screenshots

![Dashboard](https://user-images.githubusercontent.com/140797/53698460-9c58af00-3e0f-11e9-9d32-8fd0c2c0ff77.png
"Dashboard (default theme)")

![markdown_magit](https://user-images.githubusercontent.com/140797/49694620-8ba9eb80-fbc8-11e8-9800-f2486ab41991.png
"Markdown and Magit")

![org](https://user-images.githubusercontent.com/140797/49694696-a761c180-fbc9-11e8-9edc-0199f4ea7ca0.png
"Org (classic theme)")

![main](https://user-images.githubusercontent.com/140797/49694704-cfe9bb80-fbc9-11e8-9506-df9fee48e304.png
"Main (light theme)")

## FAQ

1. Why is the modline messy?

    Powerline fonts or all-the-icons are missing on your system. Please install
    [powerline-fonts](https://github.com/powerline/fonts) for `telephone-line` or
    run `M-x all-the-icons-install-fonts` for `doom-modeline`.

1. How to search Chinese via pinyin?

    In Emacs, `C-s !`. If you just want to search `!`, use `C-s \!`.

1. How to use the Centaur Dashboard?

    If `centaur-dashboard` is non-nil the Centaur Dashboard will be opened at
    startup automatically. After startup, you could use `F2` to reopen it
    anytime. In the dashboard, please press `?` or `h` to get the help. You can
    easily jump to the sections or buttons with keybindings, e.g. Homepage(`H`),
    Session(`R`, `L`), Settings(`S`), Update(`U`), Recent Files (`r`),
    Bookmarks(`m`) and Projects(`p`).

1. Does Centaur Emacs support Language Server Protocol (LSP)?

    The LSP feature is supported and enabled by default in Centuar Emacs.
    `lsp-mode` is the default client, and `eglot` is another choice. Use `(setq
    centaur-lsp nil)` to disable LSP if you don't like it.

    Before use it you should install language servers. Refer to [lsp-mode:
    supported-languages](https://github.com/emacs-lsp/lsp-mode#supported-languages)
    or [eglot: Connecting to a
    server](https://github.com/joaotavora/eglot#connecting-to-a-server) for
    details.

1. How to enable `plantuml` in `org-mode`?

    Put `(setq org-plantuml-jar-path "<path of plantumx.x.x.jar>")` in `custom.el`.

1. Why the Emacs environment variables and `exec-path` are different between GUI
   and terminal?

    Please refer to #33. You should instead set environment variables in startup
    files like .profile, .bash_profile or .zshenv, then `Centaur Emacs` is able
    to recoginze and import the environment variables.
