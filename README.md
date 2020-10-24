# Centaur Emacs

[![Build Status](https://github.com/seagle0128/.emacs.d/workflows/CI/badge.svg?branch=master)](https://github.com/seagle0128/.emacs.d/actions)
[![Release Tag](https://img.shields.io/github/tag/seagle0128/.emacs.d.svg?label=release)](https://github.com/seagle0128/.emacs.d/releases/latest)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](LICENSE)

![Centaur Emacs](logo.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Centaur Emacs](#centaur-emacs)
    - [Features](#features)
    - [Prerequisite](#prerequisite)
        - [OS](#os)
        - [GNU Emacs](#gnu-emacs)
        - [Dotfiles](#dotfiles)
    - [Quick Start](#quick-start)
        - [Install](#install)
        - [Update](#update)
        - [Docker](#docker)
    - [Customization](#customization)
        - [Customize Group](#customize-group)
        - [Manual](#manual)
    - [Hydra](#hydra)
    - [Screenshots](#screenshots)
    - [FAQ](#faq)
    - [Donate](#donate)

<!-- markdown-toc end -->

This is an Emacs distribution that aims to enhance the default
Emacs experience. It alters a lot of the default settings,
bundles a plethora of additional packages and adds its own core
library to the mix. The final product offers an easy to use Emacs
configuration for Emacs newcomers and lots of additional power for
Emacs power users.

It's able to run on Windows, GNU Linux and macOS. It is compatible **ONLY with
GNU Emacs 25.1 and above**. In general you're advised to always run with the
latest stable release - currently **27.1**.

## Features

- Out of box.
- Clean and Fast.
- Quick fuzzy search.
- Better Org/Markdown support.
- Support multiple programming languages
    - C/C++/Object-C/C#/Java
    - Python/Ruby/Perl/PHP/Shell/Powershell/Bat
    - Javascript/Typescript/JSON/YAML
    - HTML/CSS/XML
    - Golang/Swift/Rust/Dart/Elixir
    - ...
- Auto completion.
- Fly syntax check.
- Fly spell check.
- Git integration.
- Project/Workspace integration.
- Pomodor integration.
- Support docker.
- Better Chinese support:
    - Chinese calendar
    - Youdao dictionary
    - Pinyin search

## Prerequisite

### OS

- GNU Linux
- macOS
- Windows (Cygwin/MSYS)

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

Then start Emacs. Wait for a while to install packages at the first startup.
Enjoy! :smile:

**NOTE**: Start Emacs with the minimal configuration for fast startup and
troubleshooting.

``` shell
emacs -Q --l ~/.emacs.d/init-mini.el
```

### Update

``` emacs-lisp
# Update Centaur Emacs, including configurations and packages
M-x centaur-update

# Update Emacs configurations only
M-x centaur-update-config

# Update ~/.dotfiles if it exists
M-x centaur-update-dotfiles

# Update packages only
M-x centaur-update-packages

# Update all including configurations, packages and dotfiles
M-x centaur-update-all
```

### Docker

``` shell
cd ~/.emacs.d/Dockerfile
docker build -t centaur/emacs .
docker run -it centaur/emacs bash
```

## Customization

### Customize Group

`M-x customize-group` and select `centaur`. Set and save the configurations, then restart Emacs.

### Manual

Add or change the configurations in `custom.el`, then restart Emacs.

For Example:

``` emacs-lisp
(setq centaur-logo nil)                        ; Logo file or nil (official logo)
(setq centaur-full-name "user name")           ; User full name
(setq centaur-mail-address "user@email.com")   ; Email address
(setq centaur-proxy "127.0.0.1:1080")          ; Network proxy
(setq centaur-server t)                        ; Enable `server-mode' or not: t or nil
(setq centaur-icon t)                          ; Display icons or not: t or nil
(setq centaur-package-archives 'emacs-china)   ; Package repo: melpa, emacs-china, netease or tuna
(setq centaur-theme 'auto)                     ; Color theme: auto, random, default, classic, colorful, dark, light, day or night
(setq centaur-dashboard t)                     ; Use dashboard at startup or not: t or nil
(setq centaur-restore-frame-geometry nil)      ; Restore the frame's geometry at startup: t or nil
(setq centaur-lsp 'lsp-mode)                   ; Set LSP client: lsp-mode, eglot or nil
(setq centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode python-mode)) ; Ignore format on save for some languages
(setq centaur-chinese-calendar nil)            ; Use Chinese calendar or not: t or nil
(setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications. Nil to use font supports ligatures.
(setq centaur-prettify-org-symbols-alist nil)  ; Alist of symbol prettifications for `org-mode'
(setq centaur-benchmark-init nil)              ; Enable initialization benchmark or not: t or nil
```

The default package archives is `melpa`. You can change it in `custom.el`, or
set manually via `M-x set-package-archives` anytime.

For the personal configurations, you could put to `~/.emacs.d/custom-post.org`
 or`~/.emacs.d/custom-post.el`.

## Hydra

| Name                     | Scope                 | Keybinding        | Description                          |
|--------------------------|-----------------------|-------------------|--------------------------------------|
| `toggles-hydra`          | global                | `<f6>`            | Global option toggles                |
| `window-hydra`           | global                | `C-c w`/`C-x o w` | Window management                    |
| `doom-modeline-hydra`    | doom-modeline-mode    | `C-<f6>`          | Mode-line options and actions        |
| `hydra-ivy`              | minibuffer, ivy-mode  | `C-o`             | Additional key bindings for Ivy      |
| `ivy-hydra-read-action`  | minibuffer, ivy-mode  | `M-o`             | Actions for`ivy-dispatching-done`    |
| `hydra-dired-qick-sort`  | dired                 | `S`               | Options for `dired-quick-sort`       |
| `org-hydra`              | org-mode              | `<`               | Org template                         |
| `dashboard-hydra`        | dashboard-mode        | `h`/`?`           | Actions for the dashboard            |
| `dumb-jump-hydra`        | global                | `C-M-j`           | Jump to definition                   |
| `youdao-dictionay-hydra` | youdao-dictionay-mode | `h`/`?`           | Actions for `youdao-dictionary`      |
| `ztreediff-hydra`        | zreediff-mode         | `C-<f5>`          | Actions for text mode directory tree |
| `git-messenger-hydra`    | global                | `C-x v p`         | Actions for `git-messenger`          |
| `smerge-mode-hydra`      | smerge-mode           | `C-c m`           | Actions for `smerge-mode`            |
| `rect-hydra`             | text-mode, prog-mode  | `C-<return>`      | Actions for Rectangle                |
| `rect-hydra`             | org-mode              | `S-<return>`      | Actions for Rectangle                |
| `lsp-ui-hydra`           | lsp-ui-mode           | `M-<f6>`          | Actions for `lsp-ui`                 |
| `dap-hydra`              | dap-mode              | `M-<f5>`          | Actions for `dap-debug`              |
| `elfeed-hydra`           | elfeed                | `?`               | Actions for RSS reader `elfeed`      |
| `xwidget-hydra`          | xwidget-webkit-mode   | `?`               | Actions for embedded webkit browser  |

## Screenshots

![Dashboard](https://user-images.githubusercontent.com/140797/97093362-c793b000-167d-11eb-924a-1892c6e95b8a.png
"Dashboard (default theme)")

![color_icons](https://user-images.githubusercontent.com/140797/97093358-b9de2a80-167d-11eb-9e5a-8591b1248c37.png
"Main (color icons)")

![markdown_magit](https://user-images.githubusercontent.com/140797/56489193-7f894f80-6513-11e9-9c95-ea1ef41efb1f.png
"Markdown and Magit")

![org_classic](https://user-images.githubusercontent.com/140797/56489410-2837af00-6514-11e9-9e04-a1663ac479e8.png
"Org (classic theme)")

![main_light](https://user-images.githubusercontent.com/140797/56489516-85cbfb80-6514-11e9-9dd4-8602bf8a4ec7.png
"Main (light theme)")

## FAQ

1. How to display icons correctly in `Centaur Emacs`?

    [all-the-icons](https://github.com/domtronn/all-the-icons.el) are necessary.
    Run `M-x all-the-icons-install-fonts` to install the resource fonts. On
    Windows, the fonts should be installed manually. `all-the-icons` only
    support GUI. If you don't like color icons, `(setq all-the-icons-color-icons
    nil)` to disable it. Please refer to
    [all-the-icons.el](https://github.com/domtronn/all-the-icons.el) for
    details.

    If the icons are not displayed correctly although `all-the-icons` fonts are
    installed correctly, please install the
    [non-free](http://users.teilar.gr/~g1951d/License.pdf) font
    [Symbola](https://dn-works.com/wp-content/uploads/2020/UFAS-Fonts/Symbola.zip).
    This issue usually occurs on Windows. Refer to [#121](https://github.com/seagle0128/.emacs.d/issues/121)
    for more details.

    If you are using [cnfonts](https://github.com/tumashu/cnfonts), it will
    conflict with `all-the-icons`. The workaround is
    [here](https://github.com/seagle0128/doom-modeline/issues/278#issuecomment-569510336).

    For better experience, I don't recommend to use GUI with `emacsclient` in
    `daemon` mode. See [#154](https://github.com/seagle0128/.emacs.d/issues/154).

1. The packages cannot be installed, what should I do?

   Generally it's due to connection issue. Please refer to
   [#98](https://github.com/seagle0128/.emacs.d/issues/98).
   - `M-x package-refresh-contents` and try again.
   - `(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")`.
   - Use other mirror of ELPA.
   - Change another network to retry.

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

    The LSP feature is supported and enabled by default in Centaur Emacs.
    `lsp-mode` is the default client, and `eglot` is another choice. Use `(setq
    centaur-lsp nil)` to disable LSP if you don't like it.

    To use LSP you should install the language servers. Refer to [lsp-mode:
    supported-languages](https://github.com/emacs-lsp/lsp-mode#supported-languages)
    or [eglot: Connecting to a
    server](https://github.com/joaotavora/eglot#connecting-to-a-server) for
    the details.

1. How to enable `plantuml` in `org-mode`?

    Put `(setq org-plantuml-jar-path "<path of plantumx.x.x.jar>")` in `custom.el`.

1. Why the Emacs environment variables and `exec-path` are different between GUI
   and terminal?

    Please refer to #33. You should instead set environment variables in startup
    files like .profile, .bash_profile or .zshenv, then `Centaur Emacs` is able
    to recognize and import the environment variables.

1. How to use [zoom-window](https://github.com/syohex/emacs-zoom-window) in
   `Centaur Emacs`?

   See [#169](https://github.com/seagle0128/.emacs.d/issues/169#issuecomment-590035527).

## Donate

If you think the it's helpful for you, please consider paying a cup of coffee
for me. Thank you! :smile:

<img
src="https://user-images.githubusercontent.com/140797/65818854-44204900-e248-11e9-9cc5-3e6339587cd8.png"
alt="Alipay" width="120"/>
&nbsp;&nbsp;&nbsp;&nbsp;
<img
src="https://user-images.githubusercontent.com/140797/65818844-366ac380-e248-11e9-931c-4bd872d0566b.png"
alt="Wechat Pay" width="120"/>

<a href="https://paypal.me/seagle0128" target="_blank">
<img
src="https://www.paypalobjects.com/digitalassets/c/website/marketing/apac/C2/logos-buttons/optimize/44_Grey_PayPal_Pill_Button.png"
alt="PayPal" width="120" />
</a>
&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://www.buymeacoffee.com/s9giES1" target="_blank">
<img src="https://cdn.buymeacoffee.com/buttons/default-orange.png" alt="Buy Me A Coffee"
width="160"/>
</a>
