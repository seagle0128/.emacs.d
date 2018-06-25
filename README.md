# Centaur Emacs

[![Build
Status](https://travis-ci.org/seagle0128/.emacs.d.svg?branch=master)](https://travis-ci.org/seagle0128/.emacs.d)
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
- Quick fuzzy search (via `ivy`, `rg`, `ag` and `pt` etc.).
- Better Org support.
- Support multiple programming languages
  - C/C++/C#/Java
  - Ruby/Python/Perl/PHP/Shell/Powershell
  - Javascript/Typescript/JSON/YAML
  - HTML/CSS/XML
  - Golang/Swift
  - Markdown
  - ...
- Auto completion.
- Fly syntax check.
- Fly spell check.
- Git/SVN integration.
- Projectile integration.
- Workspace integration.
- Pomodor integration.
- Youdao dictionary integration.
- Support Chinese fonts and calendar.
- Support Pinyin search.

## Prerequiste

### OS

- GNU Linux
- macOS
- Windows (Cygwin)

### GNU Emacs

Emacs 24.4+. Please refer to [Installing Emacs](http://wikemacs.org/index.php/Installing_Emacs).

### Dotfiles

Recommend to use [Centaur Dotfiles](https://github.com/seagle0128/dotfiles).

## Quick Start

### Install

Backup `.emacs.d` if need,

``` shell
mv ~/.emacs.d ~/.emacs.d.bak
```

then

``` shell
git clone https://github.com/seagle0128/.emacs.d.git ~/.emacs.d
```

or download the [zip
package](https://github.com/seagle0128/.emacs.d/archive/master.zip) directly and
extract to `~./emacs.d`.

Then start emacs. Wait for a while to install packages at the first startup.
Enjoy!

### Update

Run `M-x update-config` in Emacs, or

``` shell
cd ~/.emacs.d && git pull
```

### Upgrade packages

``` emacs-lisp
M-x upgrade-packages
```

## Customization

### Customize-group

`M-x customize-group` and select `centaur`. You can add additional
customizations in `custom.el`.

### Manual

Copy `custom-example.el` to `custom.el` and change the configurations, then
restart Emacs.

For Example:

``` emacs-lisp
(setq centaur-logo nil)                        ; Logo file or nil (official logo)
(setq centaur-full-name "user name")           ; User full name
(setq centaur-mail-address "user@email.com")   ; Email address
(setq centaur-proxy "127.0.0.1:1080")          ; Network proxy
(setq centaur-package-archives 'emacs-china)   ; Package repo: melpa, emacs-china or tuna
(setq centaur-theme 'doom)                     ; Color theme: default, doom, dark, light or daylight
(setq centuar-company-enable-yas t)            ; Enable/disable yasnippet for company: t or nil
(setq centaur-emoji-enabled t)                 ; Enable/disable emoji: t or nil
(setq centaur-benchmark-enabled t)             ; Enable/disable initialization benchmark: t or nil

;; You may add addtional configurations here
;; (custom-set-variables )
```

The default pacakge archives is `melpa`. You can change it in `custom.el`, or
switch manually via `M-x switch-package-archives` anytime.

## Screenshots

### Main (default theme)

![Main](https://user-images.githubusercontent.com/140797/30391180-20bd0ba8-987e-11e7-9cb4-2aa66a6fd69d.png)

### Programming (dark theme)

![Programming](https://user-images.githubusercontent.com/140797/31727834-433c1164-b3f0-11e7-9f73-0977d9f600f1.png)

### Org mode (dark theme)

![Org](https://user-images.githubusercontent.com/140797/30391183-20c37e8e-987e-11e7-9579-c4df71549a76.png)

### Search (light theme)

![Search](https://user-images.githubusercontent.com/140797/30391184-20f47fac-987e-11e7-8be4-9f4e409d65cc.png)

### Magit (daylight theme)

![Magit](https://user-images.githubusercontent.com/140797/30391181-20bd848e-987e-11e7-9cda-3dac2865922e.png)

### Doom theme and modeline

![Doom](https://user-images.githubusercontent.com/140797/41302817-13cb7622-6e9e-11e8-894b-07aff95f91bc.png)

## FAQ

1. Why is the modline messy?

   Powerline fonts are missing on your system. Please install
   [powerline-fonts](https://github.com/powerline/fonts).

1. How to search Chinese via pinyin?

   In Emacs, `C-s !`

1. How to enable `plantuml` in `org-mode`?

   Put `(setq org-plantuml-jar-path "<path of plantumx.x.x.jar>")` in `custom.el`.
