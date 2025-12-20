# 🧙‍♂️ Centaur Emacs

<div align="center">

[![CI](https://github.com/seagle0128/.emacs.d/actions/workflows/ci.yml/badge.svg)](https://github.com/seagle0128/.emacs.d/actions/workflows/ci.yml)
[![Release Tag](https://img.shields.io/github/tag/seagle0128/.emacs.d.svg?label=Release)](https://github.com/seagle0128/.emacs.d/releases)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![GNU Emacs](https://img.shields.io/badge/Emacs-28.1%2B-7F5AB6?logo=gnu&logoColor=white)](https://www.gnu.org/software/emacs/)
[![Platform](https://img.shields.io/badge/Platform-Windows%20|%20Linux%20|%20macOS-informational)](#prerequisite)

</div>

<div align="center">

![Centaur Emacs](logo.png)

</div>

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

## Table of Contents

  - [✨ Features](#-features)
  - [🧰 Prerequisites](#-prerequisites)
    - [Operating Systems](#operating-systems)
    - [GNU Emacs](#gnu-emacs)
    - [Dotfiles (Optional)](#dotfiles-optional)
  - [🚀 Quick Start](#-quick-start)
    - [Installation](#installation)
      - [Standard Installation](#standard-installation)
      - [First Startup](#first-startup)
    - [Updating](#updating)
      - [Update Commands](#update-commands)
    - [Docker Support](#docker-support)
  - [⚙️ Customization](#-customization)
    - [Using Customize Interface](#using-customize-interface)
    - [Manual Configuration](#manual-configuration)
      - [Common Configuration Options](#common-configuration-options)
  - [🎯 Hydra Keybindings](#-hydra-keybindings)
  - [📸 Screenshots](#-screenshots)
  - [❓ Frequently Asked Questions (FAQ)](#-frequently-asked-questions-faq)
  - [💖 Support the Project](#-support-the-project)

<!-- markdown-toc end -->

This is an Emacs distribution that aims to enhance the default Emacs experience.
It alters a lot of the default settings, bundles a plethora of additional
packages and adds its own core library to the mix. The final product offers
an easy to use Emacs configuration for Emacs newcomers and lots of additional
power for Emacs power users.

It is able to run on Windows, GNU Linux and macOS. It is compatible **ONLY with
GNU Emacs 28.1 and above**. In general you're advised to always run with the
latest stable release - currently **30.1**.

## ✨ Features

- **Out of the box**: Ready to use immediately after installation
- **Clean and Fast**: Optimized for performance and clean interface
- **Quick fuzzy search**: Efficient file and text searching capabilities
- **Better Org/Markdown support**: Enhanced markup language editing
- **Multi-language Programming Support**:
  - **System Languages**: C/C++/Object-C/C#/Java
  - **Scripting Languages**: Python/Ruby/Perl/PHP/Shell/Powershell/Bat
  - **Web Technologies**: JavaScript/Typescript/JSON/YAML
  - **Markup Languages**: HTML/CSS/XML
  - **Modern Languages**: Golang/Swift/Rust/Dart/Elixir
  - *And many more...*
- **Auto completion**: Intelligent code completion
- **Fly syntax check**: Real-time syntax error detection
- **Fly spell check**: Live spelling correction
- **Git integration**: Seamless version control workflow
- **Project/Workspace integration**: Advanced project management
- **Pomodoro integration**: Built-in productivity timer
- **MPD integration**: Music player daemon support
- **Docker support**: Containerization tools integration
- **Enhanced Chinese support**:
  - Chinese calendar integration
  - Youdao dictionary lookup
  - Google translation service
  - Pinyin search functionality

## 🧰 Prerequisites

### Operating Systems

- **GNU Linux**: Fully supported
- **macOS**: Fully supported
- **Windows**: Supported via Cygwin/MSYS

### GNU Emacs

Compatible with **GNU Emacs 28.1 and above**. We recommend using the latest stable release (currently **Emacs 30.1**).

For installation instructions, please refer to [Installing Emacs](http://wikemacs.org/index.php/Installing_Emacs).

### Dotfiles (Optional)

We recommend using [Centaur Dotfiles](https://github.com/seagle0128/dotfiles) for a complete system configuration.

## 🚀 Quick Start

### Installation

#### Standard Installation

To install Centaur Emacs, backup your existing configuration and clone the repository:

```bash
# Backup your existing configuration
mv ~/.emacs.d ~/.emacs.d.bak

# Clone the repository
git clone --depth 1 https://github.com/seagle0128/.emacs.d.git ~/.emacs.d
```

Alternatively, you can download the [ZIP package](https://github.com/seagle0128/.emacs.d/archive/master.zip) directly and extract it to `~/.emacs.d`.

**:information_source: Notes**: XDG Configuration Directory (Linux)

If you're using Linux and prefer an XDG-compatible location, use:

```bash
# Ensure ~/.emacs.d, ~/.emacs and ~/.emacs.el don't exist
git clone --depth 1 https://github.com/seagle0128/.emacs.d.git $XDG_CONFIG_HOME/emacs
```

Or extract the ZIP package to the `$XDG_CONFIG_HOME/emacs` directory.

#### First Startup

After installation, start Emacs and wait for packages to install during the first startup. This may take some time depending on your network speed.

**:warning: Notes**:
- First startup may take a while as packages are downloaded and installed
- If installation stalls, check your network connection or consider using a proxy
- For troubleshooting, start with minimal configuration: `emacs -Q -l ~/.emacs.d/init-mini.el`

**:rocket: Enjoy!** :smile:

### Updating

Keep your Centaur Emacs installation up-to-date with these commands:

#### Update Commands

```elisp
;; Update everything: configurations and packages
M-x centaur-update

;; Update only Emacs configurations
M-x centaur-update-config

;; Update ~/.dotfiles (if you have them installed)
M-x centaur-update-dotfiles

;; Update only packages
M-x centaur-update-packages

;; Update everything: configurations, packages, and dotfiles
M-x centaur-update-all
```

### Docker Support

Run Centaur Emacs in a containerized environment:

```bash
# Navigate to the Dockerfile directory
cd ~/.emacs.d

# Build the Docker image
docker build -t centaur/emacs -f Dockerfile.xx .

# Run the container interactively
docker run -it centaur/emacs bash
```

## ⚙️ Customization

### Using Customize Interface

The easiest way to customize Centaur Emacs is through the built-in customization interface:

1. Execute `M-x customize-group`
2. Select the `centaur` group
3. Modify the settings as needed
4. Click **Save** to store the configurations
5. Restart Emacs to apply changes

### Manual Configuration

For advanced users, you can directly edit configuration settings in your `custom.el` file:

#### Common Configuration Options

```elisp
;; Disable startup logo
(setq centaur-logo nil)

;; Set user information
(setq centaur-full-name "Your Name")           ; Your full name
(setq centaur-mail-address "your@email.com")   ; Your email address

;; Proxy settings
(setq centaur-proxy "127.0.0.1:1087")          ; HTTP/HTTPS proxy
(setq centaur-socks-proxy "127.0.0.1:1086")    ; SOCKS proxy

;; System settings
(setq centaur-server t)                        ; Enable server-mode
(setq centaur-use-exec-path-from-shell t)      ; Use shell environment (set to nil for emacs-plus)

;; Visual settings
(setq centaur-icon t)                          ; Display icons (t to enable, nil to disable)
(setq centaur-package-archives 'melpa)         ; Package repository (melpa, bfsu, iscas, netease, sjtu, tencent, tuna, ustc)
(setq centaur-theme 'auto)                     ; Theme: auto, random, system, default, pro, dark, light, warm, cold, day, night

;; UI settings
(setq centaur-completion-style 'minibuffer)    ; Completion style: minibuffer or childframe
(setq centaur-frame-maximized-on-startup t)    ; Maximize frame on startup
(setq centaur-dashboard nil)                   ; Show dashboard on startup (t to show, nil to hide)

;; Development settings
(setq centaur-lsp 'lsp-mode)                   ; LSP client: lsp-mode, eglot, or nil
(setq centaur-lsp-format-on-save t)            ; Auto-format on save
(setq centaur-lsp-format-on-save-ignore-modes  ; Modes to skip formatting on save
     '(c-mode c++-mode python-mode markdown-mode))

;; Feature toggles
(setq centaur-tree-sitter nil)                 ; Enable tree-sitter (requires Emacs 29+)
(setq centaur-chinese-calendar nil)            ; Enable Chinese calendar support
(setq centaur-player t)                        ; Enable media player controls
(setq centaur-prettify-symbols-alist nil)      ; Symbol prettification (nil to use font ligatures)
```

**:information_source: Notes**:
- The default package archive is `melpa`. You can change this in `custom.el` or via `M-x set-package-archives`
- Personal configurations can be placed in `~/.emacs.d/custom-post.org` or `~/.emacs.d/custom-post.el`

## 🎯 Hydra Keybindings

Centaur Emacs comes with several Hydra interfaces for efficient workflows. Here are the available hydras and their keybindings:

| Hydra Name                  | Scope                | Keybinding(s)     | Description                          |
|:---------------------------|:--------------------|:------------------|:-------------------------------------|
| `dape-hydra`               | Global               | `M-<f5>`          | Debug adapter protocol actions       |
| `dashboard-hydra`          | Dashboard Mode       | `h` / `?`         | Dashboard navigation and actions     |
| `doom-modeline-hydra`      | Modeline Mode        | `C-<f6>`          | Modeline customization options       |
| `elfeed-hydra`             | Elfeed Search Mode   | `?`               | RSS feed reader commands             |
| `git-messenger-hydra`      | Global               | `C-x v p`         | Git commit message inspection        |
| `hideshow-hydra`           | Programming Modes    | `C-~`             | Code folding/unfolding actions       |
| `hydra-dired-quick-sort`   | Dired Mode           | `S`               | File sorting options                 |
| `lsp-ui-hydra`             | LSP UI Mode          | `M-<f6>`          | LSP UI interface commands            |
| `org-hydra`                | Org Mode             | `<`               | Org template insertion               |
| `rect-hydra`               | Org Mode             | `S-<return>`      | Rectangle selection operations       |
| `rect-hydra`               | Text/Programming     | `C-<return>`      | Rectangle selection operations       |
| `smerge-mode-hydra`        | Smerge Mode          | `C-c m`           | Merge conflict resolution            |
| `toggles-hydra`            | Global               | `<f6>`            | Global option toggles                |
| `window-hydra`             | Global               | `C-c w` / `C-x o w`| Window management                  |
| `xwidget-hydra`            | XWidget Webkit Mode  | `?`               | Embedded browser controls            |
| `ztreediff-hydra`          | Ztree Diff Mode      | `C-<f5>`          | Directory comparison operations      |

## 📸 Screenshots

![Dashboard](https://user-images.githubusercontent.com/140797/97093362-c793b000-167d-11eb-924a-1892c6e95b8a.png "Dashboard (default theme)")

![color_icons](https://user-images.githubusercontent.com/140797/97093358-b9de2a80-167d-11eb-9e5a-8591b1248c37.png "Main (color icons)")

![markdown_magit](https://user-images.githubusercontent.com/140797/56489193-7f894f80-6513-11e9-9c95-ea1ef41efb1f.png "Markdown and Magit")

![org_classic](https://user-images.githubusercontent.com/140797/56489410-2837af00-6514-11e9-9e04-a1663ac479e8.png "Org (classic theme)")

![main_light](https://user-images.githubusercontent.com/140797/56489516-85cbfb80-6514-11e9-9dd4-8602bf8a4ec7.png "Main (light theme)")

## ❓ Frequently Asked Questions (FAQ)

### How do I display icons correctly in Centaur Emacs?

To properly display icons in Centaur Emacs:

1. Run `M-x centaur-install-fonts` to install all necessary fonts automatically
2. Alternatively, install fonts manually:
   - Install [nerd-icons](https://github.com/rainstormstudio/nerd-icons.el) package
   - Run `M-x nerd-icons-install-fonts` to install the required icon fonts
   - On Windows, fonts need to be installed manually
   - Note: `nerd-icons` supports both GUI and TUI modes

**:information_source: Tip**: To disable color icons, add `(setq nerd-icons-color-icons nil)` to your configuration.

For more details, visit [nerd-icons.el](https://github.com/rainstormstudio/nerd-icons.el).

### What should I do if packages cannot be installed?

Most package installation issues stem from network connectivity problems. Try these solutions:

- Refresh the package contents: `M-x package-refresh-contents`, then retry installation
- Adjust TLS settings: `(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")`
- Switch to a different ELPA mirror
- Change your network connection or use a proxy

For more information, see issue [#98](https://github.com/seagle0128/.emacs.d/issues/98).

### How do I search Chinese text using pinyin?

To search Chinese text via pinyin in Emacs, use: `C-s !`

**:warning: Note**: If you want to search for the literal `!` character, use: `C-s \!`

### How do I use the Centaur Dashboard?

The Centaur Dashboard enhances your Emacs startup experience:

- If `centaur-dashboard` is enabled, it opens automatically at startup
- Use `F2` to reopen the dashboard anytime after startup
- Press `?` or `h` in the dashboard for help
- Navigate with keybindings:
  - **Homepage**: `H`g
  - **Session**: `R` (resume), `L` (load last session)
  - **Settings**: `S`
  - **Update**: `U`
  - **Recent Files**: `r`
  - **Bookmarks**: `m`
  - **Projects**: `p`

### Does Centaur Emacs support Language Server Protocol (LSP)?

Yes, LSP is supported and enabled by default in Centaur Emacs:

- **Default Client**: `lsp-mode`
- **Alternative Client**: `eglot`
- To disable LSP: `(setq centaur-lsp nil)`

**:warning: Important**: You'll need to install appropriate language servers for your programming languages.
Refer to [lsp-mode: supported languages](https://github.com/emacs-lsp/lsp-mode#supported-languages)
or [eglot: connecting to a server](https://github.com/joaotavora/eglot#connecting-to-a-server) for setup details.

### How do I enable PlantUML in Org mode?

To enable PlantUML in Org mode:

1. Download the PlantUML JAR file
2. Add this configuration to your `custom.el`:
   ```elisp
   (setq org-plantuml-jar-path "/path/to/plantuml-x.x.x.jar")
   ```

### Why are Emacs environment variables and exec-path different between GUI and terminal?

This is a common issue where GUI Emacs doesn't inherit the same environment as your shell:

**:bulb: Solution**: Set environment variables in system startup files like `.profile`, `.bash_profile`, or `.zshenv`.
Centaur Emacs will then recognize and import these environment variables properly.

See issue #33 for more details.

### How do I use zoom-window in Centaur Emacs?

The zoom-window feature allows you to temporarily maximize/minimize windows:

**:memo: Reference**: See issue [#169](https://github.com/seagle0128/.emacs.d/issues/169#issuecomment-590035527) for usage instructions.

## 💖 Support the Project

If you find Centaur Emacs helpful, consider supporting its development:

**:coffee: Buy Me a Coffee**:

<div align="center">

**Your support helps maintain and improve this project!** :heart:

| Alipay | WeChat Pay |
|:------:|:----------:|
| <img src="https://user-images.githubusercontent.com/140797/65818854-44204900-e248-11e9-9cc5-3e6339587cd8.png" alt="Alipay" width="120"/> | <img src="https://user-images.githubusercontent.com/140797/65818844-366ac380-e248-11e9-931c-4bd872d0566b.png" alt="WeChat Pay" width="120"/> |

**International Donations**:

<a href="https://paypal.me/seagle0128" target="_blank">
  <img src="https://www.paypalobjects.com/digitalassets/c/website/marketing/apac/C2/logos-buttons/optimize/44_Grey_PayPal_Pill_Button.png" alt="PayPal" width="120"/>
</a>
&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://www.buymeacoffee.com/s9giES1" target="_blank">
  <img src="https://cdn.buymeacoffee.com/buttons/default-orange.png" alt="Buy Me A Coffee" width="160"/>
</a>

</div>
