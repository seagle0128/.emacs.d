# AGENTS.md - Centaur Emacs Configuration

## Overview

This is the **Centaur Emacs** configuration - an Emacs distribution that enhances the default Emacs experience. The codebase consists of **Emacs Lisp (.el) files** organized in the `lisp/` directory.

- **Repository**: https://github.com/seagle0128/.emacs.d
- **Emacs Version Required**: 28.1+ (tested against 28.2, 29.4, 30.1, and snapshot)
- **Platforms**: Linux, macOS, Windows (via Cygwin/MSYS2)

---

## Build, Lint, and Test Commands

### Loading the Configuration (Basic Test)

```bash
# Full config test (batch mode)
emacs -q --batch \
  --eval "(message \"Testing...\")" \
  --eval "(let ((early-init-file (locate-user-emacs-file \"early-init.el\"))
              (user-init-file (locate-user-emacs-file \"init.el\")))
          (and (>= emacs-major-version 27) (load early-init-file))
          (load user-init-file))" \
  --eval "(message \"Testing...done\")"

# Minimal config test (for troubleshooting)
emacs -Q -l ~/.emacs.d/init-mini.el
```

### Byte Compilation

```elisp
;; In Emacs
M-x byte-compile-file           ; Compile current buffer
M-x byte-recompile-directory    ; Recompile directory
M-x centaur-recompile            ; Custom command if available
```

### Linting

The project uses `elisp-flymake-byte-compile` for on-the-fly linting. In Emacs:

```elisp
M-x flymake-mode                ; Enable flymake
M-x elisp-flymake-byte-compile  ; Byte compile for errors
```

### Package Archives

Default archive is `melpa`. Change in `custom.el`:
```elisp
(setq centaur-package-archives 'melpa)  ; Options: melpa, bfsu, iscas, netease, sjtu, tencent, tuna, ustc
```

---

## Code Style Guidelines

### File Header Convention

Every `.el` file MUST start with this header:

```elisp
;;; <filename> --- <description> -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2006-2026 Vincent Zhang
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; <description>

;;; Code:

;; ... code here ...
```

### Lexical Binding

- **ALWAYS use `lexical-binding: t`** in file headers
- **ALWAYS use `no-byte-compile: t`** in this project to prevent native compilation issues

### Naming Conventions

| Type | Convention | Examples |
|------|------------|----------|
| Files | `init-<feature>.el` | `init-base.el`, `init-prog.el` |
| Functions | `kebab-case` | `defun centaur-update`, `defun update-load-path` |
| Variables | `kebab-case` | `setq centaur-full-name` |
| Constants | `kebab-case` with `*` prefix | `defconst centaur-version` |
| Faces | `kebab-case` with `-face` suffix | `centaur-heading-face` |
| Groups | `kebab-case` | `defgroup centaur` |

### Required Imports

```elisp
;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(require 'init-funcs)
```

### Forward Declarations

Use `declare-function` for functions from external packages:

```elisp
(declare-function browse-url-file-url "browse-url")
(declare-function consult-theme "ext:consult")
(declare-function nerd-icons-install-fonts "ext:nerd-icons")
```

### Error Handling

- Use `with-no-warnings` to suppress compiler warnings for known-safe code
- Use `condition-case` for graceful error handling
- Never use empty catch blocks

```elisp
(with-no-warnings
  (setq some-var t))

(condition-case err
    (some-function)
  (error (message "Error: %s" err)))
```

### Use `cl-lib` Instead of Old CL

```elisp
(require 'cl-lib)
;; Use cl-lib functions: cl-loop, cl-push, cl-pop, cl-first, cl-rest, etc.
;; AVOID: funcall, apply, mapcar* from old CL
```

### Use `defvar` and `defcustom` Properly

```elisp
(defvar internal-var nil "Internal variable.")

(defcustom user-option t
  "User-configurable option."
  :type 'boolean
  :group 'centaur)
```

### Conditionals for Platform-Specific Code

```elisp
(cond
 ((sys/win32p) (setq ...))
 ((sys/mac-port-p) (setq ...))
 ((sys/linuxp) (setq ...)))
```

Common system predicates: `sys/win32p`, `sys/macp`, `sys/mac-port-p`, `sys/linuxp`, `daemonp`

### Minimize Startup Impact

- Use `autoload` for functions not needed at startup
- Use `with-eval-after-load` instead of `eval-after-load`
- Defer loading with `demand` in use-package `:demand` keyword

---

## Project Structure

```
.emacs.d/
├── early-init.el          ; Early initialization (Emacs 27+)
├── init.el                ; Main entry point
├── init-mini.el           ; Minimal config for troubleshooting
├── custom.el              ; User customizations (copy from custom-example.el)
├── env.el                 ; Environment variables
├── lisp/
│   ├── init-base.el       ; Base settings
│   ├── init-funcs.el      ; Utility functions
│   ├── init-const.el     ; Constants
│   ├── init-custom.el    ; Customization API
│   ├── init-package.el   ; Package management
│   ├── init-ui.el        ; UI settings
│   ├── init-prog.el      ; Programming modes
│   └── init-*.el         ; Language/feature-specific configs
└── .github/workflows/
    └── ci.yml            ; CI configuration
```

---

## Common Development Tasks

### Adding a New Package

1. Add configuration in a new `lisp/init-<feature>.el` file
2. Require it in `init.el` or in the appropriate init file
3. Use `use-package` for declarative configuration

### Running a Single Feature Test

```bash
emacs -q --batch \
  -l ~/.emacs.d/lisp/init-funcs.el \
  -l ~/.emacs.d/lisp/init-const.el \
  -l ~/.emacs.d/lisp/init-custom.el \
  --eval "(message \"Feature test complete\")"
```

### Updating Packages

In Emacs:
- `M-x package-refresh-contents` - Refresh package contents
- `M-x centaur-update` - Update all (configs + packages)
- `M-x centaur-update-packages` - Update packages only

---

## Key Configuration Variables

| Variable                   | Description    | Default      |
|----------------------------|----------------|--------------|
| `centaur-full-name`        | User full name | User's name  |
| `centaur-mail-address`     | User email     | User's email |
| `centaur-theme`            | Theme choice   | `auto`       |
| `centaur-lsp`              | LSP client     | `lsp-mode`   |
| `centaur-icon`             | Display icons  | `t`          |
| `centaur-package-archives` | Package mirror | `melpa`      |

---

## Troubleshooting

- **Packages won't install**: Check network, try different mirror, or use proxy
- **Emacs crashes on startup**: Use `emacs -Q -l init-mini.el` to diagnose
- **Icons not showing**: Run `M-x nerd-icons-install-fonts` or `M-x centaur-install-fonts`

---

## References

- [Centaur Emacs README](https://github.com/seagle0128/.emacs.d)
- [Emacs Lisp Reference](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
- [use-package Documentation](https://github.com/jwiegley/use-package)
