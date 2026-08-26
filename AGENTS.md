# AGENTS.md - Centaur Emacs Configuration

## Overview

**Centaur Emacs** - A fancy and fast Emacs distribution.
- **Repo**: https://github.com/seagle0128/.emacs.d
- **Emacs**: 28.1+ (CI tests 28.2, 29.4, 30.2, 31.1, snapshot)
- **Platforms**: Linux, macOS, Windows (Cygwin/MSYS2)

---

## Build, Lint, Test

### Test Configuration

**CI Command** (equivalent local test):
```bash
emacs -q --batch \
  --eval "(message \"Testing...\")" \
  --eval "(let ((early-init-file (locate-user-emacs-file \"early-init.el\"))
                (user-init-file (locate-user-emacs-file \"init.el\")))
            (setq package-check-signature nil)
            (load early-init-file)
            (load user-init-file))" \
  --eval "(message \"Testing...done\")"
```

**Note**: CI also copies `custom-example.el` → `custom.el` before running. CI only validates that config loads — no automated test suites exist.

### Byte Compilation & Lint

**IMPORTANT**: Always use `emacsclient` (never `emacs` or `--batch`):
```bash
emacsclient --eval '(byte-compile-file "/path/to/file.el")'
emacsclient --eval '(with-temp-buffer (insert-file-contents "/path/to/file.el") (check-parens))'
```

**Linting**: `elisp-flymake-byte-compile` (Flymake backend, enabled in `prog-mode`)

### Package Management

```elisp
M-x package-refresh-contents    ; Refresh archives
M-x centaur-update              ; Update configs + packages
M-x centaur-update-packages     ; Update packages only
M-x set-package-archives        ; Switch mirrors
```

### Package Archives

Options: `melpa`, `bfsu`, `iscas`, `netease`, `sjtu`, `tencent`, `tuna`, `ustc` (all except `melpa` are China mirrors)

Set in `custom.el`:
```elisp
(setq centaur-package-archives 'melpa)
```

---

## Architecture

### Load Order

```
early-init.el (Emacs 27+)
    ↓
init.el
    ↓
init-const (constants, sys/* predicates)  ← Layer 0: Foundation
    ↓
init-custom (defcustom API)               ← Layer 1: API
    ↓
init-funcs (utility functions)            ← Layer 2: Utilities
    ↓
init-package (package system)              ← Layer 3: Package
    ↓
init-base (base settings, gcmh-mode)       ← Layer 4: Core
    ↓
init-hydra, init-ui, init-edit, ...       ← Layer 5+: Features
```

### Startup Optimizations

**early-init.el**:
- `gc-cons-threshold` → `most-positive-fixnum` (GUI) or 128MB (CLI)
- `file-name-handler-alist` → Set to `nil` via lexical `let`, restored in `emacs-startup-hook` (priority 101)
- `load-suffixes` → `(".elc" ".el")` only (skip .so/.dll/.gz)
- `native-comp-jit-compilation` → `nil` (defer runtime compilation)
- UI disabled via `default-frame-alist` (before frame creation)
- `load-path-filter-function` → `load-path-filter-cache-directory-files` on Emacs 31+ (~15% startup improvement)

**init-base.el**:
- `gcmh-mode` restores GC to ~64MB after startup — never disable
- `inhibit-compacting-font-caches t` (prevents GC jank)

### Critical Dependencies

**Must load in order**:
1. `init-const` - No dependencies
2. `init-custom` - Depends on init-const
3. `init-funcs` - Depends on init-const, init-custom
4. `init-package` - Depends on init-const, init-custom, init-funcs
5. All other `init-*.el` files - Depend on init-const, init-custom

**NO circular dependencies** - Architecture is strictly layered.

### Platform Predicates (init-const.el)

```elisp
sys/win32p       ; Windows native
sys/macp         ; macOS
sys/linuxp       ; Linux
sys/mac-port-p   ; Emacs Mac port build
sys/mac-ns-p     ; macOS Cocoa (NextStep)
sys/mac-cocoa-p  ; macOS with Cocoa feature
sys/mac-x-p      ; macOS with GUI
sys/linux-x-p    ; Linux with GUI
sys/cygwinp      ; Cygwin
sys/rootp        ; Running as root
daemonp          ; Daemon mode
emacs/>=29p      ; Version checks
emacs/>=29.2p
emacs/>=30p
emacs/>=31p
emacs/>=32p
```

---

## Code Style

### File Headers

**Core init files** (`lisp/init-*.el`):
```elisp
;; <filename> --- <description>	-*- lexical-binding: t -*-
```

**User-editable and entry point files** (`custom.el`, ``early-init.el`, `init.el`, `init-mini.el`):
```elisp
;;; <filename> --- <description> -*- lexical-binding: t no-byte-compile: t -*-
```

**`no-byte-compile: t` is required** for user-editable files and entry points to prevent native compilation issues. Core init files (`lisp/init-*.el`) do NOT need this flag.

### Naming

- Functions/variables: `kebab-case` (e.g., `centaur-update`, `centaur-full-name`)
- Internal variables: `centaur--` prefix (double hyphen)
- Faces: `-face` suffix (e.g., `centaur-heading-face`)
- Files: `init-<feature>.el` (e.g., `init-base.el`, `init-prog.el`)

### Required Imports

```elisp
;;; Code:
(eval-when-compile
  (require 'init-const)
  (require 'init-custom))
(require 'init-funcs)
```

### Forward Declarations

```elisp
(declare-function browse-url-file-url "browse-url")
(declare-function consult-theme "ext:consult")
```

### Platform Conditionals

```elisp
(cond
 ((sys/win32p) ...)
 ((sys/mac-port-p) ...)
 ((sys/linuxp) ...))
```

### use-package Defaults

Set in `init-package.el`:
```elisp
(setq use-package-always-ensure t
      use-package-always-defer t       ; Lazy load by default
      use-package-expand-minimally t
      use-package-enable-imenu-support t)
```

---

## Customization

### File Hierarchy

| File | Tracked? | Purpose |
|------|----------|---------|
| `custom-example.el` | ✓ | Template for custom.el |
| `custom.el` | ✗ | User customizations (auto-copied on first startup) |
| `custom-post.el` | ✗ | Post-init overrides (loaded via after-init-hook) |

### Loading Sequence

1. `early-init.el`
2. `init.el` → Loads init-const, init-custom, init-funcs
3. `init-package.el` → Copies `custom-example.el` to `custom.el`, loads `custom.el`
4. `after-init-hook` → Loads `custom-post.el` (or `custom-post.org`)
5. All other `init-*.el` files

### Customization API (centaur-* variables)

All variables defined in `init-custom.el`, accessible via `M-x customize-group centaur`:

```elisp
;; User info
centaur-full-name, centaur-mail-address

;; Network
centaur-proxy, centaur-socks-proxy

;; Package
centaur-package-archives  ; melpa, bfsu, iscas, netease, sjtu, tencent, tuna, ustc

;; UI
centaur-icon, centaur-theme, centaur-completion-style, centaur-dashboard
centaur-frame-maximized-on-startup, centaur-logo

;; Development
centaur-lsp              ; nil or t (default: t)
centaur-tree-sitter      ; nil or t (default: t, requires Emacs 29+)
centaur-use-exec-path-from-shell

;; Features
centaur-chinese-calendar, centaur-player, centaur-prettify-symbols-alist
centaur-org-directory
```

### What Users SHOULD Modify

- `custom.el` - Centaur variable settings, fonts, proxy
- `custom-post.el` - Advanced customizations, use-package configs, hooks

### What Users SHOULD NOT Modify

- All `lisp/init-*.el` files - Core configuration
- `early-init.el` - Startup optimizations
- `init.el` - Main initialization

### Extension Points

- `after-init-hook` - Post-init customizations
- `emacs-startup-hook` - Startup completions
- Mode-specific hooks (e.g., `prog-mode-hook`, `org-mode-hook`)

---

## Key Gotchas

### 1. GC Restoration is Critical

**early-init.el** sets `gc-cons-threshold` to `most-positive-fixnum`. Without restoration, Emacs will consume unlimited memory.

**Solution**: `gcmh-mode` (in `init-base.el`) restores to ~64MB after startup. Never disable gcmh-mode.

### 2. file-name-handler-alist is Lexically Scoped

`file-name-handler-alist` is NOT saved to a variable. It's captured in a `let` binding inside early-init.el and restored via `emacs-startup-hook` with priority 101. Do not reference `centaur--file-name-handler-alist` — that name does not exist.

### 3. no-byte-compile is NOT Universal

Only **user-editable and entry point files** need `no-byte-compile: t`: `custom.el`, `early-init.el`, `init.el`, `init-mini.el`. Core init files in `lisp/init-*.el` do NOT have this flag and should not get it.

### 4. custom-post.el Loading Order

Loaded via `after-init-hook` after all packages initialized. Use this for package-dependent customizations.

### 5. Windows Package Optimization

On Windows, all packages are consolidated into `elpa/all` to reduce `load-path` entries (performance hack in `init-package.el:90-145`).

### 6. Daemon-Specific Handling

- `centaur-dashboard` defaults to `nil` in daemon mode
- No GUI features (PDF, xwidget) work in daemon

### 7. No Automated Tests

CI only validates that config loads. No unit tests, integration tests, or ERT tests exist.

### 8. emacsclient Only

Always use `emacsclient` for operations (byte compilation, evaluation, testing). Never `emacs` or `--batch`.

---

## References

- [Centaur Emacs README](https://github.com/seagle0128/.emacs.d)
- [use-package docs](https://github.com/jwiegley/use-package/blob/master/use-package.org)
- [Emacs Lisp Reference](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
