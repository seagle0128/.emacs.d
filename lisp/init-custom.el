;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2025 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
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
;;

;;; Commentary:
;;
;; Customization.
;;

;;; Code:

(eval-when-compile
  (require 'package))

(defgroup centaur nil
  "Centaur Emacs customization."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/seagle0128/.emacs.d"))

(defcustom centaur-logo (expand-file-name
                         (if (display-graphic-p) "logo.png" "banner.txt")
                         user-emacs-directory)
  "Set Centaur logo. nil means official logo."
  :group 'centaur
  :type 'string)

(defcustom centaur-full-name user-full-name
  "Set user full name."
  :group 'centaur
  :type 'string)

(defcustom centaur-mail-address user-mail-address
  "Set user email address."
  :group 'centaur
  :type 'string)

(defcustom centaur-org-directory (expand-file-name "~/org")
  "Set org directory."
  :group 'centaur
  :type 'string)

(defcustom centaur-proxy "127.0.0.1:7897"
  "Set HTTP/HTTPS proxy."
  :group 'centaur
  :type 'string)

(defcustom centaur-socks-proxy "127.0.0.1:7897"
  "Set SOCKS proxy."
  :group 'centaur
  :type 'string)

(defcustom centaur-server t
  "Enable `server-mode' or not."
  :group 'centaur
  :type 'boolean)

(defcustom centaur-icon t
  "Display icons or not."
  :group 'centaur
  :type 'boolean)

;; Emacs Lisp Package Archive (ELPA)
;; @see https://github.com/melpa/melpa and https://elpa.emacs-china.org/.
(defcustom centaur-package-archives-alist
  (let ((proto (if (gnutls-available-p) "https" "http")))
    `((melpa    . (("gnu"    . ,(format "%s://elpa.gnu.org/packages/" proto))
                   ("nongnu" . ,(format "%s://elpa.nongnu.org/nongnu/" proto))
                   ("melpa"  . ,(format "%s://melpa.org/packages/" proto))))
      (bfsu     . (("gnu"    . ,(format "%s://mirrors.bfsu.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.bfsu.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.bfsu.edu.cn/elpa/melpa/" proto))))
      (iscas    . (("gnu"    . ,(format "%s://mirror.iscas.ac.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirror.iscas.ac.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirror.iscas.ac.cn/elpa/melpa/" proto))))
      (netease  . (("gnu"    . ,(format "%s://mirrors.163.com/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.163.com/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.163.com/elpa/melpa/" proto))))
      (sjtu     . (("gnu"    . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/" proto))))
      (tuna     . (("gnu"    . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/" proto))))
      (ustc     . (("gnu"    . ,(format "%s://mirrors.ustc.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.ustc.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.ustc.edu.cn/elpa/melpa/" proto))))))
  "A list of the package archives."
  :group 'centaur
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))

(defcustom centaur-package-archives 'melpa
  "Set package archives from which to fetch."
  :group 'centaur
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives
               (or (alist-get value centaur-package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    centaur-package-archives-alist)))

(defcustom centaur-theme-alist
  '((default . doom-one)
    (pro     . doom-monokai-pro)
    (dark    . doom-vibrant)
    (light   . doom-one-light)
    (warm    . doom-solarized-light)
    (cold    . doom-palenight)
    (day     . doom-tomorrow-day)
    (night   . doom-tomorrow-night))
  "List of themes mapped to internal themes."
  :group 'centaur
  :type '(alist :key-type (symbol :tag "Theme")
                :value-type (symbol :tag "Internal theme")))

(defcustom centaur-auto-themes '(("8:00"  . doom-one-light)
				                 ("19:00" . doom-one))
  "List of themes mapped to the time they should be loaded.

The keywords `:sunrise' and `:sunset' can be used for the time
if the option `calendar-latitude' and option `calendar-longitude' are set.
For example:
  \\='((:sunrise . doom-one-light)
    (:sunset  . doom-one))"
  :group 'centaur
  :type '(alist :key-type (string :tag "Time")
                :value-type (symbol :tag "Theme")))

(defcustom centaur-system-themes '((dark  . doom-one)
                                   (light . doom-one-light))
  "List of themes related the system appearance.

It's only available on macOS currently."
  :group 'centaur
  :type '(alist :key-type (symbol :tag "Appearance")
                :value-type (symbol :tag "Theme")))

(defcustom centaur-theme 'default
  "The color theme."
  :group 'centaur
  :type `(choice (const :tag "Auto" auto)
                 (const :tag "Random" random)
                 (const :tag "System" system)
                 ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    centaur-theme-alist)
                 symbol))

(defcustom centaur-completion-style 'childframe
  "Completion display style."
  :group 'centaur
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Child Frame" childframe)))

(defcustom centaur-frame-maximized-on-startup nil
  "Maximize frame on startup or not."
  :group 'centaur
  :type 'boolean)

(defcustom centaur-dashboard (not (daemonp))
  "Display dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 'centaur
  :type 'boolean)

(defcustom centaur-lsp 'eglot
  "Set language server.

`lsp-mode': See https://github.com/emacs-lsp/lsp-mode.
`eglot': See https://github.com/joaotavora/eglot.
nil means disabled."
  :group 'centaur
  :type '(choice (const :tag "LSP Mode" lsp-mode)
                 (const :tag "Eglot" eglot)
                 (const :tag "Disable" nil)))

(defcustom centaur-tree-sitter t
  "Enable tree-sitter or not.
Native tree-sitter is introduced in 29."
  :group 'centaur
  :type 'boolean)

(defcustom centaur-lsp-format-on-save nil
  "Auto format buffers on save."
  :group 'centaur
  :type 'boolean)

(defcustom centaur-lsp-format-on-save-ignore-modes
  '(c-mode c++-mode python-mode markdown-mode)
  "The modes that don't auto format and organize imports while saving the buffers.
`prog-mode' means ignoring all derived modes."
  :group 'centaur
  :type '(repeat (symbol :tag "Major-Mode")))

(defcustom centaur-chinese-calendar nil
  "Enable Chinese calendar or not."
  :group 'centaur
  :type 'boolean)

(defcustom centaur-player nil
  "Enable players or not."
  :group 'centaur
  :type 'boolean)

(defcustom centaur-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-"     . ?←)
    ("->"     . ?→)
    ("->>"    . ?↠)
    ("=>"     . ?⇒)
    ("map"    . ?↦)
    ("/="     . ?≠)
    ("!="     . ?≠)
    ("=="     . ?≡)
    ("<="     . ?≤)
    (">="     . ?≥)
    ("=<<"    . (?= (Br . Bl) ?≪))
    (">>="    . (?≫ (Br . Bl) ?=))
    ("<=<"    . ?↢)
    (">=>"    . ?↣)
    ("&&"     . ?∧)
    ("||"     . ?∨)
    ("not"    . ?¬))
  "A list of symbol prettifications.
Nil to use font supports ligatures."
  :group 'centaur
  :type '(alist :key-type string :value-type (choice character sexp)))

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
