;;; init.el --- user init configuration.     -*- no-byte-compile: t -*-
;;
;; Filename: init.el
;; Description:
;; Author: Vincent Zhang
;; Maintainer:
;; Created: Wed Nov 29 00:57:38 2006
;; Version:
;; Last-Updated: Fri May 30 08:00:00 2016 (+0800)
;;           By: Vincent Zhang
;;     Update #: 5000
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Vincent's Emacs configuration
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(unless (>= emacs-major-version 24)
  (error "This requires Emacs 24 or later!"))

(setq load-prefer-newer t)

;; Load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; Customization
(require 'init-custom)

;; Packages
(package-initialize)
(require 'init-package)

(use-package benchmark-init
  :if my-profile-enable
  :config (benchmark-init/activate))

;; Preferences
(require 'init-basic)
(require 'init-ui)

(require 'init-edit)
(require 'init-smartparens)
(require 'init-ibuffer)
(require 'init-kill-ring)

(cond
 ((eq my-completion-method 'helm)
  (require 'init-helm))
 ((eq my-completion-method 'ivy)
  (require 'init-ivy))
 ((eq my-completion-method 'ido)
  (require 'init-ido))
 )

(require 'init-calendar)
(require 'init-bookmark)
(require 'init-highlight)
(require 'init-window)

(require 'init-yasnippet)

(cond
 ((eq my-ac-method 'company)
  (require 'init-company))
 ((eq my-ac-method 'auto-complete)
  (require 'init-auto-complete))
 )

(require 'init-shell)
(require 'init-eshell)

(require 'init-utils)

;; Programming
(require 'init-scm)
(require 'init-projectile)
(require 'init-flycheck)
(require 'init-tags)

(require 'init-emacs-lisp)
(require 'init-c)
(require 'init-dos)
(require 'init-python)
(require 'init-ruby)
(require 'init-web)
(require 'init-fish)
(require 'init-prog)

(require 'init-org)

;; Restore
(if my-desktop-restore
    (require 'init-restore))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
