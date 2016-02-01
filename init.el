;;; init.el --- my personal emacs configuration
;;
;; Filename: init.el
;; Description:
;; Author: Vincent Zhang
;; Maintainer:
;; Created: Wed Nov 29 00:57:38 2006
;; Version:
;; Last-Updated: Fri Jan 30 08:00:00 2016 (+0800)
;;           By: Vincent Zhang
;;     Update #: 4000
;; URL:
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


;; Added by Package.el. This must come before configurations of
;; installed packages. Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

;; Load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; Preparation
(require 'init-core)
(require 'init-package)

(benchmark-init/activate)

;; Preferences
(require 'init-ui)
(require 'init-diminish)

(require 'init-basic)
(require 'init-calendar)

(require 'init-edit)

(require 'init-helm)
;; (require 'init-ido)

(require 'init-bookmark)
(require 'init-highlight)
(require 'init-kill-ring)
(require 'init-window)

(require 'init-auto-complete)
(require 'init-yasnippet)

(require 'init-term)
(require 'init-shell)
(require 'init-eshell)

(require 'init-utils)

;; Programming
(require 'init-scm)
(require 'init-projectile)
(require 'init-flycheck)
(require 'init-tags)

(require 'init-lisp)
(require 'init-c)
(require 'init-dos)
(require 'init-python)
(require 'init-ruby)
(require 'init-web)
(require 'init-org)

;; Restore
(require 'init-restore)
;; (require 'recentf)

;; Post initialization
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))
  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
