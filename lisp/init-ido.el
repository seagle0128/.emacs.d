;;; init-ido.el --- Initialize ido configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Ido configurations.
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

;; Smex
(use-package smex
  :defer t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c M-x" . execute-extended-command)))

;; IDO
(use-package ido
  :defer t
  :config
  (progn
    (ido-mode 1)
    (ido-everywhere 1)
    (setq ido-use-filename-at-point 'guess)
    (setq ido-create-new-buffer 'always)
    (setq ido-enable-flex-matching t)

    (use-package ido-ubiquitous
      :config (ido-ubiquitous-mode 1))

    (use-package ido-at-point
      :config (ido-at-point-mode 1))

    (use-package ido-complete-space-or-hyphen
      :config (ido-complete-space-or-hyphen-enable))

    (use-package ido-sort-mtime
      :config (ido-sort-mtime-mode 1))

    (use-package ido-vertical-mode
      :disabled t
      :config (ido-vertical-mode 1))

    (use-package flx-ido
      :config (flx-ido-mode 1))

    (use-package ido-load-library
      :config (defalias 'load-library 'ido-load-library))
    
    (use-package idomenu
      :defer t
      :bind ("C-." . idomenu))

    (use-package swoop
      :defer t
      :bind (("C-o" . swoop)
             ("C-M-o" . swoop-multi)
             ("M-o" . swoop-pcre-regexp)
             ("C-S-o" . swoop-back-to-last-position))
      :init
      (setq swoop-font-size-change: nil))
    ))

(provide 'init-ido)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ido.el ends here
