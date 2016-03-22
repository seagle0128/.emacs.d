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

;; IDO
(use-package ido
  :defer t
  :config
  (progn
    (ido-mode 1)
    (ido-everywhere 1)
    (setq ido-use-filename-at-point 'guess)
    (setq ido-create-new-buffer 'always)
    (setq ido-enable-flex-matching t))

  (progn
    (use-package ido-ubiquitous
      :defer t
      :config (ido-ubiquitous-mode 1))

    (use-package ido-at-point
      :defer t
      :config (ido-at-point-mode 1))

    (use-package ido-complete-space-or-hyphen
      :defer t
      :config (ido-complete-space-or-hyphen-enable))

    (use-package ido-sort-mtime
      :defer t
      :config (ido-sort-mtime-mode 1))

    (use-package ido-vertical-mode
      :defer t
      :disabled t
      :config (ido-vertical-mode 1))

    (use-package flx-ido
      :defer t
      :config (flx-ido-mode 1))

    (use-package ido-load-library
      :defer t
      :config (defalias 'load-library 'ido-load-library))

    ;; Smex
    (use-package smex
      :defer t
      :bind
      (("M-x" . smex)
       ("M-X" . smex-major-mode-commands))
      ;; This is your old M-x.
      ("C-c C-c M-x" . execute-extended-command))
    ))

(provide 'init-ido)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ido.el ends here
