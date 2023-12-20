;; init-template.el --- Initialize template configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2023 Vincent Zhang

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
;; Template configurations.
;;

;;; Code:

;; Configure Tempel
(use-package tempel
  :diminish
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
         :map tempel-map
         ("<backtab>" . tempel-previous)
         ("TAB" . tempel-next))
  :hook (after-init . global-tempel-abbrev-mode)
  :init
  (defun tempel-setup-capf ()
    "Setup completion at point."
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

;; Collection of templates for Tempel
(use-package tempel-collection)

;; Use eglot as inline template expander
(use-package eglot-tempel
  :after eglot
  :diminish
  :hook (after-init . eglot-tempel-mode))

(provide 'init-template)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-template.el ends here
