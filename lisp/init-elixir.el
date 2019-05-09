;; init-elixir.el --- Initialize elixir configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 N.Ahmet BASTUG

;; Author: N.Ahmet BASTUG <bastugn@itu.edu.tr>
;; URL: https://github.com/kosantosbik/.emacs.d

;; This file is not part of GNU Emacs.
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

;;; Commentary:
;;
;; Elixir configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package elixir-mode)

(use-package alchemist
  :hook ((elixir-mode . alchemist-mode)
         (elixir-mode . alchemist-phoenix-mode))
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode)
  (add-hook 'elixir-mode-hook 'alchemist-phoenix-mode))

(use-package flycheck-mix
  :config
  (flycheck-mix-setup))

(use-package flycheck-credo
  :config
  (eval-after-load 'flycheck
    '(flycheck-credo-setup))
  )

(provide 'init-elixir)
