;; init-elixir.el --- Initialize elixir configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2025 N.Ahmet BASTUG

;; Author: N.Ahmet BASTUG <bastugn@itu.edu.tr>
;; URL: https://github.com/kosantosbik/.emacs.d

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
;; Elixir configurations.
;;

;;; Code:

(use-package elixir-mode
  :config
  (use-package alchemist
    :diminish (alchemist-mode alchemist-phoenix-mode)
    :hook (((elixir-mode elixir-ts-mode) . alchemist-mode)
           (alchemist-mode . alchemist-phoenix-mode))))

(provide 'init-elixir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-elixir.el ends here
