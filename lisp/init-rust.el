;; init-rust.el --- Initialize Rust configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2025 Vincent Zhang

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
;; Rust configurations.
;;

;;; Code:

;; Rust
(use-package rust-mode
  :mode ("\\.rs\\'" . rustic-mode)
  :init (setq rust-format-on-save t
              rust-mode-treesitter-derive t)
  :config
  ;; HACK: `global-treesit-auto-mode' will override `rust-mode'.
  (define-derived-mode rustic-mode rust-mode "Rust"
    "Major mode for Rust code.

\\{rust-mode-map}")

  (when (centaur-treesit-available-p)
    (setq auto-mode-alist (delete '("\\.rs\\'" . rust-mode) auto-mode-alist))
    (setq auto-mode-alist (delete '("\\.rs\\'" . rust-ts-mode) auto-mode-alist))))

(use-package ron-mode
  :mode ("\\.ron" . ron-mode))

(provide 'init-rust)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rust.el ends here
