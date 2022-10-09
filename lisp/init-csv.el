;; init-csv.el --- Initialize c configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2022 Vincent Zhang

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
;; csv configuration.
;;

;;; Code:

;; csv Mode
(use-package csv-mode
  :bind (:map csv-mode-map
         ("C-c C-a" . csv-align-fields)
         ("C-c C-u" . csv-unalign-fields)
         ("C-c C-k" . csv-kill-fields)
         ("C-c C-y" . csv-yank-fields))
  :config
  (setq csv-align-max-width 14)
  ;; csv-invisibility-default t
  ;; csv-toggle-invisibility nil
  ;; (add-hook 'csv-mode-hook 'csv-guess-set-separator))
  (add-hook 'csv-mode-hook 'csv-align-mode))

(provide 'init-csv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-csv.el ends here
