;; init-docker.el --- Initialize docker configurations.	-*- lexical-binding: t -*-

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
;; Docker configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; Docker
(use-package docker
  :defines docker-image-run-arguments
  :bind ("C-c D" . docker)
  :init (setq docker-image-run-arguments '("-i" "-t" "--rm")
              docker-container-shell-file-name "/bin/bash"))

;;`tramp-container' is builtin since 29
(unless emacs/>=29p
  (use-package docker-tramp))

(use-package dockerfile-mode)

(provide 'init-docker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-docker.el ends here
