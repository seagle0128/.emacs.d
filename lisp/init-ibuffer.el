;; init-buffer.el --- Initialize ibuffer configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2026 Vincent Zhang

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
;; IBuffer configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package ibuffer
  :ensure nil
  :custom
  (ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  (ibuffer-human-readable-size t)
  :bind ("C-x C-b" . ibuffer))

;; Display icons for buffers
(use-package nerd-icons-ibuffer
  :custom (nerd-icons-ibuffer-icon centaur-icon)
  :hook ibuffer-mode)

;; Group ibuffer's list by project
(use-package ibuffer-project
  :autoload (ibuffer-project-generate-filter-groups ibuffer-do-sort-by-project-file-relative)
  :functions icons-displayable-p
  :custom (ibuffer-project-use-cache t)
  :hook (ibuffer . (lambda ()
                     "Group ibuffer's list by project."
                     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
                     (unless (eq ibuffer-sorting-mode 'project-file-relative)
                       (ibuffer-do-sort-by-project-file-relative))))
  :config
  (with-no-warnings
    (defun my/ibuffer-project-group-name (root type)
      "Return group name for project ROOT and TYPE."
      (if (and (stringp type) (> (length type) 0))
          (format "%s %s" type root)
        (format "%s" root)))
    (if (icons-displayable-p)
        (progn
          (advice-add #'ibuffer-project-group-name :override #'my/ibuffer-project-group-name)
          (setq ibuffer-project-root-functions
                `((ibuffer-project-project-root . ,(nerd-icons-octicon "nf-oct-repo" :height 1.2 :face ibuffer-filter-group-name-face))
                  (file-remote-p . ,(nerd-icons-mdicon "nf-md-remote_desktop" :height 1.2 :face ibuffer-filter-group-name-face))
                  (identity . ,(nerd-icons-octicon "nf-oct-file_directory" :height 1.2 :face ibuffer-filter-group-name-face)))))
      (progn
        (advice-remove #'ibuffer-project-group-name #'my/ibuffer-project-group-name)
        (setq ibuffer-project-root-functions
              '((ibuffer-project-project-root . "Project")
                (file-remote-p . "Remote")
                (identity . "Directory")))))))

(provide 'init-ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ibuffer.el ends here
