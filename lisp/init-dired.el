;; init-dired.el --- Initialize dired configurations.	-*- lexical-binding: t -*-

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
;; Directory configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; Directory operations
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
         ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  ;; Guess a default target directory
  (setq dired-dwim-target t)

  ;; Show directory first
  (setq dired-listing-switches "-alh --group-directories-first")

  (when (or sys/macp sys/unixp)
    (if (executable-find "gls")
        ;; Use GNU ls in `coreutils' if available.
        (setq insert-directory-program "gls")
      ;; Suppress the warning: `ls does not support --dired'.
      (setq dired-use-ls-dired nil
            dired-listing-switches "-alh"))))

;; Quick sort dired buffers
(when (or sys/linuxp sys/cygwinp
          (and (or sys/macp sys/unixp)
               (executable-find "gls")))
  (use-package dired-quick-sort
    :after dired
    :bind (:map dired-mode-map
           ("S" . dired-quick-sort-transient))
    :hook (dired-mode . dired-quick-sort-set-switches)))

;; Allow rsync from dired buffers
(use-package dired-rsync
  :after dired
  :bind (:map dired-mode-map
         ("C-c C-r" . dired-rsync)))

;; Colorful dired
(use-package diredfl
  :hook dired-mode)

;; Shows icons in dired
(use-package nerd-icons-dired
  :functions (nerd-icons-icon-for-dir my/nerd-icons-icon-for-dir)
  :hook dired-mode
  :init
  (defface nerd-icons-dired-dir-face
    '((t nil))
    "Face for the directory icon."
    :group 'nerd-icons-faces)
  (defun my/nerd-icons-icon-for-dir (dir &rest args)
    (apply #'nerd-icons-icon-for-dir
           dir
           :face 'nerd-icons-dired-dir-face
           args))
  (setq nerd-icons-dired-dir-icon-function #'my/nerd-icons-icon-for-dir))

;; Extra Dired functionality
(use-package dired-aux :ensure nil)
(use-package dired-x
  :ensure nil
  :init
  (when-let* ((cmd (cond (sys/macp "open")
                         (sys/linuxp "xdg-open")
                         (sys/win32p "start")
                         (t nil))))
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))
  :config
  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))

(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
