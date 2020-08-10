;; init-dired.el --- Initialize dired configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2020 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

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
;; Directory configurations.
;;

;;; Code:

(require 'init-const)
(require 'init-funcs)

;; Directory operations
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
         ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  (when sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when (executable-find "gls")
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (when (or (and sys/macp (executable-find "gls"))
            (and (not sys/macp) (executable-find "ls")))
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)

    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first")

    ;; Quick sort dired buffers via hydra
    (use-package dired-quick-sort
      :bind (:map dired-mode-map
             ("S" . hydra-dired-quick-sort/body))))

  ;; Show git info in dired
  (use-package dired-git-info
    :bind (:map dired-mode-map
           (")" . dired-git-info-mode)))

  ;; Allow rsync from dired buffers
  (use-package dired-rsync
    :bind (:map dired-mode-map
           ("C-c C-r" . dired-rsync)))

  ;; Colourful dired
  (use-package diredfl
    :init (diredfl-global-mode 1))

  ;; Shows icons
  (use-package all-the-icons-dired
    :diminish
    :if (icons-displayable-p)
    :hook (dired-mode . all-the-icons-dired-mode)
    :config
    ;; FIXME: Refresh after creating or renaming the files/directories.
    ;; @see https://github.com/jtbm37/all-the-icons-dired/issues/34.
    (with-no-warnings
      (advice-add #'dired-do-create-files :around #'all-the-icons-dired--refresh-advice)
      (advice-add #'dired-create-directory :around #'all-the-icons-dired--refresh-advice)
      (advice-add #'wdired-abort-changes :around #'all-the-icons-dired--refresh-advice))

    (with-no-warnings
      (defun my-all-the-icons-dired--refresh ()
        "Display the icons of files in a dired buffer."
        (all-the-icons-dired--remove-all-overlays)
        ;; NOTE: don't display icons it too many items
        (if (<= (count-lines (point-min) (point-max)) 1000)
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (when (dired-move-to-filename nil)
                  (let ((file (file-local-name (dired-get-filename 'relative 'noerror))))
                    (when file
                      (let ((icon (if (file-directory-p file)
                                      (all-the-icons-icon-for-dir file
                                                                  :face 'all-the-icons-dired-dir-face
                                                                  :height 0.9
                                                                  :v-adjust all-the-icons-dired-v-adjust)
                                    (all-the-icons-icon-for-file file :height 0.9 :v-adjust all-the-icons-dired-v-adjust))))
                        (if (member file '("." ".."))
                            (all-the-icons-dired--add-overlay (point) "  \t")
                          (all-the-icons-dired--add-overlay (point) (concat icon "\t")))))))
                (forward-line 1)))
          (message "Not display icons because of too many items.")))
      (advice-add #'all-the-icons-dired--refresh :override #'my-all-the-icons-dired--refresh)))

  ;; Extra Dired functionality
  (use-package dired-aux :ensure nil)
  (use-package dired-x
    :ensure nil
    :demand
    :config
    (let ((cmd (cond (sys/mac-x-p "open")
                     (sys/linux-x-p "xdg-open")
                     (sys/win32p "start")
                     (t ""))))
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

    (setq dired-omit-files
          (concat dired-omit-files
                  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))

(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
