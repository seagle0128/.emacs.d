;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2025 Vincent Zhang

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
;; Emacs Package management configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(declare-function set-package-archives "init-funcs")
(declare-function centaur-test-package-archives "init-funcs")

;; At first startup
(when (and (file-exists-p centaur-custom-example-file)
           (not (file-exists-p custom-file)))
  (copy-file centaur-custom-example-file custom-file)

  ;; Test and select the fastest package archives
  (message "Testing connection... Please wait a moment.")
  (set-package-archives (centaur-test-package-archives 'no-chart)))

;; Load `custom-file'
(and (file-readable-p custom-file) (load custom-file))

;; Load custom-post file
(defun load-custom-post-file ()
  "Load custom-post file."
  (cond ((file-exists-p centaur-custom-post-org-file)
         (and (fboundp 'org-babel-load-file)
              (org-babel-load-file centaur-custom-post-org-file)))
        ((file-exists-p centaur-custom-post-file)
         (load centaur-custom-post-file))))
(add-hook 'after-init-hook #'load-custom-post-file)

;; HACK: DO NOT save `package-selected-packages' to `custom-file'
;; @see https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-package--save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to option `custom-file'."
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'my-package--save-selected-packages)))
(advice-add 'package--save-selected-packages :override #'my-package--save-selected-packages)

;; Set ELPA packages
(set-package-archives centaur-package-archives nil nil t)

;; To speedup the Emacs windows, reducing the count on searching `load-path'
;; is significant, there're ways to do that:
;;  1. Use the load-hints in https://mail.gnu.org/archive/html/bug-gnu-emacs/2024-10/msg00905.html
;;  2. Combin the packages into one directory.
;; This code slice can make the package.el to install packages in the package-user-dir/"all" to speedup Emacs:
(when sys/win32p
  (defun package-user-all-dir ()
    (file-name-concat package-user-dir "all"))

  (define-advice package-unpack (:around (ofun pkg-desc) ADV)
    (let ((pkg-dir (funcall ofun pkg-desc)))
      (when-let* (pkg-dir
                  (pkg-file (format "%s-pkg.el" (package-desc-name pkg-desc)))
                  (files (seq-difference (directory-files pkg-dir)
                                         '("." "..") 'string=))
                  (target-dir (file-name-as-directory (package-user-all-dir))))
        (make-directory target-dir t)
        (unless (seq-intersection files (directory-files target-dir) #'string=)
          (with-current-buffer (find-file-noselect (file-name-concat pkg-dir pkg-file))
            (goto-char (point-max))
            (when (re-search-backward ")" nil t)
              (newline-and-indent)
              (insert (format ":files '%S" (remove pkg-file files)))
              (save-buffer))
            (kill-buffer))
          (dolist (file files)
            (rename-file (expand-file-name file pkg-dir) target-dir))
          (delete-directory pkg-dir)
          (setf (package-desc-dir (car (alist-get (package-desc-name pkg-desc)
                                                  package-alist)))
                target-dir)
          (setq pkg-dir target-dir)))
      pkg-dir))

  (define-advice package-load-all-descriptors (:after () ADV)
    (when-let* ((pkg-dir (package-user-all-dir))
                ((file-directory-p pkg-dir)))
      (dolist (pkg-file (directory-files pkg-dir t ".*-pkg.el"))
        (cl-letf (((symbol-function 'package--description-file)
                   (lambda (_) pkg-file)))
          (package-load-descriptor pkg-dir)))))

  (define-advice package-delete (:around (ofun pkg-desc &optional force nosave) ADV)
    (if (string-prefix-p (package-user-all-dir)
                         (package-desc-dir pkg-desc))
        (cl-letf* ((default-directory (package-user-all-dir))
                   (pkg-file (format "%s-pkg.el" (package-desc-name pkg-desc)))
                   ((symbol-function 'package--delete-directory)
                    (lambda (x)
                      (with-current-buffer (find-file-noselect pkg-file)
                        (goto-char (point-min))
                        (when (re-search-forward ":files '\\((.*)\\)" nil t)
                          (dolist (file (read (match-string 1)))
                            (delete-file file))))
                      (delete-file pkg-file))))
          (funcall ofun pkg-desc force nosave))
      (funcall ofun pkg-desc force nosave))))

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; More options
(setq package-install-upgrade-built-in t)

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

;; Required by `use-package'
(use-package diminish :ensure t)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; Update packages
(unless (fboundp 'package-upgrade-all)
  (use-package auto-package-update
    :init
    (setq auto-package-update-delete-old-versions t
          auto-package-update-hide-results t)
    (defalias 'package-upgrade-all #'auto-package-update-now)))

(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
