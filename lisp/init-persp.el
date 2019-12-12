;;; init-persp.el --- Initialize perspectives configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Vincent Zhang

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
;; perspectives configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; Windows/buffers sets shared among frames + save/load.
(use-package persp-mode
  :diminish
  :defines (recentf-exclude ivy-ignore-buffers ivy-sort-functions-alist)
  :commands (get-current-persp persp-contain-buffer-p)
  :hook ((after-init . persp-mode)
         (window-setup . toggle-frame-maximized))
  :init (setq persp-keymap-prefix (kbd "C-x p")
              persp-nil-name "default"
              persp-set-last-persp-for-new-frames nil
              persp-kill-foreign-buffer-behaviour 'kill
              persp-auto-resume-time (if centaur-dashboard 0 1.0)
              persp-common-buffer-filter-functions
              (list #'(lambda (b)
                        "Ignore temporary buffers."
                        (let ((bname (file-name-nondirectory (buffer-name b))))
                          (or (string-prefix-p " " bname)
                              (and (string-prefix-p "*" bname)
                                   (not (string-equal "*scratch*" bname)))
                              (string-suffix-p ".elc" bname)
                              (string-suffix-p ".gz" bname)
                              (string-suffix-p ".zip" bname)
                              (string-prefix-p "magit" bname)
                              (string-prefix-p "Pfuture-Callback" bname)
                              (string-match-p ".elfeed" bname)
                              (eq (buffer-local-value 'major-mode b) 'erc-mode)
                              (eq (buffer-local-value 'major-mode b) 'rcirc-mode)
                              (eq (buffer-local-value 'major-mode b) 'nov-mode)
                              (eq (buffer-local-value 'major-mode b) 'vterm-mode))))))
  :config
  ;; Don't save persp configs in `recentf'
  (push persp-save-dir recentf-exclude)

  ;; Ivy Integraticon
  (with-eval-after-load 'ivy
    (add-to-list 'ivy-ignore-buffers
                 #'(lambda (b)
                     (when persp-mode
                       (let ((persp (get-current-persp)))
                         (if persp
                             (not (persp-contain-buffer-p b persp))
                           nil)))))))

;; Projectile integration
(use-package persp-mode-projectile-bridge
  :after projectile
  :functions (persp-get-by-name
              persp-add-new
              persp-add-buffer
              set-persp-parameter
              my-persp-mode-projectile-bridge-add-new-persp)
  :commands (persp-mode-projectile-bridge-find-perspectives-for-all-buffers
             persp-mode-projectile-bridge-kill-perspectives
             persp-mode-projectile-bridge-add-new-persp
             projectile-project-buffers)
  :hook ((persp-mode . persp-mode-projectile-bridge-mode)
         (persp-mode-projectile-bridge-mode
          .
          (lambda ()
            (if persp-mode-projectile-bridge-mode
                (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
              (persp-mode-projectile-bridge-kill-perspectives)))))
  :init (setq persp-mode-projectile-bridge-persp-name-prefix "[p]")
  :config
  ;; HACK: Allow saving to files
  (defun my-persp-mode-projectile-bridge-add-new-persp (name)
    (let ((persp (persp-get-by-name name *persp-hash* :nil)))
      (if (eq :nil persp)
          (prog1
              (setq persp (persp-add-new name))
            (when persp
              (set-persp-parameter 'persp-mode-projectile-bridge t persp)
              (persp-add-buffer (projectile-project-buffers)
                                persp nil nil)))
        persp)))
  (advice-add #'persp-mode-projectile-bridge-add-new-persp
              :override #'my-persp-mode-projectile-bridge-add-new-persp))

(provide 'init-persp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-persp.el ends here
