;;; init-persp.el --- Initialize perspectives configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Vincent Zhang

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
  :defines ivy-sort-functions-alist
  :commands (get-current-persp persp-contain-buffer-p)
  :hook (after-init . (lambda ()
                        (unless centaur-dashboard
                          (toggle-frame-maximized)
                          (persp-mode 1))))
  :init
  (setq persp-keymap-prefix (kbd "C-x p"))
  (setq persp-nil-name "main")
  :config
  ;; NOTE: Redefine `persp-add-new' to address.
  ;; Issue: Unable to create/handle persp-mode
  ;; https://github.com/Bad-ptr/persp-mode.el/issues/96
  ;; https://github.com/Bad-ptr/persp-mode-projectile-bridge.el/issues/4
  ;; https://emacs-china.org/t/topic/6416/7
  (defun* persp-add-new (name &optional (phash *persp-hash*))
    "Create a new perspective with the given `NAME'. Add it to `PHASH'.
  Return the created perspective."
    (interactive "sA name for the new perspective: ")
    (if (and name (not (equal "" name)))
        (destructuring-bind (e . p)
            (persp-by-name-and-exists name phash)
          (if e p
            (setq p (if (equal persp-nil-name name)
                        nil (make-persp :name name)))
            (persp-add p phash)
            (run-hook-with-args 'persp-created-functions p phash)
            p))
      (message "[persp-mode] Error: Can't create a perspective with empty name.")
      nil))

  ;; Display on mode-line
  (add-to-list 'global-mode-string
               '(:eval
                 (format
                  (propertize
                   " #%s "
                   'face (let ((persp (get-current-persp)))
                           (if persp
                               (if (persp-contain-buffer-p (current-buffer) persp)
                                   'persp-face-lighter-default
                                 'persp-face-lighter-buffer-not-in-persp)
                             'persp-face-lighter-nil-persp)))
                  (safe-persp-name (get-current-persp)))))

  ;; Integrate `ivy'
  (with-eval-after-load "ivy"
    (add-hook 'ivy-ignore-buffers
              #'(lambda (b)
                  (when persp-mode
                    (let ((persp (get-current-persp)))
                      (if persp
                          (not (persp-contain-buffer-p b persp))
                        nil)))))

    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch  . nil))))))

;; Projectile integration
(use-package persp-mode-projectile-bridge
  :after projectile persp-mode
  :commands (persp-mode-projectile-bridge-find-perspectives-for-all-buffers
             persp-mode-projectile-bridge-kill-perspectives)
  :hook ((persp-mode . persp-mode-projectile-bridge-mode)
         (persp-mode-projectile-bridge-mode
          . (lambda ()
              (if persp-mode-projectile-bridge-mode
                  (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
                (persp-mode-projectile-bridge-kill-perspectives))))))

(provide 'init-persp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-persp.el ends here
