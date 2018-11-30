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
  :hook (after-init . (lambda ()
                        (unless centaur-dashboard
                          (toggle-frame-maximized)
                          (persp-mode 1))))
  :init
  (setq persp-keymap-prefix (kbd "C-x p"))
  (setq persp-nil-name "main")
  :config (add-to-list 'global-mode-string persp-lighter))

;; Projectile integration
(use-package persp-mode-projectile-bridge
  :commands (persp-mode-projectile-bridge-find-perspectives-for-all-buffers
             persp-mode-projectile-bridge-kill-perspectives)
  :hook ((after-init . persp-mode-projectile-bridge-mode)
         (persp-mode-projectile-bridge-mode
          . (lambda ()
              (if persp-mode-projectile-bridge-mode
                  (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
                (persp-mode-projectile-bridge-kill-perspectives))))))

(provide 'init-persp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-persp.el ends here
