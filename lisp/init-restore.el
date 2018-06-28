;; init-restore.el --- Initialize restoring configurations.	-*- lexical-binding: t -*-

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
;; Restoring configurations.
;;

;;; Code:

;; Save and restore status
(use-package desktop
  :ensure nil
  :init (desktop-save-mode 1)
  :config
  ;; Restore frames into their original displays (if possible)
  (setq desktop-restore-in-current-display nil)

  (if (display-graphic-p)
      ;; Prevent desktop from holding onto theme elements
      (add-hook 'desktop-after-read-hook
                (lambda ()
                  "Load custom theme."
                  (dolist (theme custom-enabled-themes)
                    (load-theme theme t))))
    ;; Don't save/restore frames in TTY
    (setq desktop-restore-frames nil)))

;; Persistent the scratch buffter
(use-package persistent-scratch
  :hook (after-init . persistent-scratch-setup-default))

(provide 'init-restore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-restore.el ends here
