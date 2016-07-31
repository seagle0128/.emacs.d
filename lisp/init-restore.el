;; init-restore.el --- Initialize restore configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 2.0.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Restore configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(desktop-save-mode 1)

;; Perspectives
(use-package persp-mode
  :defer t
  :defines desktop-files-not-to-save
  :init
  (progn
    (add-hook 'after-init-hook 'persp-mode)
    (setq persp-nil-name "main")               ; Do not use "none"
    (setq persp-keymap-prefix (kbd "C-c C-p")) ; Avoid conflict with projectile
    (setq desktop-files-not-to-save "")        ; Do not save buffers via desktop
    ))

(use-package persistent-scratch
  :defer t
  :init (add-hook 'desktop-after-read-hook 'persistent-scratch-setup-default))

(provide 'init-restore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-restore.el ends here
