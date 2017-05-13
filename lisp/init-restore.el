;; init-restore.el --- Initialize restore configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 2.2.0
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

;; Save and restore status
(use-package desktop
  :ensure nil
  :init (desktop-save-mode 1)
  :config
  ;; Don't save/restore frame in tty
  (unless (display-graphic-p)
    (setq desktop-restore-frames nil)))

;; Persistent the scratch buffter
(use-package persistent-scratch
  :diminish persistent-scratch-autosave-mode
  :init (add-hook 'emacs-startup-hook 'persistent-scratch-setup-default)
  :config (setq persistent-scratch-backup-directory
                (expand-file-name "persistent-scratch" user-emacs-directory)))

(provide 'init-restore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-restore.el ends here
