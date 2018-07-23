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

(eval-when-compile
  (require 'init-custom))

(if centaur-dashboard
    ;; Dashboard
    (use-package dashboard
      :diminish dashboard-mode
      :bind (("<f2>" . (lambda ()
                         "Open the *dashboard* buffer."
                         (interactive)
                         (dashboard-insert-startupify-lists)
                         (switch-to-buffer dashboard-buffer-name))))
      :hook ((after-init . dashboard-setup-startup-hook)
             (emacs-startup . toggle-frame-maximized))
      :config
      (setq dashboard-banner-logo-title "Welcome to Centaur Emacs")
      (setq dashboard-startup-banner (if centaur-logo centaur-logo 'official))
      (setq dashboard-items '((recents  . 10)
                              (bookmarks . 5)
                              (projects . 5)))

      (defun dashboard-insert-buttons (list-size)
        (insert "\n")
        (insert (make-string (max 0 (floor (/ (- dashboard-banner-length 51) 2))) ?\ ))
        (widget-create 'url-link
                       :tag (propertize "Homepage" 'face 'font-lock-keyword-face)
                       :help-echo "Open the Centaur Emacs Github page."
                       :mouse-face 'highlight
                       :follow-link "\C-m"
                       "https://github.com/seagle0128/.emacs.d")
        (insert " ")
        (widget-create 'push-button
                       :help-echo "Recover Desktop."
                       :action `(lambda (&rest ignore) (desktop-read))
                       :mouse-face 'highlight
                       :follow-link "\C-m"
                       :button-prefix ""
                       :button-suffix ""
                       (propertize "Recover Desktop" 'face 'font-lock-keyword-face))
        (insert " ")
        (widget-create 'push-button
                       :help-echo "Edit Configurations."
                       :action `(lambda (&rest ignore) (open-custom-file))
                       :mouse-face 'highlight
                       :follow-link "\C-m"
                       :button-prefix ""
                       :button-suffix ""
                       (propertize "Edit Config" 'face 'font-lock-keyword-face))
        (insert " ")
        (widget-create 'push-button
                       :help-echo "Update Centaur Emacs."
                       :action (lambda (&rest ignore) (update-config) (upgrade-packages))
                       :mouse-face 'highlight
                       :follow-link "\C-m"
                       (propertize "Update" 'face 'font-lock-keyword-face))
        (insert "\n"))

      (add-to-list 'dashboard-item-generators  '(buttons . dashboard-insert-buttons))
      (add-to-list 'dashboard-items '(buttons)))

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
      (setq desktop-restore-frames nil))))

;; Persistent the scratch buffter
(use-package persistent-scratch
  :hook (after-init . persistent-scratch-setup-default))

(provide 'init-restore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-restore.el ends here
