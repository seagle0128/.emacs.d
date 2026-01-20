;;; init-workspace.el --- Initialize workspace configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2026 Vincent Zhang

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
;; Workspace configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package tabspaces
  :diminish
  :commands tabspaces-mode
  :hook ((after-init . (lambda()
                         ;; Don't enable in bashboard
                         (unless centaur-dashboard
                           (tabspaces-mode 1))
                         (tab-bar-history-mode 1))))
  :custom
  (tab-bar-show nil)

  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  (tabspaces-exclude-buffers '("*eat*" "*vterm*" "*shell*" "*eshell*"))
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore (not centaur-dashboard))
  (tabspaces-session-file (concat user-emacs-directory "tabspaces/tabsession.el"))
  :config
  (with-no-warnings
    ;; Filter Buffers for Consult-Buffer
    (with-eval-after-load 'consult
      ;; hide full buffer list (still available with "b" prefix)
      (consult-customize consult-source-buffer :hidden t :default nil)
      ;; set consult-workspace buffer list
      (defvar consult-source-workspace
        (list :name     "Workspace Buffer"
              :narrow   ?w
              :history  'buffer-name-history
              :category 'buffer
              :state    #'consult--buffer-state
              :default  t
              :items    (lambda () (consult--buffer-query
                               :predicate #'tabspaces--local-buffer-p
                               :sort 'visibility
                               :as #'buffer-name)))
        "Set workspace buffer list for consult-buffer.")
      (add-to-list 'consult-buffer-sources 'consult-source-workspace))

    (defun tabspaces--delete-old-files (dir days)
      "Delete backup files of DIR, with timestamp suffix older than DAYS days."
      (let ((cutoff (time-subtract (current-time)
                                   (seconds-to-time (* days 24 60 60)))))
        (dolist (file (directory-files dir 'full "\\.[0-9]\\{8\\}\\'"))
          (when-let ((timestamp-str (substring file (string-match "\\([0-9]\\{8\\}\\)\\'" file))))
            (when (time-less-p (date-to-time timestamp-str) cutoff)
              (delete-file file 'trash))))))

    (defun tabspaces--prepare-save-session (&rest _)
      "Prepare for saving session."
      ;; Backup session
      (when tabspaces-session
        (let ((dir (expand-file-name "tabspaces" user-emacs-directory)))
          (unless (file-exists-p dir)
            (mkdir dir))
          ;; Delete the sessions that are older than 7 days
          (tabspaces--delete-old-files dir 7))

        (when (file-exists-p tabspaces-session-file)
          (copy-file tabspaces-session-file
                     (format "%s.%s" tabspaces-session-file (format-time-string "%Y%m%d"))
                     t)))

      ;; Cleanup
      (and (fboundp 'helpful-kill-buffers)
           (helpful-kill-buffers))

      (and (fboundp 'magit-mode-get-buffers)
           (mapc #'kill-buffer (magit-mode-get-buffers)))

      (and (fboundp 'posframe-delete-all)
           (posframe-delete-all)))
    (advice-add #'tabspaces--save-session-smart :before #'tabspaces--prepare-save-session)

    (defun tabspaces--bury-messages (&rest _)
      "Bury *Messages* buffer."
      (quit-windows-on messages-buffer-name))
    (advice-add #'tabspaces-restore-session :after #'tabspaces--bury-messages)))

(provide 'init-workspace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-workspace.el ends here
