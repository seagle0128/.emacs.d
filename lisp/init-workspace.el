;;; init-workspace.el --- Initialize workspace configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2025 Vincent Zhang

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
  :functions tabspaces-mode
  :hook (after-init . (lambda() (unless centaur-dashboard (tabspaces-mode t))))
  :custom
  (tab-bar-show nil)

  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore (not centaur-dashboard))
  :config
  (with-no-warnings
    ;; Filter Buffers for Consult-Buffer
    (with-eval-after-load 'consult
      ;; hide full buffer list (still available with "b" prefix)
      (consult-customize consult--source-buffer :hidden t :default nil)
      ;; set consult-workspace buffer list
      (defvar consult--source-workspace
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
      (add-to-list 'consult-buffer-sources 'consult--source-workspace))

    (defun my-tabspaces-hide-childframes (&rest _)
      "Hide all child frames."
      (when (childframe-workable-p)
        ;; Hide shell pop frames
        (when (and (frame-live-p shell-pop--frame)
                   (frame-visible-p shell-pop--frame))
          ;; Hide child frame and refocus in parent frame
          (make-frame-invisible shell-pop--frame)
          (select-frame-set-input-focus (frame-parent shell-pop--frame))
          (setq shell-pop--frame nil))
        ;; Hide posframe
        (posframe-hide-all)))
    (advice-add #'tabspaces-save-session :before #'my-tabspaces-hide-childframes)

    (defun my-tabspaces-burry-window (&rest _)
      "Burry *Messages* buffer."
      (quit-windows-on messages-buffer-name))
    (advice-add #'tabspaces-restore-session :after #'my-tabspaces-burry-window)))

(provide 'init-workspace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-workspace.el ends here
