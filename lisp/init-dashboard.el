;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-

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
;; Dashboard configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; Dashboard
(when centaur-dashboard
  (use-package dashboard
    :diminish
    :autoload dashboard-setup-startup-hook
    :functions icons-displayable-p nerd-icons-mdicon nerd-icons-octicon
    :custom-face
    (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
    (dashboard-items-face ((t (:weight normal))))
    (dashboard-no-items-face ((t (:weight normal))))
    :bind (("<f2>" . open-dashboard)
           :map dashboard-mode-map
           ("H" . browse-homepage)
           ("R" . restore-session)
           ("S" . find-custom-file)
           ("U" . update-config-and-packages)
           ("q" . quit-dashboard))
    :hook (dashboard-mode . (lambda () (setq-local frame-title-format nil)))
    :init
    (setq dashboard-banner-logo-title "CENTAUR EMACS - Enjoy Programming & Writing"
          dashboard-startup-banner (or centaur-logo 'official)
          dashboard-page-separator "\n\f\n"
          dashboard-projects-backend 'project-el
          dashboard-path-style 'truncate-middle
          dashboard-path-max-length 60
          dashboard-center-content t
          dashboard-vertically-center-content t
          dashboard-show-shortcuts nil
          dashboard-items '((recents  . 10)
                            (bookmarks . 5)
                            (projects . 5))

          dashboard-startupify-list '(dashboard-insert-banner
                                      dashboard-insert-newline
                                      dashboard-insert-banner-title
                                      dashboard-insert-newline
                                      dashboard-insert-navigator
                                      dashboard-insert-newline
                                      dashboard-insert-init-info
                                      dashboard-insert-items
                                      dashboard-insert-newline
                                      dashboard-insert-footer)

          dashboard-display-icons-p #'icons-displayable-p
          dashboard-set-file-icons centaur-icon
          dashboard-set-heading-icons centaur-icon
          dashboard-heading-icons '((recents   . "nf-oct-history")
                                    (bookmarks . "nf-oct-bookmark")
                                    (agenda    . "nf-oct-calendar")
                                    (projects  . "nf-oct-briefcase")
                                    (registers . "nf-oct-database"))

          dashboard-navigator-buttons
          `(((,(when (icons-displayable-p)
                 (nerd-icons-mdicon "nf-md-github" :height 1.4))
              "Homepage" "Browse homepage"
              (lambda (&rest _) (browse-url centaur-homepage)))
             (,(when (icons-displayable-p)
                 (nerd-icons-mdicon "nf-md-backup_restore" :height 1.5))
              "Restore" "Restore previous session"
              (lambda (&rest _) (restore-session)))
             (,(when (icons-displayable-p)
                 (nerd-icons-mdicon "nf-md-tools" :height 1.3))
              "Settings" "Open custom file"
              (lambda (&rest _) (find-file custom-file)))
             (,(when (icons-displayable-p)
                 (nerd-icons-mdicon "nf-md-update" :height 1.3))
              "Update" "Update Centaur Emacs"
              (lambda (&rest _) (centaur-update)))
             (,(if (icons-displayable-p)
                   (nerd-icons-mdicon "nf-md-help" :height 1.2)
                 "?")
              "" "Help (?/h)"
              (lambda (&rest _) (dashboard-hydra/body)))))

          dashboard-footer-icon
          (if (icons-displayable-p)
              (nerd-icons-octicon "nf-oct-heart" :height 1.2 :face 'nerd-icons-lred)
            (propertize ">" 'face 'dashboard-footer)))

    (dashboard-setup-startup-hook)
    :config
    (with-no-warnings
      ;; Insert copyright
      ;; @see https://github.com/emacs-dashboard/emacs-dashboard/issues/219
      (defun my-dashboard-insert-copyright ()
        "Insert copyright in the footer."
        (dashboard-insert-center
         (propertize (format "\nPowered by Vincent Zhang, %s\n" (format-time-string "%Y"))
                     'face 'font-lock-comment-face)))
      (advice-add #'dashboard-insert-footer :after #'my-dashboard-insert-copyright)

      (defun restore-session ()
        "Restore the previous session."
        (interactive)
        (message "Restoring previous session...")
        (quit-window t)

        (when (fboundp 'tabspaces-mode)
          (unless tabspaces-mode
            (tabspaces-mode t))
          (tabspaces-restore-session)
          (tabspaces-switch-or-create-workspace tabspaces-default-tab))

        (message "Restoring previous session...done"))

      (defvar dashboard-recover-layout-p nil
        "Wether recovers the layout.")

      (defun open-dashboard ()
        "Open the *dashboard* buffer and jump to the first widget."
        (interactive)
        ;; Check if need to recover layout
        (if (length> (window-list-1)
                     ;; exclude `treemacs' window
                     (if (and (fboundp 'treemacs-current-visibility)
                              (eq (treemacs-current-visibility) 'visible))
                         2
                       1))
            (setq dashboard-recover-layout-p t))

        ;; Display dashboard in maximized window
        (delete-other-windows)

        ;; Refresh dashboard buffer
        (dashboard-refresh-buffer)

        ;; Jump to the first section
        (dashboard-goto-recent-files))

      (defun quit-dashboard ()
        "Quit dashboard window."
        (interactive)
        (quit-window t)

        ;; Create workspace
        (when (fboundp 'tabspaces-mode)
          (unless tabspaces-mode
            (tabspaces-mode t)
            (tabspaces-switch-or-create-workspace tabspaces-default-tab)))

        ;; Recover layout
        (when dashboard-recover-layout-p
          (cond
           ((bound-and-true-p tab-bar-history-mode)
            (tab-bar-history-back))
           ((bound-and-true-p winner-mode)
            (winner-undo)))
          (setq dashboard-recover-layout-p nil))))))

(provide 'init-dashboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
