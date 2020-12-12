;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Vincent Zhang

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
;; Dashboard configurations.
;;

;;; Code:

(require 'init-const)
(require 'init-custom)

;; Dashboard
(unless emacs/>=25.3p (setq centaur-dashboard nil))
(when centaur-dashboard
  (use-package dashboard
    :diminish (dashboard-mode page-break-lines-mode)
    :functions (all-the-icons-faicon
                all-the-icons-material
                winner-undo
                widget-forward)
    :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
    :pretty-hydra
    ((:title (pretty-hydra-title "Dashboard" 'material "dashboard" :height 1.2 :v-adjust -0.2)
      :color pink :quit-key "q")
     ("Navigator"
      (("U" update-config-and-packages "update" :exit t)
       ("H" browse-homepage "homepage" :exit t)
       ("R" restore-previous-session "recover session" :exit t)
       ("L" restore-session "list sessions" :exit t)
       ("S" open-custom-file "settings" :exit t))
      "Section"
      (("}" dashboard-next-section "next")
       ("{" dashboard-previous-section "previous")
       ("r" dashboard-goto-recent-files "recent files")
       ("m" dashboard-goto-bookmarks "bookmarks")
       ("p" dashboard-goto-projects "projects"))
      "Item"
      (("RET" widget-button-press "open" :exit t)
       ("<tab>" widget-forward "next")
       ("C-i" widget-forward "next")
       ("<backtab>" widget-backward "previous")
       ("C-n" next-line "next line")
       ("C-p" previous-line "previous  line"))
      "Misc"
      (("<f2>" open-dashboard "open" :exit t)
       ("g" dashboard-refresh-buffer "refresh" :exit t)
       ("Q" quit-dashboard "quit" :exit t))))
    :bind (("<f2>" . open-dashboard)
           :map dashboard-mode-map
           ("H" . browse-homepage)
           ("R" . restore-previous-session)
           ("L" . restore-session)
           ("S" . open-custom-file)
           ("U" . update-config-and-packages)
           ("q" . quit-dashboard)
           ("h" . dashboard-hydra/body)
           ("?" . dashboard-hydra/body))
    :hook (dashboard-mode . (lambda () (setq-local frame-title-format "")))
    :init
    (setq dashboard-banner-logo-title "CENTAUR EMACS - Enjoy Programming & Writing"
          dashboard-startup-banner (or centaur-logo 'official)
          dashboard-center-content t
          dashboard-show-shortcuts nil
          dashboard-items '((recents  . 10)
                            (bookmarks . 5)
                            (projects . 5))

          dashboard-set-init-info t
          dashboard-set-file-icons centaur-icon
          dashboard-set-heading-icons centaur-icon
          dashboard-heading-icons '((recents   . "file-text")
                                    (bookmarks . "bookmark")
                                    (agenda    . "calendar")
                                    (projects  . "briefcase")
                                    (registers . "database"))

          dashboard-set-footer t
          dashboard-footer (format "Powered by Vincent Zhang, %s" (format-time-string "%Y"))
          dashboard-footer-icon (cond ((icons-displayable-p)
                                       (all-the-icons-faicon "heart"
                                                             :height 1.1
                                                             :v-adjust -0.05
                                                             :face 'error))
                                      ((char-displayable-p ?🧡) "🧡 ")
                                      (t (propertize ">" 'face 'dashboard-footer)))

          dashboard-set-navigator t
          dashboard-navigator-buttons
          `(((,(when (icons-displayable-p)
                 (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
              "Homepage" "Browse homepage"
              (lambda (&rest _) (browse-url centaur-homepage)))
             (,(when (icons-displayable-p)
                 (all-the-icons-material "restore" :height 1.35 :v-adjust -0.24))
              "Restore" "Restore previous session"
              (lambda (&rest _) (restore-previous-session)))
             (,(when (icons-displayable-p)
                 (all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0))
              "Settings" "Open custom file"
              (lambda (&rest _) (find-file custom-file)))
             (,(when (icons-displayable-p)
                 (all-the-icons-material "update" :height 1.35 :v-adjust -0.24))
              "Update" "Update Centaur Emacs"
              (lambda (&rest _) (centaur-update)))
             (,(if (icons-displayable-p)
                   (all-the-icons-faicon "question" :height 1.2 :v-adjust -0.1)
                 "?")
              "" "Help (?/h)"
              (lambda (&rest _) (dashboard-hydra/body))
              font-lock-string-face))))

    (dashboard-setup-startup-hook)
    :config
    ;; WORKAROUND: fix differnct background color of the banner image.
    ;; @see https://github.com/emacs-dashboard/emacs-dashboard/issues/203
    (defun my-dashboard-insert-image-banner (banner)
      "Display an image BANNER."
      (when (file-exists-p banner)
        (let* ((title dashboard-banner-logo-title)
               (spec (create-image banner))
               (size (image-size spec))
               (width (car size))
               (left-margin (max 0 (floor (- dashboard-banner-length width) 2))))
          (goto-char (point-min))
          (insert "\n")
          (insert (make-string left-margin ?\ ))
          (insert-image spec)
          (insert "\n\n")
          (when title
            (dashboard-center-line title)
            (insert (format "%s\n\n" (propertize title 'face 'dashboard-banner-logo-title)))))))
    (advice-add #'dashboard-insert-image-banner :override #'my-dashboard-insert-image-banner)

    ;; FIXME: Insert copyright
    ;; @see https://github.com/emacs-dashboard/emacs-dashboard/issues/219
    (defun my-dashboard-insert-copyright ()
      "Insert copyright in the footer."
      (when dashboard-footer
        (insert "\n  ")
        (dashboard-center-line dashboard-footer)
        (insert (propertize dashboard-footer 'face 'font-lock-comment-face))
        (insert "\n")))
    (advice-add #'dashboard-insert-footer :after #'my-dashboard-insert-copyright)

    (defvar dashboard-recover-layout-p nil
      "Wether recovers the layout.")

    (defun restore-previous-session ()
      "Restore the previous session."
      (interactive)
      (when (bound-and-true-p persp-mode)
        (restore-session persp-auto-save-fname)))

    (defun restore-session (fname)
      "Restore the specified session."
      (interactive (list (read-file-name "Load perspectives from a file: "
                                         persp-save-dir)))
      (when (bound-and-true-p persp-mode)
        (message "Restoring session...")
        (quit-window t)
        (condition-case-unless-debug err
            (persp-load-state-from-file fname)
          (error "Error: Unable to restore session -- %s" err))
        (message "Restoring session...done")))

    (defun dashboard-goto-recent-files ()
      "Go to recent files."
      (interactive)
      (let ((func (local-key-binding "r")))
        (and func (funcall func))))

    (defun dashboard-goto-projects ()
      "Go to projects."
      (interactive)
      (let ((func (local-key-binding "p")))
        (and func (funcall func))))

    (defun dashboard-goto-bookmarks ()
      "Go to bookmarks."
      (interactive)
      (let ((func (local-key-binding "m")))
        (and func (funcall func))))

    (defun open-dashboard ()
      "Open the *dashboard* buffer and jump to the first widget."
      (interactive)
      ;; Check if need to recover layout
      (if (> (length (window-list-1))
             ;; exclude `treemacs' window
             (if (and (fboundp 'treemacs-current-visibility)
                      (eq (treemacs-current-visibility) 'visible))
                 2
               1))
          (setq dashboard-recover-layout-p t))

      (delete-other-windows)

      ;; Refresh dashboard buffer
      (when (get-buffer dashboard-buffer-name)
        (kill-buffer dashboard-buffer-name))
      (dashboard-insert-startupify-lists)
      (switch-to-buffer dashboard-buffer-name)

      ;; Jump to the first section
      (dashboard-goto-recent-files))

    (defun quit-dashboard ()
      "Quit dashboard window."
      (interactive)
      (quit-window t)
      (when (and dashboard-recover-layout-p
                 (bound-and-true-p winner-mode))
        (winner-undo)
        (setq dashboard-recover-layout-p nil)))))

(provide 'init-dashboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
