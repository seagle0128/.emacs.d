;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Vincent Zhang

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

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Dashboard
(when centaur-dashboard
  (use-package dashboard
    :diminish (dashboard-mode page-break-lines-mode)
    :defines persp-special-last-buffer
    :functions (all-the-icons-faicon
                all-the-icons-material
                open-custom-file
                persp-get-buffer-or-null
                persp-load-state-from-file
                persp-switch-to-buffer
                winner-undo
                widget-forward)
    :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
    :bind (("<f2>" . open-dashboard)
           :map dashboard-mode-map
           ("H" . browse-homepage)
           ("R" . restore-session)
           ("L" . persp-load-state-from-file)
           ("S" . open-custom-file)
           ("U" . update-config-and-packages)
           ("q" . quit-dashboard))
    :hook (dashboard-mode . (lambda () (setq-local frame-title-format "")))
    :init (dashboard-setup-startup-hook)
    :config
    (setq dashboard-banner-logo-title "CENTAUR EMACS - Enjoy Programming & Writing"
          dashboard-startup-banner (or centaur-logo 'official)
          dashboard-center-content t
          dashboard-show-shortcuts nil
          dashboard-items '((recents  . 10)
                            (bookmarks . 5)
                            (projects . 5))

          dashboard-set-init-info t
          dashboard-set-file-icons t
          dashboard-set-heading-icons t
          dashboard-heading-icons '((recents   . "file-text")
                                    (bookmarks . "bookmark")
                                    (agenda    . "calendar")
                                    (projects  . "file-directory")
                                    (registers . "database"))

          dashboard-set-footer t
          dashboard-footer (format "Powered by Vincent Zhang, %s" (format-time-string "%Y"))
          dashboard-footer-icon (cond ((display-graphic-p)
                                       (all-the-icons-faicon "heart"
                                                             :height 1.1
                                                             :v-adjust -0.05
                                                             :face 'error))
                                      ((char-displayable-p ?🧡) "🧡 ")
                                      (t (propertize ">" 'face 'font-lock-doc-face)))

          dashboard-set-navigator t
          dashboard-navigator-buttons
          `(((,(when (display-graphic-p)
                 (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
              "Homepage" "Browse homepage"
              (lambda (&rest _) (browse-url centaur-homepage)))
             (,(when (display-graphic-p)
                 (all-the-icons-material "restore" :height 1.35 :v-adjust -0.24))
              "Restore" "Restore previous session"
              (lambda (&rest _) (restore-session)))
             (,(when (display-graphic-p)
                 (all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0))
              "Settings" "Open custom file"
              (lambda (&rest _) (find-file custom-file)))
             (,(when (display-graphic-p)
                 (all-the-icons-material "update" :height 1.35 :v-adjust -0.24))
              "Update" "Update Centaur Emacs"
              (lambda (&rest _) (centaur-update)))
             (,(if (display-graphic-p)
                   (all-the-icons-faicon "question" :height 1.2 :v-adjust -0.1)
                 "?")
              "" "Help (?/h)"
              (lambda (&rest _) (hydra-dashboard/body))
              font-lock-string-face))))

    (defun my-banner-path (&rest _)
      "Return the full path to banner."
      (expand-file-name "banner.txt" user-emacs-directory))
    (advice-add #'dashboard-get-banner-path :override #'my-banner-path)

    (defvar dashboard-recover-layout-p nil
      "Wether recovers the layout.")

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
      (if (get-buffer dashboard-buffer-name)
          (kill-buffer dashboard-buffer-name))
      (dashboard-insert-startupify-lists)
      (switch-to-buffer dashboard-buffer-name)

      ;; Jump to the first section
      (goto-char (point-min))
      (dashboard-goto-recent-files))

    (defun restore-session ()
      "Restore last session."
      (interactive)
      (when (bound-and-true-p persp-mode)
        (message "Restoring session...")
        (condition-case-unless-debug err
            (persp-load-state-from-file)
          (error
           (message "Error: Unable to restore last session -- %s" err)))
        (when (persp-get-buffer-or-null persp-special-last-buffer)
          (persp-switch-to-buffer persp-special-last-buffer))))

    (defun quit-dashboard ()
      "Quit dashboard window."
      (interactive)
      (quit-window t)
      (when (and dashboard-recover-layout-p
                 (bound-and-true-p winner-mode))
        (winner-undo)
        (setq dashboard-recover-layout-p nil)))

    (defun dashboard-goto-recent-files ()
      "Go to recent files."
      (interactive)
      (funcall (local-key-binding "r")))

    (defun dashboard-goto-projects ()
      "Go to projects."
      (interactive)
      (funcall (local-key-binding "p")))

    (defun dashboard-goto-bookmarks ()
      "Go to bookmarks."
      (interactive)
      (funcall (local-key-binding "m")))

    (defhydra hydra-dashboard (:color red :hint none)
      "
^Head^               ^Section^            ^Item^                  ^Dashboard^
^^───────────────────^^───────────────────^^──────────────────────^^───────────────
_U_pdate             _}_: Next            _RET_: Open             _<f2>_: Open
_H_omePage           _{_: Previous        _<tab>_/_C-i_: Next       _Q_: Quit
_R_ecover session    _r_: Recent Files    _<backtab>_: Previous
_L_ist sessions      _m_: Bookmarks       _C-n_: Next line
_S_ettings           _p_: Projects        _C-p_: Previous Line
"
      ("<tab>" widget-forward)
      ("C-i" widget-forward)
      ("<backtab>" widget-backward)
      ("RET" widget-button-press :exit t)
      ("g" dashboard-refresh-buffer :exit t)
      ("}" dashboard-next-section)
      ("{" dashboard-previous-section)
      ("r" dashboard-goto-recent-files)
      ("p" dashboard-goto-projects)
      ("m" dashboard-goto-bookmarks)
      ("H" browse-homepage :exit t)
      ("R" restore-session :exit t)
      ("L" persp-load-state-from-file :exit t)
      ("S" open-custom-file :exit t)
      ("U" update-config-and-packages :exit t)
      ("C-n" next-line)
      ("C-p" previous-line)
      ("<f2>" open-dashboard :exit t)
      ("Q" quit-dashboard :exit t)
      ("q" nil "quit")
      ("C-g" nil "quit"))
    (bind-keys :map dashboard-mode-map
               ("h" . hydra-dashboard/body)
               ("?" . hydra-dashboard/body))))

(provide 'init-dashboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
