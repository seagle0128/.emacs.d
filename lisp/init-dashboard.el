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
    :defines (persp-save-dir persp-special-last-buffer)
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
                                      (t ">"))
          dashboard-items '((recents  . 10)
                            (bookmarks . 5)
                            (projects . 5)))

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

    (defun dashboard-center-line-1 (&optional real-width)
      "When point is at the end of a line, center it.
    REAL-WIDTH: the real width of the line.  If the line contains an image, the size
                of that image will be considered to be 1 by the calculation method
                used in this function.  As a consequence, the caller must calculate
                himself the correct length of the line taking into account the
                images he inserted in it."
      (let* ((width (or real-width (current-column)))
             (margin (max 0 (floor (/ (- dashboard-banner-length width) 2)))))
        (beginning-of-line)
        (insert (make-string margin ?\s))
        (end-of-line)))

    ;; Navigator
    (defun dashboard-insert-navigator ()
      "Insert navigator buttions below the banner."
      (interactive)
      (with-current-buffer (get-buffer dashboard-buffer-name)
        (let ((inhibit-read-only t)
              (prefix (propertize "[" 'face '(:inherit (font-lock-keyword-face bold))))
              (suffix (propertize "]" 'face '(:inherit (font-lock-keyword-face bold)))))
          (goto-char (point-min))
          (search-forward dashboard-banner-logo-title nil t)

          (insert "\n\n\n")
          (widget-create 'url-link
                         :tag (concat
                               (if (display-graphic-p)
                                   (concat
                                    (all-the-icons-octicon "mark-github"
                                                           :height 1.1
                                                           :v-adjust 0.0
                                                           :face 'font-lock-keyword-face)
                                    (propertize " " 'face 'variable-pitch)))
                               (propertize "Homepage" 'face 'font-lock-keyword-face))
                         :help-echo "Browse homepage"
                         :mouse-face 'highlight
                         :button-prefix prefix
                         :button-suffix suffix
                         :format "%[%t%]"
                         centaur-homepage)
          (insert " ")
          (widget-create 'push-button
                         :tag (concat
                               (if (display-graphic-p)
                                   (concat
                                    (all-the-icons-material "restore"
                                                            :height 1.35
                                                            :v-adjust -0.24
                                                            :face 'font-lock-keyword-face)
                                    (propertize " " 'face 'variable-pitch)))
                               (propertize "Session" 'face 'font-lock-keyword-face))
                         :help-echo "Restore previous session"
                         :action (lambda (&rest _) (restore-session))
                         :mouse-face 'highlight
                         :button-prefix prefix
                         :button-suffix suffix
                         :format "%[%t%]")
          (insert " ")
          (widget-create 'file-link
                         :tag (concat
                               (if (display-graphic-p)
                                   (concat
                                    (all-the-icons-octicon "tools"
                                                           :height 1.0
                                                           :v-adjust 0.0
                                                           :face 'font-lock-keyword-face)
                                    (propertize " " 'face 'variable-pitch)))
                               (propertize "Settings" 'face 'font-lock-keyword-face))
                         :help-echo "Open custom file"
                         :mouse-face 'highlight
                         :button-prefix prefix
                         :button-suffix suffix
                         :format "%[%t%]"
                         custom-file)
          (insert " ")
          (widget-create 'push-button
                         :tag (concat
                               (if (display-graphic-p)
                                   (concat
                                    (all-the-icons-material "update"
                                                            :height 1.35
                                                            :v-adjust -0.24
                                                            :face 'font-lock-keyword-face)
                                    (propertize " " 'face 'variable-pitch)))
                               (propertize "Update" 'face 'font-lock-keyword-face))
                         :help-echo "Update Centaur Emacs"
                         :action (lambda (&rest _) (centaur-update))
                         :mouse-face 'highlight
                         :button-prefix prefix
                         :button-suffix suffix
                         :format "%[%t%]")
          (insert " ")
          (widget-create 'push-button
                         :tag (concat
                               (if (display-graphic-p)
                                   (all-the-icons-faicon "question"
                                                         :height 1.2
                                                         :v-adjust -0.1
                                                         :face 'font-lock-string-face)
                                 (propertize "?" 'face 'font-lock-string-face)))
                         :help-echo "Help (?/h)"
                         :action (lambda (&rest _) (hydra-dashboard/body))
                         :mouse-face 'highlight
                         :button-prefix prefix
                         :button-suffix suffix
                         :format "%[%t%]")
          (dashboard-center-line-1)
          (insert "\n"))))
    (add-hook 'dashboard-mode-hook #'dashboard-insert-navigator)

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
