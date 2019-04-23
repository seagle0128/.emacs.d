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
           ("U" . centaur-update)
           ("q" . quit-dashboard))
    :hook (dashboard-mode . (lambda () (setq-local frame-title-format "")))
    :init (dashboard-setup-startup-hook)
    :config
    (setq dashboard-banner-logo-title "CENTAUR EMACS - Enjoy Programming & Writing")
    (setq dashboard-startup-banner (or centaur-logo 'official))
    (setq dashboard-center-content t)
    (setq dashboard-show-shortcuts nil)
    (setq dashboard-items '((recents  . 10)
                            (bookmarks . 5)
                            (projects . 5)))

    (defun my-banner-path (&rest _)
      "Return the full ,@restpath to banner."
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

    ;; Add heading icons
    (defun dashboard-insert-heading-icon (heading &optional _shortcut)
      (when (display-graphic-p)
        ;; Load `all-the-icons' if it's unavailable
        (unless (featurep 'all-the-icons)
          (require 'all-the-icons nil t))

        (insert (cond
                 ((string-equal heading "Recent Files:")
                  (all-the-icons-octicon "history" :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))
                 ((string-equal heading "Bookmarks:")
                  (all-the-icons-octicon "bookmark" :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))
                 ((string-equal heading "Projects:")
                  (all-the-icons-octicon "file-directory" :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))))
        (insert " ")))
    (advice-add #'dashboard-insert-heading :before #'dashboard-insert-heading-icon)

    ;; Add file icons
    ;; MUST redefine the sections because of the macro `dashboard-insert-section-list'
    (defmacro dashboard-insert-section-list (section-name list action &rest rest)
      "Insert into SECTION-NAME a LIST of items, expanding ACTION and passing REST to widget creation."
      `(when (car ,list)
         (mapc (lambda (el)
                 (let ((widget nil)
                       (tab-width 1))
                   (insert "\n    ")
                   (when (display-graphic-p)
                     (insert (when-let ((path (car (last (split-string ,@rest " - ")))))
                               (if (file-directory-p path)
                                   (cond
                                    ((and (fboundp 'tramp-tramp-file-p)
                                          (tramp-tramp-file-p default-directory))
                                     (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01))
                                    ((file-symlink-p path)
                                     (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust 0.01))
                                    ((all-the-icons-dir-is-submodule path)
                                     (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust 0.01))
                                    ((file-exists-p (format "%s/.git" path))
                                     (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.01))
                                    (t (let ((matcher (all-the-icons-match-to-alist path all-the-icons-dir-icon-alist)))
                                         (apply (car matcher) (list (cadr matcher) :v-adjust 0.01)))))
                                 (all-the-icons-icon-for-file (file-name-nondirectory path)))))
                     (insert "\t"))
                   (setq widget
                         (widget-create 'push-button
                                        :action ,action
                                        :mouse-face 'highlight
                                        :button-prefix ""
                                        :button-suffix ""
                                        :format "%[%t%]"
                                        ,@rest))))
               ,list)))

    ;; Recentf
    (defun dashboard-insert-recents (list-size)
      "Add the list of LIST-SIZE items from recently edited files."
      (recentf-mode)
      (dashboard-insert-section
       "Recent Files:"
       recentf-list
       list-size
       "r"
       `(lambda (&rest ignore) (find-file-existing ,el))
       (abbreviate-file-name el)))

    ;; Bookmarks
    (defun dashboard-insert-bookmarks (list-size)
      "Add the list of LIST-SIZE items of bookmarks."
      (require 'bookmark)
      (dashboard-insert-section
       "Bookmarks:"
       (dashboard-subseq (bookmark-all-names)
                         0 list-size)
       list-size
       "m"
       `(lambda (&rest ignore) (bookmark-jump ,el))
       (let ((file (bookmark-get-filename el)))
         (if file
             (format "%s - %s" el (abbreviate-file-name file))
           el))))

    ;; Projectile
    (defun dashboard-insert-projects (list-size)
      "Add the list of LIST-SIZE items of projects."
      (require 'projectile)
      (projectile-load-known-projects)
      (dashboard-insert-section
       "Projects:"
       (dashboard-subseq (projectile-relevant-known-projects)
                         0 list-size)
       list-size
       "p"
       `(lambda (&rest ignore) (projectile-switch-project-by-name ,el))
       (abbreviate-file-name el)))

    (defun dashboard-center-line (&optional real-width)
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

    (defun dashboard-insert-buttons ()
      "Insert buttions after the banner."
      (interactive)
      (with-current-buffer (get-buffer dashboard-buffer-name)
        (read-only-mode -1)
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
                       centaur-homepage)
        (insert " ")
        (widget-create 'push-button
                       :help-echo "Restore previous session"
                       :action (lambda (&rest _) (restore-session))
                       :mouse-face 'highlight
                       :tag (concat
                             (if (display-graphic-p)
                                 (concat
                                  (all-the-icons-material "restore"
                                                          :height 1.35
                                                          :v-adjust -0.24
                                                          :face 'font-lock-keyword-face)
                                  (propertize " " 'face 'variable-pitch)))
                             (propertize "Session" 'face 'font-lock-keyword-face)))
        (insert " ")
        (widget-create 'file-link
                       :tag (concat
                             (if (display-graphic-p)
                                 (concat
                                  (all-the-icons-faicon "cog"
                                                        :height 1.2
                                                        :v-adjust -0.1
                                                        :face 'font-lock-keyword-face)
                                  (propertize " " 'face 'variable-pitch)))
                             (propertize "Settings" 'face 'font-lock-keyword-face))
                       :help-echo "Open custom file"
                       :mouse-face 'highlight
                       custom-file)
        (insert " ")
        (widget-create 'push-button
                       :help-echo "Update Centaur Emacs"
                       :action (lambda (&rest _) (centaur-update))
                       :mouse-face 'highlight
                       :tag (concat
                             (if (display-graphic-p)
                                 (concat
                                  (all-the-icons-material "update"
                                                          :height 1.35
                                                          :v-adjust -0.24
                                                          :face 'font-lock-keyword-face)
                                  (propertize " " 'face 'variable-pitch)))
                             (propertize "Update" 'face 'font-lock-keyword-face)))
        (insert " ")
        (widget-create 'push-button
                       :help-echo "Help (?/h)"
                       :action (lambda (&rest _) (dashboard-hydra/body))
                       :mouse-face 'highlight
                       :tag (concat
                             (if (display-graphic-p)
                                 (all-the-icons-faicon "question"
                                                       :height 1.2
                                                       :v-adjust -0.1
                                                       :face 'font-lock-string-face)
                               (propertize "?" 'face 'font-lock-string-face))))
        (dashboard-center-line)
        (insert "\n\n")

        (insert (concat
                 (propertize (format "%d packages loaded in %s"
                                     (length package-activated-list) (emacs-init-time))
                             'face 'font-lock-comment-face)))
        (dashboard-center-line)

        (read-only-mode 1)))
    (add-hook 'dashboard-mode-hook #'dashboard-insert-buttons)

    (defun dashboard-insert-footer ()
      "Insert footer of dashboard."
      (interactive)
      (with-current-buffer (get-buffer dashboard-buffer-name)
        (read-only-mode -1)
        (goto-char (point-max))

        (insert "\n\n")
        (insert (if (display-graphic-p)
                    (all-the-icons-faicon "heart"
                                          :height 1.1
                                          :v-adjust -0.05
                                          :face 'error)
                  "🧡 "))
        (insert " ")
        (insert (propertize
                 (format "Powered by Vincent Zhang, %s" (format-time-string "%Y"))
                 'face font-lock-doc-face))
        (dashboard-center-line)
        (insert "\n")

        (read-only-mode 1)))
    (add-hook 'dashboard-mode-hook #'dashboard-insert-footer)

    (defhydra dashboard-hydra (:color red :columns 3)
      "Help"
      ("<tab>" widget-forward "Next Widget")
      ("C-i" widget-forward "Prompt")
      ("<backtab>" widget-backward "Previous Widget")
      ("RET" widget-button-press "Press Widget" :exit t)
      ("g" dashboard-refresh-buffer "Refresh" :exit t)
      ("}" dashboard-next-section "Next Section")
      ("{" dashboard-previous-section "Previous Section")
      ("r" dashboard-goto-recent-files "Recent Files")
      ("p" dashboard-goto-projects "Projects")
      ("m" dashboard-goto-bookmarks "Bookmarks")
      ("H" browse-homepage "Browse Homepage" :exit t)
      ("R" restore-session "Restore Previous Session" :exit t)
      ("L" persp-load-state-from-file "List Saved Sessions" :exit t)
      ("S" open-custom-file "Settings" :exit t)
      ("U" centaur-update "Update Centaur Emacs" :exit t)
      ("<f2>" open-dashboard "Open Dashboard" :exit t)
      ("q" quit-dashboard "Quit Dashboard" :exit t)
      ("C-g" nil "quit"))
    (bind-keys :map dashboard-mode-map
               ("h" . dashboard-hydra/body)
               ("?" . dashboard-hydra/body))))

(provide 'init-dashboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
