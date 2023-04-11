;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 Vincent Zhang

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

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Dashboard
(when centaur-dashboard
  (use-package dashboard
    :diminish dashboard-mode
    :functions (nerd-icons-faicon
                nerd-icons-mdicon
                winner-undo
                widget-forward)
    :custom-face
    (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
    (dashboard-items-face ((t (:weight normal))))
    (dashboard-no-items-face ((t (:weight normal))))
    :pretty-hydra
    ((:title (pretty-hydra-title "Dashboard" 'mdicon "nf-md-dashboard")
      :color pink :quit-key ("q" "C-g"))
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
    :hook (dashboard-mode . (lambda ()
                              ;; No title
                              (setq-local frame-title-format nil)
                              ;; Enable `page-break-lines-mode'
                              (when (fboundp 'page-break-lines-mode)
                                (page-break-lines-mode 1))))
    :init
    (setq dashboard-banner-logo-title "CENTAUR EMACS - Enjoy Programming & Writing"
          dashboard-startup-banner (or centaur-logo 'official)
          dashboard-page-separator "\n\f\n"
          dashboard-center-content t
          dashboard-show-shortcuts nil
          dashboard-items '((recents  . 10)
                            (bookmarks . 5)
                            (projects . 5))

          dashboard-set-init-info t
          dashboard-display-icons-p #'icons-displayable-p
          dashboard-set-file-icons centaur-icon
          dashboard-set-heading-icons centaur-icon
          dashboard-heading-icons '((recents   . "nf-cod-history")
                                    (bookmarks . "nf-cod-bookmark")
                                    (agenda    . "nf-cod-calendar")
                                    (projects  . "nf-cod-folder")
                                    (registers . "nf-cod-database"))

          dashboard-set-footer t
          dashboard-footer-icon (cond
                                 ((icons-displayable-p)
                                  (nerd-icons-mdicon "nf-md-heart" :height 1.2 :face 'nerd-icons-lred))

                                 (t (propertize ">" 'face 'dashboard-footer)))

          dashboard-set-navigator t
          dashboard-navigator-buttons
          `(((,(when (icons-displayable-p)
                 (nerd-icons-mdicon "nf-md-github" :height 1.5))
              "Homepage" "Browse homepage"
              (lambda (&rest _) (browse-url centaur-homepage)))
             (,(when (icons-displayable-p)
                 (nerd-icons-mdicon "nf-md-backup_restore" :height 1.5))
              "Restore" "Restore previous session"
              (lambda (&rest _) (restore-previous-session)))
             (,(when (icons-displayable-p)
                 (nerd-icons-mdicon "nf-md-tools" :height 1.5))
              "Settings" "Open custom file"
              (lambda (&rest _) (find-file custom-file)))
             (,(when (icons-displayable-p)
                 (nerd-icons-mdicon "nf-md-update" :height 1.5))
              "Update" "Update Centaur Emacs"
              (lambda (&rest _) (centaur-update)))
             (,(if (icons-displayable-p)
                   (nerd-icons-mdicon "nf-md-help" :height 1.5)
                 "?")
              "" "Help (?/h)"
              (lambda (&rest _) (dashboard-hydra/body))
              font-lock-string-face))))

    (dashboard-setup-startup-hook)
    :config
    (defmacro dashboard-insert-section-list (section-name list action &rest rest)
      "Insert into SECTION-NAME a LIST of items, expanding ACTION and passing REST
to widget creation."
      `(when (car ,list)
         (mapc
          (lambda (el)
            (let ((tag ,@rest))
              (insert "\n    ")

              (when (and (dashboard-display-icons-p)
                         dashboard-set-file-icons
                         (or (fboundp 'nerd-icons-icon-for-dir)
                             (require 'nerd-icons nil 'noerror)))
                (let* ((path (car (last (split-string ,@rest " - "))))
                       (icon (if (and (not (file-remote-p path))
                                      (file-directory-p path))
                                 (nerd-icons-icon-for-dir path nil "")
                               (cond
                                ((or (string-equal ,section-name "Agenda for today:")
                                     (string-equal ,section-name "Agenda for the coming week:"))
                                 (nerd-icons-codicon "nf-oct-primitive_dot"))
                                (t (nerd-icons-icon-for-file (file-name-nondirectory path)))))))
                  (setq tag (concat icon " " ,@rest))))

              (widget-create 'item
                             :tag tag
                             :action ,action
                             :button-face 'dashboard-items-face
                             :mouse-face 'highlight
                             :button-prefix ""
                             :button-suffix ""
                             :format "%[%t%]")))
          ,list)))
    ;; (advice-add #'dashboard-insert-section-list :override #'my-dashboard-insert-section-list)

    ;; Insert heading
    (defun my-dashboard-insert-heading (heading &optional shortcut icon)
      "Insert a widget HEADING in dashboard buffer, adding SHORTCUT, ICON if provided."
      (when (and (dashboard-display-icons-p) dashboard-set-heading-icons)
        ;; Try loading `nerd-icons'
        (unless (or (fboundp 'nerd-icons-codicon)
                    (require 'nerd-icons nil 'noerror))
          (error "Package `nerd-icons' isn't installed"))

        (insert (cond
                 ((string-equal heading "Recent Files:")
                  (nerd-icons-codicon (cdr (assoc 'recents dashboard-heading-icons))
                                      :height 1.1 :face 'dashboard-heading))
                 ((string-equal heading "Bookmarks:")
                  (nerd-icons-codicon (cdr (assoc 'bookmarks dashboard-heading-icons))
                                      :height 1.1 :face 'dashboard-heading))
                 ((or (string-equal heading "Agenda for today:")
                      (string-equal heading "Agenda for the coming week:"))
                  (nerd-icons-codicon (cdr (assoc 'agenda dashboard-heading-icons))
                                      :height 1.1 :face 'dashboard-heading))
                 ((string-equal heading "Registers:")
                  (nerd-icons-codicon (cdr (assoc 'registers dashboard-heading-icons))
                                      :height 1.1 :face 'dashboard-heading))
                 ((string-equal heading "Projects:")
                  (nerd-icons-codicon (cdr (assoc 'projects dashboard-heading-icons))
                                      :height 1.1 :face 'dashboard-heading))
                 ((not (null icon)) icon)
                 (t " ")))
        (insert " "))

      (insert (propertize heading 'face 'dashboard-heading))

      ;; Turn the inserted heading into an overlay, so that we may freely change
      ;; its name without breaking any of the functions that expect the default name.
      ;; If there isn't a suitable entry in `dashboard-item-names',
      ;; we fallback to using HEADING.  In that case we still want it to be an
      ;; overlay to maintain consistent behavior (such as the point movement)
      ;; between modified and default headings.
      (let ((ov (make-overlay (- (point) (length heading)) (point) nil t)))
        (overlay-put ov 'display (or (cdr (assoc heading dashboard-item-names)) heading))
        (overlay-put ov 'face 'dashboard-heading))
      (when shortcut (insert (format " (%s)" shortcut))))
    (advice-add #'dashboard-insert-heading :override #'my-dashboard-insert-heading)

    ;; Insert copyright
    ;; @see https://github.com/emacs-dashboard/emacs-dashboard/issues/219
    (defun my-dashboard-insert-copyright ()
      "Insert copyright in the footer."
      (when dashboard-set-footer
        (dashboard-insert-center
         (propertize (format "\nPowered by Vincent Zhang, %s\n" (format-time-string "%Y"))
                     'face 'font-lock-comment-face))))
    (advice-add #'dashboard-insert-footer :after #'my-dashboard-insert-copyright)

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
      (and dashboard-recover-layout-p
           (and (bound-and-true-p winner-mode) (winner-undo))
           (setq dashboard-recover-layout-p nil)))))

(provide 'init-dashboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
