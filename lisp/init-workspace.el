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

(use-package project-x
  :custom
  ;; auto-save project state after 5 seconds of idle time
  (project-x-auto-save-delay 5) ; nil to disable autosave
  ;; use the custom prompter that shows session labels
  (project-prompter #'project-x--project-prompt)
  ;; automatically restore the last project on startup
  (project-x-restore-last-project-on-startup (not centaur-dashboard))
  ;; not display tab bar
  (tab-bar-show nil)
  :hook
  (after-init     . project-x-mode)
  (project-x-mode . project-x-tabs-mode))

(provide 'init-workspace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-workspace.el ends here
