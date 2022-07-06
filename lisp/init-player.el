;; init-player.el --- Initialize player configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2022 Vincent Zhang

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
;; Player configurations.
;;

;;; Code:

(require 'init-custom)

(when centaur-player
  ;; Music player
  (use-package bongo
    :bind ("C-<f9>" . bongo)
    :config
    (with-eval-after-load 'dired
      (with-no-warnings
        (defun bongo-add-dired-files ()
          "Add marked files to the Bongo library."
          (interactive)
          (bongo-buffer)
          (let (file (files nil))
            (dired-map-over-marks
             (setq file (dired-get-filename)
                   files (append files (list file)))
             nil t)
            (with-bongo-library-buffer
             (mapc 'bongo-insert-file files)))
          (bongo-switch-buffers))
        (bind-key "b" #'bongo-add-dired-files dired-mode-map))))

  ;; Music Player Daemon
  ;; Built-in client for mpd
  (use-package mpc
    :ensure nil
    :bind ("s-<f9>" . mpc)
    :init
    (defun restart-mpd ()
      (interactive)
      (call-process "pkill" nil nil nil "mpd")
      (call-process "mpd"))
    :config
    (with-no-warnings
      (defun add-mpc-status-to-mode-line ()
        "Display current song in mode line."
        (add-to-list 'global-mode-string '("" mpc-current-song)))
      (advice-add #'mpc :after #'add-mpc-status-to-mode-line)))

  ;; Simple client for mpd
  (use-package simple-mpc
    :if (executable-find "mpc")
    :bind ("M-<f9>" . simple-mpc)))

(provide 'init-player)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-player.el ends here
