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
(require 'init-funcs)

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
      (call-process "mpd")))

  ;; Simple client for mpd
  (use-package simple-mpc
    :if (executable-find "mpc")
    :commands (simple-mpc-call-mpc simple-mpc-call-mpc-strings)
    :functions (simple-mpc-current simple-mpc-start-timer)
    :bind (("M-<f9>" . simple-mpc)
           :map simple-mpc-mode-map
           ("P" . simple-mpc-play)
           ("O" . simple-mpc-stop))
    :init
    (setq simple-mpc-playlist-format "[[%artist% - ]%title%]|[%file%]")

    (defun simple-mpc-play ()
      "Start playing the song."
      (interactive)
      (simple-mpc-call-mpc nil "play"))

    (defun simple-mpc-stop ()
      "Stop the playback."
      (interactive)
      (simple-mpc-call-mpc nil "stop"))

    ;; Display current song in mode-line
    (defvar simple-mpc-current nil)
    (add-to-list 'global-mode-string '("" (:eval simple-mpc-current)))

    (defun simple-mpc-current ()
      "Get current song information."
      (setq simple-mpc-current
            (let ((strs (simple-mpc-call-mpc-strings nil)))
              (when (length> strs 2)
                (when-let* ((title (nth 0 strs))
                            (info (nth 1 strs))
                            (info-strs (split-string info))
                            (state (nth 0 info-strs))
                            (time (nth 2 info-strs)))
                  (propertize (format " %s%s [%s] "
                                      (when (icon-displayable-p)
                                        (pcase state
                                          ("[playing]" " ")
                                          ("[paused]" " ")
                                          (_ "")))
                                      title time)
                              'face 'font-lock-comment-face)))))
      (force-mode-line-update))

    (defvar simple-mpc--timer nil)
    (defun simple-mpc-start-timer ()
      "Start simple-mpc timer to refresh current song."
      (setq simple-mpc--timer (run-with-timer 1 1 #'simple-mpc-current)))
    (defun simple-mpc-stop-timer ()
      "Stop simple-mpc timer."
      (when (timerp simple-mpc--timer)
        (cancel-timer simple-mpc--timer)))
    (simple-mpc-start-timer)))

(provide 'init-player)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-player.el ends here
