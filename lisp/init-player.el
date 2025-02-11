;; init-player.el --- Initialize player configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2025 Vincent Zhang

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

(eval-when-compile
  (require 'init-custom))

(when centaur-player
  ;; Music player
  (use-package bongo
    :bind ("C-<f8>" . bongo)
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

  ;;
  ;; Music Player Daemon
  ;;
  ;; Built-in mpc client
  (use-package mpc
    :ensure nil
    :bind ("S-<f8>" . mpc)
    :init
    (defun restart-mpd ()
      (interactive)
      (call-process "pkill" nil nil nil "mpd")
      (call-process "mpd")))

  ;; MPD Interface
  (use-package mingus
    :bind ("M-<f8>" . mingus))

  ;; Simple mpd client
  (when (executable-find "mpc")
    (use-package simple-mpc
      :custom-face
      (simple-mpc-main-name ((t (:inherit font-lock-string-face :bold t :height 1.3))))
      (simple-mpc-main-headers ((t (:inherit font-lock-keyword-face :bold t :height 1.1))))
      (simple-mpc-current-track-face ((t (:inherit font-lock-keyword-face))))
      :bind (("s-<f8>" . simple-mpc+)
             :map simple-mpc-mode-map
             ("P" . simple-mpc-play)
             ("O" . simple-mpc-stop)
             ("u" . simple-mpc-update))
      :init (setq simple-mpc-playlist-format
                  "[%time% ][[%title%[ - %artist%[ (%album%)]]]|[%file%]]")
      :config
      (with-no-warnings
        (defun simple-mpc-play ()
          "Start playing the song."
          (interactive)
          (simple-mpc-call-mpc nil "play"))

        (defun simple-mpc-stop ()
          "Stop the playback."
          (interactive)
          (simple-mpc-call-mpc nil "stop"))

        (defun simple-mpc-update ()
          "Update database."
          (interactive)
          (message "Updating music DB...")
          (simple-mpc-call-mpc nil "update")
          (message "Updating music DB...done"))

        ;; Enhance UI
        (defun simple-mpc+ (&optional _ignore-auto _noconfirm)
          "Start simple-mpc.

IGNORE-AUTO and NOCONFIRM are passed by `revert-buffer'."
          (interactive)
          (let ((buf (get-buffer-create simple-mpc-main-buffer-name)))
            (with-current-buffer buf
              (read-only-mode -1)
              (erase-buffer)
              (insert (propertize "🔊 Simple MPC\n"
                                  'face 'simple-mpc-main-name)

                      (propertize "\n  ⚙ Controls\n" 'face 'simple-mpc-main-headers)
                      "\t [t]oggle\n"
                      "\t [n]ext track\n"
                      "\t [p]revious track\n"
                      "\t seek [f]orward\n"
                      "\t seek [b]ackward\n"
                      "\t increase [V]olume\n"
                      "\t decrease [v]olume\n"
                      "\t toggle [r]epeat mode\n"

                      (propertize "\n  🔈 Playlist\n" 'face 'simple-mpc-main-headers)
                      "\t Start [P]laying\n"
                      "\t St[O]p playing\n"
                      "\t view [c]urrent playlist\n"
                      "\t [C]lear current playlist\n"
                      "\t [S]huffle playlist\n"
                      "\t [l]oad playlist\n"
                      "\t [u]pdate database\n"
                      "\t [s]earch database\n"

                      (propertize "\n 🛠 Misc\n" 'face 'simple-mpc-main-headers)
                      "\t [q]uit")
              (simple-mpc-mode) ; start major mode
              (switch-to-buffer buf))))

        (define-advice simple-mpc-format-as-table (:around (fn &rest args) plus)
          "Prettify playlist."
          (propertize (apply fn args) 'face 'font-lock-constant-face))

        ;; Display current song in mode-line
        (defvar simple-mpc-current nil)
        (add-to-list 'global-mode-string '("" (:eval simple-mpc-current)))

        (defun simple-mpc-current ()
          "Get current song information."
          (setq simple-mpc-current
                (when (derived-mode-p 'simple-mpc-mode)
                  (let ((strs (simple-mpc-call-mpc-strings nil)))
                    (when (length> strs 2)
                      (when-let* ((title (nth 0 strs))
                                  (info (nth 1 strs))
                                  (info-strs (split-string info))
                                  (state (nth 0 info-strs))
                                  (time (nth 2 info-strs)))
                        (concat
                         (when (icons-displayable-p)
                           (pcase state
                             ("[playing]"
                              (concat
                               " "
                               (nerd-icons-mdicon "nf-md-play_circle_outline" :face font-lock-comment-face)))
                             ("[paused]"
                              (concat
                               " "
                               (nerd-icons-mdicon "nf-md-pause_circle_outline" :face font-lock-comment-face)))
                             (_ "")))
                         (propertize (format " %s [%s] " title time)
                                     'face '(:inherit 'font-lock-comment-face :height 0.9))))))))
          (force-mode-line-update))

        (defvar simple-mpc--timer nil)
        (defun simple-mpc-start-timer ()
          "Start simple-mpc timer to refresh current song."
          (setq simple-mpc--timer (run-with-timer 1 1 #'simple-mpc-current)))
        (defun simple-mpc-stop-timer ()
          "Stop simple-mpc timer."
          (when (timerp simple-mpc--timer)
            (cancel-timer simple-mpc--timer)))
        (simple-mpc-start-timer)))))

(provide 'init-player)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-player.el ends here
