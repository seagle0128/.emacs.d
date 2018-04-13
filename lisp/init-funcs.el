;; init-funcs.el --- Initialize functions.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 3.3.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Some functions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; Revert buffer
(defun revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer.")
  (revert-buffer t t))
(bind-key "<f5>" #'revert-current-buffer)
(bind-key "s-r" #'revert-current-buffer)

;; Update configurations
(defun update-config ()
  "Update Emacs configurations to the latest version."
  (interactive)
  (message "Updating Emacs configurations...")
  (cd "~/.emacs.d/")
  (shell-command "git pull")
  (message "Update finished. Restart Emacs to complete the process."))

;; Update dotfiles
(defun update-dotfiles ()
  "Update the dotfiles to the latest version."
  (interactive)
  (message "Updating dotfiles...")
  (cd "~/.dotfiles/")
  (shell-command "git pull")
  (message "Update finished."))

;; Update org files
(defun update-org ()
  "Update Org files to the latest version."
  (interactive)
  (message "Updating Org files...")
  (cd "~/org/")
  (shell-command "git pull")
  (message "Update finished."))

;; Create a new scratch buffer
(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

;; Save a file as utf-8
(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

;; Configure network proxy
(defun show-proxy ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current proxy is \"%s\"" my-proxy)
    (message "No proxy")))

(defun set-proxy ()
  "Set http/https proxy."
  (interactive)
  (setq url-proxy-services `(("http" . ,my-proxy)
                             ("https" . ,my-proxy)))
  (show-proxy))

(defun unset-proxy ()
  "Unset http/https proxy."
  (interactive)
  (setq url-proxy-services nil)
  (show-proxy))

(defun toggle-proxy ()
  "Toggle http/https proxy."
  (interactive)
  (if url-proxy-services
      (unset-proxy)
    (set-proxy)))

(provide 'init-funcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-funcs.el ends here
