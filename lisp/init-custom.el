;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Vincent Zhang

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
;; Customizations.
;;

;;; Code:

(defgroup centaur nil
  "Centaur Emacs customizations."
  :group 'convenience)

(defcustom centaur-logo (expand-file-name "logo.png" user-emacs-directory)
  "Set Centaur logo. nil means official logo."
  :type 'string
  :group 'centaur)

(defcustom centaur-full-name "Vincent Zhang"
  "Set user full name."
  :type 'string
  :group 'centaur)

(defcustom centaur-mail-address "seagle0128@gmail.com"
  "Set user email address."
  :type 'string)

(defcustom centaur-proxy "127.0.0.1:1087"
  "Set network proxy."
  :type 'string
  :group 'centaur)

(defcustom centaur-package-archives 'melpa
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Tuna" tuna))
  :group 'centaur)

(defcustom centaur-theme 'default
  "Set color theme."
  :type '(choice
          (const :tag "Default theme" default)
          (const :tag "Dark theme" dark)
          (const :tag "Light theme" light)
          (const :tag "Daylight theme" daylight)
          (const :tag "Doom theme" doom)
          symbol)
  :group 'centaur)

(defcustom centuar-company-enable-yas nil
  "Enable/disable yasnippet for company backends."
  :type 'boolean
  :group 'centaur)

(defcustom centaur-emoji-enabled nil
  "Enable/disable emoji features or not."
  :type 'boolean
  :group 'centaur)

(defcustom centaur-benchmark-enabled nil
  "Enable/disable the init benchmark."
  :type 'boolean
  :group 'centaur)

;; Load `custome.el' file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
