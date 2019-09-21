;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

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
;; Customizations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

(defgroup centaur nil
  "Centaur Emacs customizations."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/seagle0128/.emacs.d"))

(defcustom centaur-logo (expand-file-name "logo.png" user-emacs-directory)
  "Set Centaur logo. nil means official logo."
  :group 'centaur
  :type 'string)

(defcustom centaur-full-name "Vincent Zhang"
  "Set user full name."
  :group 'centaur
  :type 'string)

(defcustom centaur-mail-address "seagle0128@gmail.com"
  "Set user email address."
  :group 'centaur
  :type 'string)

(defcustom centaur-proxy "127.0.0.1:1087"
  "Set network proxy."
  :group 'centaur
  :type 'string)

(defcustom centaur-package-archives 'melpa
  "Set package archives from which to fetch."
  :group 'centaur
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Melpa Mirror" melpa-mirror)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Netease" netease)
          (const :tag "Tencent" tencent)
          (const :tag "Tuna" tuna)))

(defcustom centaur-theme 'default
  "Set color theme."
  :group 'centaur
  :type '(choice
          (const :tag "Default theme" default)
          (const :tag "Classic theme" classic)
          (const :tag "Dark theme" dark)
          (const :tag "Light theme" light)
          (const :tag "Daylight theme" daylight)
          symbol))

(defcustom centaur-dashboard t
  "Use dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 'centaur
  :type 'boolean)

(defcustom centaur-lsp 'lsp-mode
  "Set language server."
  :group 'centaur
  :type '(choice
          (const :tag "LSP Mode" 'lsp-mode)
          (const :tag "eglot" 'eglot)
          nil))

(defcustom centaur-chinese-calendar t
  "Use Chinese calendar or not."
  :group 'centaur
  :type 'boolean)

(defcustom centaur-benchmark nil
  "Enable the init benchmark or not."
  :group 'centaur
  :type 'boolean)

;; Load `custom-file'
;; If it doesn't exist, copy from the template, then load it.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(let ((custom-template-file
       (expand-file-name "custom-template.el" user-emacs-directory)))
  (if (and (file-exists-p custom-template-file)
           (not (file-exists-p custom-file)))
      (copy-file custom-template-file custom-file)))

;; Load `custom-post.el'
;; Put personal configurations to override defaults here.
(add-hook 'after-init-hook
          (lambda ()
            (if (file-exists-p custom-file)
                 (load custom-file))
            (let ((file
                   (expand-file-name "custom-post.el" user-emacs-directory)))
              (if (file-exists-p file)
                  (load file)))))

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
