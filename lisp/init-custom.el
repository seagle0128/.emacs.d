;; init-custom.el --- Initialize custom configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Custom configurations.
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

(defgroup my nil
  "Personal Emacs configurations."
  :group 'extensions)

(defcustom my-ac-method 'company
  "Auto complete method: `company' or `auto-complete'."
  :type '(choice
          (const :tag "Company" company)
          (const :tag "Auto-Complete" auto-complete)))

(defcustom my-completion-method 'helm
  "Incremental complition method: `helm', `ivy' or `ido'."
  :type '(choice
          (const :tag "Helm" helm)
          (const :tag "Ivy" ivy)
          (const :tag "Ido" ido)))

(defcustom my-desktop-restore t
  "Restore desktop inlcuding buffers, sessions or not."
  :type 'boolean)

(defcustom my-profile-enable nil
  "Enable the init profiler or not."
  :type 'boolean)

(let ((file (concat user-emacs-directory "custom.el")))
  (if (file-exists-p file)
      (load-file file)))

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
