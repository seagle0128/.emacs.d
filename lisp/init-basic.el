;; init-basic.el --- Initialize basic configurations.
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
;;             Basic configurations.
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

(require 'init-const)

;; Disable ad-handle-definition warning
(setq ad-redefinition-action 'accept)

;; Personal information
(setq user-full-name "Vincent Zhang")
(setq user-mail-address "seagle012@gmail.com")

;; Environment
(use-package exec-path-from-shell
  :defer t
  :if sys/mac-x-p
  :init (add-hook 'after-init-hook 'exec-path-from-shell-initialize))

;; Start server
(if (fboundp 'server-mode)
    ;; Emacs 24 has a proper mode for `server'
    (server-mode 1)
  (progn
    (require 'server)
    (unless (server-running-p)
      (server-start))
    ))

;; History
(if (fboundp 'save-place-mode)
    ;; Emacs 25 has a proper mode for `save-place'
    (save-place-mode 1)
  (progn
    (require 'saveplace)
    (setq save-place t)
    ))

(recentf-mode 1)
(savehist-mode 1)

(provide 'init-basic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
