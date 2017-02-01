;; init-basic.el --- Initialize basic configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 2.1.0
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
(require 'init-custom)

;; Personal information
(setq user-full-name my-full-name)
(setq user-mail-address my-mail-address)

;; Environment
(use-package exec-path-from-shell
  :defer t
  :if sys/macp
  :init (add-hook 'after-init-hook 'exec-path-from-shell-initialize))

;; Start server
(use-package server
  :defer t
  :init (add-hook 'after-init-hook 'server-mode))

;; History
(if (fboundp 'save-place-mode)
    ;; Emacs 25 has a proper mode for `save-place'
    (use-package saveplace
      :defer t
      :init (add-hook 'after-init-hook 'save-place-mode))
  ;; <= Emacs 24
  (use-package saveplace :config (setq save-place t)))

(use-package recentf
  :defer t
  :init (add-hook 'after-init-hook 'recentf-mode))

(use-package savehist
  :defer t
  :init (add-hook 'after-init-hook 'savehist-mode)
  :config (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring)))

(provide 'init-basic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
