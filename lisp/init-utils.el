;; init-utils.el --- Initialize basic configurations.
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
;;             Utils configurations.
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

;; Which key
(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode 1))

;; Browse url
(use-package browse-url-dwim
  :config
  (browse-url-dwim-mode 1)
  (setq browse-url-dwim-always-confirm-extraction nil))

;; Tramp
(use-package tramp
  :defer t  
  :init
  (let ((val (if (executable-find "plink") "plink" "ssh")))
    (setq tramp-default-method val)))

;; Dos2Unix
(defun dos2unix ()
  "Not exactly but it's easier to remember."
  (interactive)
  (set-buffer-file-coding-system 'unix 't) )

;; Tree explorer
(use-package neotree
  :defer t
  :bind (([f12] . neotree-toggle)
         ([C-f12] . neotree-toggle))
  :init
  (add-hook 'neotree-mode-hook
            '(lambda ()
               (linum-mode -1))))

;; Revert buffer
(global-set-key [(f5)] '(lambda ()
                          (interactive)
                          (message "Revert this buffer.")
                          (revert-buffer t t)))

;; Text zoom in/out
(global-set-key [(C-wheel-up)] 'text-scale-increase)
(global-set-key [(C-wheel-down)] 'text-scale-decrease)

;; Dash
(when sys/macp
  (use-package dash
    :defer t
    :bind (("\C-cd" . dash-at-point)
           ("\C-ce" . dash-at-point-with-docset))))

;; Youdao Dict
(use-package youdao-dictionary
  :defer t
  :config (setq url-automatic-caching t)
  :bind ("C-c y" . youdao-dictionary-search-at-point))

(when (executable-find "ack") (use-package ack :defer t))
(when (executable-find "ag") (use-package ag :defer t))
(use-package htmlize :defer t)
(use-package list-environment :defer t)

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
