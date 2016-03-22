;;; init-helm.el --- Initialize helm configurations.
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
;;             Some basic configurations for helm mode.
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

(require 'helm-config)

;; (setq helm-ff-lynx-style-map nil
;;       helm-input-idle-delay 0.1
;;       helm-idle-delay 0.1)

(set helm-ff-guess-ffap-filenames t)

;; fuzzy match
(setq helm-M-x-fuzzy-match t)
(setq helm-locate-fuzzy-match t)
(setq helm-imenu-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(setq helm-semantic-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)

;; key bindings
(global-set-key (kbd "C-x b")   #'helm-mini)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
(global-set-key (kbd "M-x")     #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x C-r") #'helm-recentf)
(global-set-key (kbd "C-x r l") #'helm-filtered-bookmarks)
(global-set-key (kbd "M-y")     #'helm-show-kill-ring)
(global-set-key (kbd "C-h a")   #'helm-apropos)
(global-set-key (kbd "C-h i")   #'helm-info-emacs)

;; eshell
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map
                [remap eshell-pcomplete]
                'helm-esh-pcomplete)))

;; plugins
(global-set-key (kbd "C-h b")   #'helm-descbinds)
(global-set-key (kbd "M-s o")   #'helm-swoop)
(global-set-key (kbd "M-s /")   #'helm-multi-swoop)
(global-set-key (kbd "M-s s")   #'helm-ag)
(global-set-key (kbd "C-.")     #'helm-imenu)
(global-set-key (kbd "C-x t")   #'helm-mt)
(if sys/macp
    (global-set-key (kbd "s-t") #'helm-cmd-t)
  (global-set-key (kbd "C-S-t") #'helm-cmd-t))

;; modes
(helm-mode 1)
(add-hook 'helm-mode-hook
          (lambda()
            (helm-autoresize-mode 1)
            (helm-adaptive-mode 1)

            (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
            (define-key helm-find-files-map (kbd "S-TAB") 'helm-find-files-up-one-level)
            (define-key helm-map (kbd "C-z") 'helm-select-action)
            ))

(provide 'init-helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-helm.el ends here
