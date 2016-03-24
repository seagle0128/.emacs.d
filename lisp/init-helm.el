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

(use-package helm
  ;; :defer t
  :diminish helm-mode
  :defines (helm-M-x-fuzzy-match
            helm-imenu-fuzzy-match
            helm-apropos-fuzzy-match
            helm-semantic-fuzzy-match
            helm-lisp-fuzzy-completion
            eshell-mode-map)
  :commands helm-autoresize-mode helm-info-emacs
  :bind
  (("C-x b"   . helm-mini)
   ("C-x C-b" . helm-buffers-list)
   ("M-x"     . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-recentf)
   ("C-x r l" . helm-filtered-bookmarks)
   ("M-y"     . helm-show-kill-ring)
   ("C-h a"   . helm-apropos)
   ("C-h i"   . helm-info-emacs)
   ("C-."     . helm-imenu))
  :config
  (progn
    (require 'helm-config)

    ;; (setq helm-ff-lynx-style-map nil
    ;;       helm-input-idle-delay 0.1
    ;;       helm-idle-delay 0.1)

    (setq helm-ff-guess-ffap-filenames t)

    ;; fuzzy match
    (setq helm-M-x-fuzzy-match t)
    (setq helm-locate-fuzzy-match t)
    (setq helm-imenu-fuzzy-match t)
    (setq helm-buffers-fuzzy-matching t)
    (setq helm-recentf-fuzzy-match t)
    (setq helm-apropos-fuzzy-match t)
    (setq helm-semantic-fuzzy-match t)
    (setq helm-lisp-fuzzy-completion t)

    ;; eshell
    (eval-after-load 'eshell
      '(bind-key [remap eshell-pcomplete] 'helm-esh-pcomplete eshell-mode-map))

    ;; exchange TAB and C-z
    (bind-key "TAB" 'helm-execute-persistent-action helm-map)
    (bind-key "C-z" 'helm-select-action helm-map)
    ;; (bind-key "S-TAB" 'helm-find-files-up-one-level helm-find-files-map)

    ;; modes
    (helm-mode 1)
    ;; (helm-autoresize-mode 1)
    ;; (helm-adaptive-mode 1)

    ;; plugins
    (use-package helm-descbinds
      :defer t
      :bind ("C-h b" . helm-descbinds))

    (use-package helm-swoop
      :defer t
      :bind (("M-s o" . helm-swoop)
             ("M-s /" . helm-multi-swoop)))

    (use-package helm-ag
      :defer t
      :bind ("M-s s" . helm-ag))

    (use-package helm-mt
      :defer t
      :bind ("C-x t" . helm-mt))

    (use-package helm-cmd-t
      :defer t
      :bind (("C-S-t" . helm-cmd-t)
             ("s-t" . helm-cmd-t)))

    (use-package helm-ls-git :defer t)
    (use-package helm-projectile :defer t)
    (use-package helm-flycheck :defer t)
    (use-package helm-bm :defer t)
    ))

(provide 'init-helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-helm.el ends here
