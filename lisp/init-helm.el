;;; init-helm.el --- Initialize helm configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 2.2.0
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
  :diminish helm-mode
  :commands (helm-autoresize-mode helm-info-emacs)
  :bind (("C-x b"   . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x r l" . helm-filtered-bookmarks)
         ("M-y"     . helm-show-kill-ring)
         ("C-h a"   . helm-apropos)
         ("C-h i"   . helm-info-emacs)
         ("C-."     . helm-imenu)
         :map helm-map
         ;; exchange TAB and C-z
         ("TAB" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :init
  (add-hook 'after-init-hook
            '(lambda ()
               (helm-mode 1)
               (helm-adaptive-mode 1)))
  (add-hook 'desktop-after-read-hook
            '(lambda () (diminish 'helm-mode)))
  :config
  (use-package helm-config
    :ensure nil
    :demand)

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

  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook
              '(lambda ()
                 (bind-key [remap eshell-pcomplete]
                           'helm-esh-pcomplete eshell-mode-map))))

  ;; Plugins
  (use-package helm-flx
    :init (helm-flx-mode 1))

  (use-package helm-descbinds
    :bind ("C-h b" . helm-descbinds))

  (use-package helm-swoop
    :bind (("M-s o" . helm-swoop)
           ("M-s /" . helm-multi-swoop)))

  (use-package helm-ag
    :bind ("M-s s" . helm-ag))

  (use-package helm-mt
    :bind ("C-x t" . helm-mt))

  (use-package helm-cmd-t
    :bind (("C-S-t" . helm-cmd-t)
           ("s-t" . helm-cmd-t)))

  (use-package helm-gtags
    :diminish helm-gtags-mode
    :bind (:map helm-gtags-mode-map
                ("M-t" . helm-gtags-find-tag)
                ("M-r" . helm-gtags-find-rtag)
                ("M-s" . helm-gtags-find-symbol)
                ("M-g M-p" . helm-gtags-parse-file)
                ("C-c <" . helm-gtags-previous-history)
                ("C-c >" . helm-gtags-next-history)
                ("M-," . helm-gtags-pop-stack))
    :init (add-hook 'c-mode-common-hook
                    (lambda ()
                      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                        (helm-gtags-mode 1))))
    :config (setq helm-gtags-auto-update t))

  (use-package helm-bm)
  (use-package helm-flycheck)
  (use-package helm-ls-git)
  (use-package helm-projectile)

  (use-package flyspell-correct-helm
    :bind (:map flyspell-mode-map
                ("C-;" . flyspell-correct-previous-word-generic)))

  ;; Combines isearch, ace-jump-mode, avy and helm-swoop.
  (use-package ace-isearch
    :diminish ace-isearch-mode
    :bind (:map isearch-mode-map
                ("C-:" . ace-isearch-jump-during-isearch))
    :init
    (setq ace-isearch-function 'avy-goto-char)
    (setq ace-isearch-use-jump 'printing-char)
    (global-ace-isearch-mode 1)))

(provide 'init-helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-helm.el ends here
