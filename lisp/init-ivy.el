;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 3.1.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Ivy configurations.
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

(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-S-s" . swiper-all)
         ("C-c M-x" . execute-extended-command)

         ("C-c C-r" . ivy-resume)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)

         ("C-." . counsel-imenu)
         ("C-x C-r" . counsel-recentf)
         ("C-h F" . counsel-find-library)
         ("C-h u" . counsel-unicode-char)
         ("C-c c" . counsel-colors-emacs)
         ("C-c w" . counsel-colors-web)
         ("C-c i" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c r" . counsel-rg)
         ("C-c P" . counsel-pt)
         ("C-c S" . counsel-ag)
         ("C-c f" . counsel-fzf)
         ("C-c l" . counsel-locate)
         ("C-c L" . counsel-load-library)
         ("C-c C-p" . counsel-package)
         ("C-x j" . counsel-mark-ring)
         ("C-x r b" . counsel-bookmark)
         ("C-x r m" . counsel-bookmark)

         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)

         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)

         :map swiper-map
         ("M-%" . swiper-query-replace))
  :init (add-hook 'after-init-hook
                  (lambda ()
                    (ivy-mode 1)
                    (counsel-mode 1)))
  :config
  (setq ivy-use-virtual-buffers t)    ; Enable bookmarks and recentf
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function nil)
  ;; (setq ivy-initial-inputs-alist nil)

  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))

  (setq swiper-action-recenter t)
  (setq counsel-find-file-at-point t)
  (setq counsel-yank-pop-separator "\n-------\n")

  ;; Use faster search tools: ripgrep or the silver search
  (let ((command
         (cond
          ((executable-find "rg")
           "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
          ((executable-find "ag")
           "ag -i --noheading --nocolor --nofilename --numbers '%s' %s"))))
    (setq counsel-grep-base-command command))

  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  ;; Search at point
  ;; "M-j": word-at-point
  ;; "M-n"/"C-w": symbol-at-point
  ;; Refer to https://www.emacswiki.org/emacs/SearchAtPoint#toc8
  ;; and https://github.com/abo-abo/swiper/wiki/FAQ
  ;; (bind-key "C-w" (lambda ()
  ;;                   (interactive)
  ;;                   (insert (format "%s" (with-ivy-window (ivy-thing-at-point)))))
  ;;           ivy-minibuffer-map)

  ;; Enhance M-x
  (use-package smex)

  ;; Additional key bindings for Ivy
  (use-package ivy-hydra
    :bind (:map ivy-minibuffer-map
                ("M-o" . ivy-dispatching-done-hydra)))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :init (counsel-projectile-on))

  ;; Correcting words with flyspell via Ivy
  (use-package flyspell-correct-ivy
    :after flyspell
    :bind (:map flyspell-mode-map
                ("C-;" . flyspell-correct-previous-word-generic)))

  ;; More friendly display transformer for Ivy
  (use-package ivy-rich
    :init
    (setq ivy-virtual-abbreviate 'full
          ivy-rich-switch-buffer-align-virtual-buffer nil)
    (setq ivy-rich-abbreviate-paths t)

    (ivy-set-display-transformer 'ivy-switch-buffer
                                 'ivy-rich-switch-buffer-transformer)

    (with-eval-after-load 'counsel-projectile
      (ivy-set-display-transformer 'counsel-projectile
                                   'ivy-rich-switch-buffer-transformer)
      (ivy-set-display-transformer 'counsel-projectile-switch-to-buffer
                                   'ivy-rich-switch-buffer-transformer)))

  ;; Support pinyin in Ivy
  ;; Input prefix ':' to match pinyin
  ;; Refer to  https://github.com/abo-abo/swiper/issues/919
  (use-package pinyinlib
    :commands pinyinlib-build-regexp-string
    :init
    (defun my-pinyinlib-build-regexp-string (str)
      (cond ((equal str ".*")
             ".*")
            (t
             (pinyinlib-build-regexp-string str t))))

    (defun my-pinyin-regexp-helper (str)
      (cond ((equal str " ")
             ".*")
            ((equal str "")
             nil)
            (t
             str)))

    (defun pinyin-to-utf8 (str)
      (cond ((equal 0 (length str))
             nil)
            ((equal (substring str 0 1) ":")
             (mapconcat 'my-pinyinlib-build-regexp-string
                        (remove nil (mapcar 'my-pinyin-regexp-helper
                                            (split-string
                                             (replace-regexp-in-string ":" "" str ) "")))
                        ""))
            nil))

    (defun re-builder-pinyin (str)
      (or (pinyin-to-utf8 str)
          (ivy--regex-plus str)))

    (setq ivy-re-builders-alist
          '((t . re-builder-pinyin))))

  ;; Ivy for GNU global
  (use-package counsel-gtags
    :diminish counsel-gtags-mode
    :bind (:map counsel-gtags-mode-map
                ("M-." . counsel-gtags-find-definition)
                ("M-r" . counsel-gtags-find-reference)
                ("M-s" . counsel-gtags-find-symbol)
                ("M-," . counsel-gtags-go-backward))
    :init
    (setq counsel-gtags-auto-update t)

    (add-hook 'c-mode-hook 'counsel-gtags-mode)
    (add-hook 'c++-mode-hook 'counsel-gtags-mode)))

(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
