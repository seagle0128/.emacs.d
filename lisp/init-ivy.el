;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-

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
;; Ivy configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package counsel
  :diminish ivy-mode counsel-mode
  :defines (projectile-completion-system magit-completing-read-function)
  :bind (("C-s" . swiper)
         ("C-S-s" . swiper-all)

         ("C-c C-r" . ivy-resume)
         ("C-c v p" . ivy-push-view)
         ("C-c v o" . ivy-pop-view)
         ("C-c v ." . ivy-switch-view)

         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         ("C-x C-r" . counsel-recentf)
         ("C-x j" . counsel-mark-ring)

         ("C-c L" . counsel-load-library)
         ("C-c P" . counsel-package)
         ("C-c f" . counsel-find-library)
         ("C-c g" . counsel-grep)
         ("C-c h" . counsel-command-history)
         ("C-c i" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c l" . counsel-locate)
         ("C-c r" . counsel-rg)
         ("C-c z" . counsel-fzf)

         ("C-c c L" . counsel-load-library)
         ("C-c c P" . counsel-package)
         ("C-c c a" . counsel-apropos)
         ("C-c c e" . counsel-colors-emacs)
         ("C-c c f" . counsel-find-library)
         ("C-c c g" . counsel-grep)
         ("C-c c h" . counsel-command-history)
         ("C-c c i" . counsel-git)
         ("C-c c j" . counsel-git-grep)
         ("C-c c l" . counsel-locate)
         ("C-c c m" . counsel-minibuffer-history)
         ("C-c c o" . counsel-outline)
         ("C-c c p" . counsel-pt)
         ("C-c c r" . counsel-rg)
         ("C-c c s" . counsel-ag)
         ("C-c c t" . counsel-load-theme)
         ("C-c c u" . counsel-unicode-char)
         ("C-c c w" . counsel-colors-web)
         ("C-c c z" . counsel-fzf)

         ;; Find counsel commands quickly
         ("<f6>" . (lambda ()
                     (interactive)
                     (counsel-M-x "^counsel ")))

         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)

         ;; Search at point
         ;; "M-j": word-at-point
         ;; "M-n"/"C-w": symbol-at-point
         ;; Refer to https://www.emacswiki.org/emacs/SearchAtPoint#toc8
         ;; and https://github.com/abo-abo/swiper/wiki/FAQ
         ;; ("C-w" . (lambda ()
         ;;            (interactive)
         ;;            (insert (format "%s" (with-ivy-window (ivy-thing-at-point))))))

         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)

         :map swiper-map
         ("M-%" . swiper-query-replace))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :config
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t)    ; Enable bookmarks and recentf
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function nil)
  (setq ivy-format-function 'ivy-format-function-arrow)
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
           "ag -i --noheading --nocolor --nofilename --numbers '%s' %s")
          (t counsel-grep-base-command))))
    (setq counsel-grep-base-command command))

  (when (executable-find "rg")
    (setq counsel-git-cmd "rg --files")
    (setq counsel-rg-base-command
          "rg -i -M 120 --no-heading --line-number --color never %s ."))

  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  ;; Enhance fuzzy matching
  (use-package flx)

  ;; Enhance M-x
  (use-package amx)

  ;; Additional key bindings for Ivy
  (use-package ivy-hydra
    :bind (:map ivy-minibuffer-map
                ("M-o" . ivy-dispatching-done-hydra)))

  ;; More friendly display transformer for Ivy
  (use-package ivy-rich
    :defines all-the-icons-mode-icon-alist
    :functions (all-the-icons-icon-family-for-mode all-the-icons-icon-family-for-file)
    :preface
    (with-eval-after-load 'all-the-icons
      (add-to-list 'all-the-icons-mode-icon-alist
                   '(gfm-mode  all-the-icons-octicon "markdown" :v-adjust 0.0 :face all-the-icons-lblue)))

    (defun ivy-rich-switch-buffer-icon (candidate)
      "Show buffer icons in `ivy-rich'."
      ;; Only on GUI
      (when (and centaur-ivy-icon
                 (display-graphic-p)
                 (featurep 'all-the-icons))
        (with-current-buffer (get-buffer candidate)
          (let ((icon (all-the-icons-icon-for-mode major-mode)))
            (propertize
             (if (symbolp icon)
                 (all-the-icons-icon-for-mode 'text-mode)
               icon)
             'face `(
                     :height 1.1
                     :family ,(all-the-icons-icon-family-for-mode
                               (if (symbolp icon)
                                   'text-mode
                                 major-mode))
                     :inherit
                     ))))))

    (defun ivy-rich-file-icon (candidate)
      "Show file icons in `ivy-rich'."
      ;; Only on GUI
      (when (and centaur-ivy-icon
                 (display-graphic-p)
                 (featurep 'all-the-icons))
        (let ((icon (all-the-icons-icon-for-file candidate)))
          (propertize
           (if (symbolp icon)
               (all-the-icons-icon-for-mode 'text-mode)
             icon)
           'face `(
                   :height 1.1
                   :family ,(all-the-icons-icon-family-for-file candidate)
                   :inherit
                   )))))

    (setq ivy-rich--display-transformers-list
          '(ivy-switch-buffer
            (:columns
             ((ivy-rich-switch-buffer-icon :width 2)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand)))
            ivy-switch-buffer-other-window
            (:columns
             ((ivy-rich-switch-buffer-icon :width 2)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand)))
            counsel-M-x
            (:columns
             ((counsel-M-x-transformer (:width 50))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
            counsel-describe-function
            (:columns
             ((counsel-describe-function-transformer (:width 50))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
            counsel-describe-variable
            (:columns
             ((counsel-describe-variable-transformer (:width 50))
              (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
            counsel-find-file
            (:columns
             ((ivy-rich-file-icon :width 2)
              (ivy-rich-candidate (:width 30))))
            counsel-file-jump
            (:columns
             ((ivy-rich-file-icon :width 2)
              (ivy-rich-candidate (:width 30))))
            counsel-git
            (:columns
             ((ivy-rich-file-icon :width 2)
              (ivy-rich-candidate (:width 30))))
            counsel-projectile-find-file
            (:columns
             ((ivy-rich-file-icon :width 2)
              (ivy-rich-candidate (:width 30))))
            counsel-projectile-find-dir
            (:columns
             ((ivy-rich-file-icon :width 2)
              (ivy-rich-candidate (:width 30))))
            counsel-recentf
            (:columns
             ((ivy-rich-file-icon :width 2)
              (ivy-rich-candidate (:width 90))
              (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))))
    :init (ivy-rich-mode 1)
    :hook (ivy-rich-mode . (lambda ()
                             (setq ivy-virtual-abbreviate
                                   (or (and ivy-rich-mode 'abbreviate) 'name)))))

  ;; Select from xref candidates with Ivy
  (use-package ivy-xref
    :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

  ;; Correcting words with flyspell via Ivy
  (use-package flyspell-correct-ivy
    :after flyspell
    :bind (:map flyspell-mode-map
                ([remap flyspell-correct-word-before-point] . flyspell-correct-previous-word-generic)))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :init (counsel-projectile-mode 1))

  ;; Display world clock using Ivy
  (use-package counsel-world-clock
    :bind (:map counsel-mode-map
                ("C-c c k" . counsel-world-clock)))

  ;; Tramp ivy interface
  (use-package counsel-tramp
    :bind (:map counsel-mode-map
                ("C-c c v" . counsel-tramp)))

  ;; Improve `counsel-ag', also impact `counsel-rg', `counsel-pt'.
  ;; search the selection or current symbol by default
  (eval-and-compile
    (declare-function ivy-thing-at-point "ivy")
    (defun my-counsel-ag(-counsel-ag &optional initial-input initial-directory extra-ag-args ag-prompt)
      "Search the selection or current symbol via `ag' by default."
      (unless initial-input
        (if (region-active-p)
            (setq initial-input (buffer-substring-no-properties
                                 (region-beginning) (region-end)))
          (setq initial-input (ivy-thing-at-point))))
      (unless initial-directory
        (setq initial-directory default-directory))
      (message "input: %s" initial-input)
      (funcall -counsel-ag initial-input initial-directory extra-ag-args ag-prompt))

    (advice-add 'counsel-ag :around #'my-counsel-ag))

  ;; Support pinyin in Ivy
  ;; Input prefix ':' to match pinyin
  ;; Refer to  https://github.com/abo-abo/swiper/issues/919 and
  ;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
  (use-package pinyinlib
    :functions ivy--regex-plus ivy--regex-ignore-order
    :commands pinyinlib-build-regexp-string
    :preface
    (defun re-builder-pinyin (str)
      "The regex builder wrapper to support pinyin."
      (or (pinyin-to-utf8 str)
          (ivy--regex-plus str)
          (ivy--regex-ignore-order str)))
    (defun my-pinyinlib-build-regexp-string (str)
      "Build a pinyin regexp sequence from STR."
      (cond ((equal str ".*")
             ".*")
            (t
             (pinyinlib-build-regexp-string str t))))
    (defun my-pinyin-regexp-helper (str)
      "Construct pinyin regexp for STR."
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
            (t
             nil)))
    :init (setq ivy-re-builders-alist
                '((read-file-name-internal . ivy--regex-fuzzy)
                  (t . re-builder-pinyin)))))

(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
