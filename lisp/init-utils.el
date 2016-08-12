;; init-utils.el --- Initialize basic configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 2.0.0
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

;; Dos2Unix
(defun dos2unix ()
  "Not exactly but it's easier to remember."
  (interactive)
  (set-buffer-file-coding-system 'unix 't) )

;; Revert buffer
(global-set-key [(f5)] '(lambda ()
                          (interactive)
                          (message "Revert this buffer.")
                          (revert-buffer t t)))

;; Describe function
(defun my-describe-symbol (symbol)
  "Display the full documentation of SYMBOL (function and variable) in tooltip."
  (interactive (list (symbol-at-point)))
  (let ((x-gtk-use-system-tooltips nil))
    (if (null symbol)
        (pos-tip-show
         "** You didn't specify a symbol! **" '("red"))
      (pos-tip-show
       (with-temp-buffer
         (let ((standard-output (current-buffer))
               (help-xref-following t))
           (help-mode)
           (read-only-mode -1)
           (prin1 symbol)
           (princ " is ")
           (save-window-excursion
             (if (fboundp symbol)
                 (describe-function symbol)
               (describe-variable symbol)))
           (read-only-mode -1)
           (buffer-string)))))))

(define-key emacs-lisp-mode-map (kbd "C-`") 'my-describe-symbol)

;; Text zoom in/out
(global-set-key [(C-wheel-up)] 'text-scale-increase)
(global-set-key [(C-wheel-down)] 'text-scale-decrease)

;; Which key
(use-package which-key
  :defer t
  :diminish which-key-mode
  :init (add-hook 'after-init-hook 'which-key-mode))

;; Browse url
(use-package browse-url-dwim
  :defer t
  :init (add-hook 'after-init-hook 'browse-url-dwim-mode))

;; Tramp
(use-package tramp
  :defer t
  :init (let ((val (if (executable-find "plink") "plink" "ssh")))
          (setq tramp-default-method val)))

;; Tree explorer
(use-package neotree
  :defer t
  :bind (([f12] . neotree-toggle)
         ([C-f12] . neotree-toggle))
  :init (add-hook 'neotree-mode-hook '(lambda () (linum-mode -1))))

;; Dash
(use-package dash-at-point
  :defer t
  :if sys/macp
  :bind (("\C-cd" . dash-at-point)
         ("\C-ce" . dash-at-point-with-docset)))

;; Youdao Dictionay
(use-package youdao-dictionary
  :defer t
  :commands youdao-dictionary--region-or-word youdao-dictionary--format-result
  :bind ("C-c y" . youdao-dictionary-search-at-point+)
  :config
  (progn
    ;; Cache documents
    (setq url-automatic-caching t)

    ;; Use pos-tip instead of popup to display results
    (if (display-graphic-p)
        (eval-after-load 'pos-tip
          '(defun youdao-dictionary-search-at-point+ ()
             "Search word at point and display results with pos-tip."
             (interactive)
             (let ((word (youdao-dictionary--region-or-word))
                   (x-gtk-use-system-tooltips t))
               (if word
                   (pos-tip-show (youdao-dictionary--format-result word)
                                 nil nil nil 0)
                 (message "Nothing to look up"))))))
    ))

;; Search
(use-package ack :defer t :if (executable-find "ack"))
(use-package ag :defer t :if (executable-find "ag"))

;; Junk file
(use-package open-junk-file
  :defer t
  :commands open-junk-file)

;; Misc
(use-package htmlize :defer t)
(use-package list-environment :defer t)
(use-package memory-usage :defer t)

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
