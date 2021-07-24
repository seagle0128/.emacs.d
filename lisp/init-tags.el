;; init-tags.el --- Initialize TAGS configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2021 Vincent Zhang

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
;; TAGS configurations.
;;

;;; Code:

(require 'init-const)
(require 'init-custom)

(when emacs/>=26p
  (use-package citre
    :diminish
    :bind (("C-x c j" . citre-jump+)
           ("C-x c k" . citre-jump-back)
           ("C-x c p" . citre-peek)
           ("C-x c a" . citre-ace-peek))
    :hook (prog-mode . citre-auto-enable-citre-mode)
    :init
    (defun citre-jump+ ()
      (interactive)
      (condition-case _
          (citre-jump)
        (error (call-interactively #'xref-find-definitions))))
    :config
    (with-no-warnings
      (with-eval-after-load 'cc-mode (require 'citre-lang-c))
      (with-eval-after-load 'dired (require 'citre-lang-fileref))
      (with-eval-after-load 'projectile
        (setq citre-project-root-function #'projectile-project-root))

      ;; Integrate with `lsp-mode' and `eglot'
      (define-advice xref--create-fetcher (:around (fn &rest args) fallback)
        (let ((fetcher (apply fn args))
              (citre-fetcher
               (let ((xref-backend-functions '(citre-xref-backend t)))
                 (ignore xref-backend-functions)
                 (apply fn args))))
          (lambda ()
            (or (with-demoted-errors "%s, fallback to citre"
                  (funcall fetcher))
                (funcall citre-fetcher)))))

      (defun lsp-citre-capf-function ()
        "A capf backend that tries lsp first, then Citre."
        (let ((lsp-result (pcase centaur-lsp
                            ('lsp-mode
                             (and (fboundp #'lsp-completion-at-point)
                                  (lsp-completion-at-point)))
                            ('eglot
                             (and (fboundp #'eglot-completion-at-point)
                                  (eglot-completion-at-point))))))
          (if (and lsp-result
                   (try-completion
                    (buffer-substring (nth 0 lsp-result)
                                      (nth 1 lsp-result))
                    (nth 2 lsp-result)))
              lsp-result
            (citre-completion-at-point))))

      (defun enable-lsp-citre-capf-backend ()
        "Enable the lsp + Citre capf backend in current buffer."
        (add-hook 'completion-at-point-functions #'lsp-citre-capf-function nil t))

      (add-hook 'citre-mode-hook #'enable-lsp-citre-capf-backend)

      (with-eval-after-load 'company
        (defun company-citre (-command &optional -arg &rest _ignored)
          "Completion backend of Citre.  Execute COMMAND with ARG and IGNORED."
          (interactive (list 'interactive))
          (cl-case -command
            (interactive (company-begin-backend 'company-citre))
            (prefix (and (bound-and-true-p citre-mode)
                         (or (citre-get-symbol) 'stop)))
            (meta (citre-get-property 'signature -arg))
            (annotation (citre-capf--get-annotation -arg))
            (candidates (all-completions -arg (citre-capf--get-collection -arg)))
            (ignore-case (not citre-completion-case-sensitive))))

        (setq company-backends '((company-capf company-citre :with company-yasnippet :separate)))))))

(provide 'init-tags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tags.el ends here
