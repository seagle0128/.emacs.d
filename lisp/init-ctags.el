;; init-ctags.el --- Initialize TAGS configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2023 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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

;; Ctags IDE on the True Editor
;; @see https://github.com/universal-ctags/citre#quick-start
(use-package citre
  :diminish
  :commands citre-jump-back
  :functions xref-go-back
  :bind (:map prog-mode-map
         ("C-x c j" . citre-jump+)
         ("C-x c k" . citre-jump-back+)
         ("C-x c p" . citre-peek)
         ("C-x c a" . citre-ace-peek)
         ("C-x c u" . citre-update-this-tags-file))
  :init
  (setq citre-auto-enable-citre-mode-modes '(prog-mode)
        citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t)

  (defun citre-jump+ ()
    "Jump to the definition of the symbol at point.
Fallback to `xref-find-definitions'."
    (interactive)
    (condition-case _
        (citre-jump)
      (error (call-interactively #'xref-find-definitions))))

  (defun citre-jump-back+ ()
    "Go back to the position before last `citre-jump'.
Fallback to `xref-go-back'."
    (interactive)
    (condition-case _
        (citre-jump-back)
      (error (if (fboundp #'xref-go-back)
                 (call-interactively #'xref-go-back)
               (call-interactively #'xref-pop-marker-stack)))))
  :config
  (with-eval-after-load 'cc-mode (require 'citre-lang-c))
  (with-eval-after-load 'dired (require 'citre-lang-fileref))
  (with-eval-after-load 'verilog-mode (require 'citre-lang-verilog))

  (with-no-warnings
    ;; Use Citre xref backend as a fallback
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

    ;; Combine completions from Citre and lsp
    (defun lsp-citre-capf-function ()
      "A capf backend that tries lsp first, then Citre."
      (let ((lsp-result (cond
                         ((bound-and-true-p lsp-mode)
                          (and (fboundp #'lsp-completion-at-point)
                               (lsp-completion-at-point)))
                         ((bound-and-true-p eglot--managed-mode)
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

    (add-hook 'citre-mode-hook #'enable-lsp-citre-capf-backend)))

(provide 'init-ctags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ctags.el ends here
