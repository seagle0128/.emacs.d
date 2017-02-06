;; init-emacs-lisp.el --- Initialize Emacs Lisp configurations.
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
;;             Emacs Lisp configurations.
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

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(when (boundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '("<=" . ?≤) prettify-symbols-alist))))

;; Emacs lisp mode
(bind-key "C-c C-z" 'ielm emacs-lisp-mode-map)
(bind-key "C-c C-c" 'eval-defun emacs-lisp-mode-map)
(bind-key "C-c C-b" 'eval-buffer emacs-lisp-mode-map)

;; Enable Eldoc in lisp modes
(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :init (dolist (hook '(emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook))
          (add-hook hook 'eldoc-mode)))

;; Make M-. and M-, work in elisp like they do in slime.
;; In Emacs 25, xref is perfect, so only enable in <=24.
(use-package elisp-slime-nav
  :defer t
  :if (< emacs-major-version 25)
  :diminish elisp-slime-nav-mode
  :init (dolist (hook '(emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook))
          (add-hook hook 'elisp-slime-nav-mode)))

;; Interactive macro expander
(use-package macrostep
  :defer t
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)
              :map lisp-interaction-mode-map
              ("C-c e" . macrostep-expand)))

;; Byte compiler
(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (setq package-selected-packages nil)  ; Fix Emacs 25
  (setq use-package-always-ensure nil)  ; Don't install unnedeeded packages.
  (byte-recompile-file user-init-file 0 0)
  (byte-recompile-directory (expand-file-name "lisp" user-emacs-directory) 0)
  (byte-recompile-directory (expand-file-name "site-lisp" user-emacs-directory) 0)
  (setq use-package-always-ensure t))

(add-hook 'kill-emacs-hook 'byte-compile-init-dir)

(defun recompile-el-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              "Only byte-compile files in init dirs."
              (when (string-match "\\.*\.emacs\.d\/\\(lisp\\|site-lisp\\)\/.*\\el$" buffer-file-name)
                (setq package-selected-packages nil)  ; Fix Emacs 25
                (setq use-package-always-ensure nil)  ; Don't install unnedeeded packages.
                (byte-recompile-file buffer-file-name 0 0)
                (setq use-package-always-ensure t)))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook 'recompile-el-on-save)

;; Only on X
(when (display-graphic-p)
  ;; Describe symbol at point
  (defun my-describe-symbol-at-point (symbol)
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
             (buffer-string)))
         nil nil nil 0))))

  (bind-key "<f1>" 'my-describe-symbol-at-point emacs-lisp-mode-map)
  (bind-key "<f1>" 'my-describe-symbol-at-point lisp-interaction-mode-map)
  )

(provide 'init-emacs-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-emacs-lisp.el ends here
