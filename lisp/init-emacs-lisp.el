;; init-emacs-lisp.el --- Initialize Emacs Lisp configurations.
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

;; Emacs lisp mode
(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

;; Enable Eldoc in lisp modes
(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
    ))

;; Byte compiler
(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (setq use-package-always-ensure nil)
  (byte-recompile-file user-init-file 0 0)
  (byte-recompile-directory (expand-file-name "lisp" user-emacs-directory) 0)
  (byte-recompile-directory (expand-file-name "site-lisp" user-emacs-directory) 0)
  (setq use-package-always-ensure t))

(add-hook 'kill-emacs-hook 'byte-compile-init-dir)

(defun recompile-el-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (byte-recompile-file buffer-file-name 0 0))
            nil
            t))

;; (add-hook 'emacs-lisp-mode-hook 'recompile-el-on-save)

(provide 'init-emacs-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-emacs-lisp.el ends here
