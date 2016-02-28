;; init-utils.el --- Initialize basic configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 1.0.0
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Some basic configurations.
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

;; Elisp byte compiler
(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-file user-init-file 0 0)
  (byte-recompile-directory (concat user-emacs-directory "lisp") 0))

(add-hook 'after-init-hook 'byte-compile-init-dir)

(defun recompile-el-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (byte-recompile-file buffer-file-name 0 0))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook 'recompile-el-on-save)

;; ibuffer
;; (global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Dired" (mode . dired-mode))
         ("Emacs Lisp" (mode . emacs-lisp-mode))
         ("C" (mode . c-mode))
         ("C++" (mode . c++-mode))
         ("Org" (mode . org-mode))
         ("Python" (mode . python-mode))
         ("Ruby" (mode . ruby-mode))
         ("Helm" (predicate string-match "Helm" mode-name))
         ("Earmuffs" (name . "^\\*.*?\\*$")))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Which key
(which-key-mode 1)

;; Browse url
(browse-url-dwim-mode 1)

;; Tramp
(cond
 ((eq system-type 'windows-nt)
  (setq tramp-default-method "plink"
        tramp-password-end-of-line "\r\n"))
 ((eq system-type 'gnu/linux)
  (setq tramp-default-method "ssh")))

;; Dos2Unix
(defun dos2unix ()
  "Automate \\<keymap> & \\[function]." ; M-% C-q C-m RET C-q C-j RET
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t))))

;; Tree explorer
(global-set-key [f12] 'neotree-toggle)
(add-hook 'neotree-mode-hook
          '(lambda ()
             (linum-mode -1)))

;; Revert buffer
(global-set-key [(f5)] '(lambda ()
                          (interactive)
                          (message "Revert this buffer.")
                          (revert-buffer t t)))

;; Dash
(when sys/macp
  (global-set-key "\C-cd" 'dash-at-point)
  (global-set-key "\C-ce" 'dash-at-point-with-docset))

;; Youdao Dict
(setq url-automatic-caching t)
(global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point)

;; Powerline
(powerline-center-theme)

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
