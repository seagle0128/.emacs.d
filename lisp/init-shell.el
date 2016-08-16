;; init-shell.el --- Initialize shell configurations.
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
;;             Shell configurations.
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

;; Shell

(require 'init-const)

(use-package shell
  :defer t
  :config
  (progn
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    (add-hook 'shell-mode-hook 'n-shell-mode-hook)

    (defun n-shell-mode-hook ()
      "Shell mode customizations."
      (local-set-key '[up] 'comint-previous-input)
      (local-set-key '[down] 'comint-next-input)
      (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
      (setq comint-input-sender 'n-shell-simple-send))

    (defun n-shell-simple-send (proc command)
      "Various PROC COMMANDs pre-processing before sending to shell."
      (cond
       ;; Checking for clear command and execute it.
       ((string-match "^[ \t]*clear[ \t]*$" command)
        (comint-send-string proc "\n")
        (erase-buffer)
        )
       ;; Checking for man command and execute it.
       ((string-match "^[ \t]*man[ \t]*" command)
        (comint-send-string proc "\n")
        (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
        (setq command (replace-regexp-in-string "[ \t]+$" "" command))
        ;;(message (format "command %s command" command))
        (funcall 'man command)
        )
       ;; Send other commands to the default handler.
       (t (comint-simple-send proc command))
       ))
    ))

;; Do not use terminal on Windows
;; Term
(use-package term
  :defer t
  :if (not sys/win32p)
  :init
  (progn
    (setq system-uses-terminfo nil)

    ;; Disable yasnippet mode to enable TAB in term
    (eval-after-load 'yasnippet
      '(add-hook 'term-mode-hook '(lambda() (yas-minor-mode -1))))
    ))

;; Multi term
(use-package multi-term :defer t :if (not sys/win32p))

;; Shell Pop
(use-package shell-pop
  :defer t
  :bind ([f8] . shell-pop)
  :init
  (progn
    (if sys/win32p
        (setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
      (setq shell-pop-shell-type '("terminal" "*terminal*" (lambda () (term shell-pop-term-shell)))))
    ))

(provide 'init-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
