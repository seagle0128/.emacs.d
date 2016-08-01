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

;; Do not use term on Windows
(when (not sys/win32p)
  (defvar term-program shell-file-name)
  (cond ((executable-find "fish")
         (setq term-program "fish"))
        ((executable-find "zsh")
         (setq term-program "zsh")))

  (setenv "SHELL" term-program))

;; Multi term
(use-package multi-term
  :defer t
  :if (not sys/win32p)
  :config
  (progn
    (setq system-uses-terminfo nil)

    (setq multi-term-program term-program)
    ;; Disable yasnippet mode to enable TAB in term
    (add-hook 'term-mode-hook '(lambda() (yas-minor-mode -1)))
    ))

;; Shell Pop
(use-package shell-pop
  :defer t
  :if (not sys/win32p)
  :bind ([f8] . shell-pop)
  :config
  (progn
    (setq shell-pop-shell-type
          '("ansi-term" "*ansi-term*"
            (lambda nil (ansi-term shell-pop-term-shell))))
    (setq shell-pop-term-shell term-program)
    ;; (setq shell-pop-universal-key "C-t")
    (setq shell-pop-window-size 30)
    (setq shell-pop-full-span t)
    (setq shell-pop-window-position "bottom")
    ))

(provide 'init-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
