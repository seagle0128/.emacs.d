;; init-shell.el --- Initialize shell configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2021 Vincent Zhang

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
;; Shell configurations.
;;

;;; Code:

(require 'init-const)
(require 'init-funcs)

(use-package shell
  :ensure nil
  :hook ((shell-mode . my-shell-mode-hook)
         (comint-output-filter-functions . comint-strip-ctrl-m))
  :init
  (setq system-uses-terminfo nil)

  (with-no-warnings
    (defun my-shell-simple-send (proc command)
      "Various PROC COMMANDs pre-processing before sending to shell."
      (cond
       ;; Checking for clear command and execute it.
       ((string-match "^[ \t]*clear[ \t]*$" command)
        (comint-send-string proc "\n")
        (erase-buffer))
       ;; Checking for man command and execute it.
       ((string-match "^[ \t]*man[ \t]*" command)
        (comint-send-string proc "\n")
        (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
        (setq command (replace-regexp-in-string "[ \t]+$" "" command))
        ;;(message (format "command %s command" command))
        (funcall 'man command))
       ;; Send other commands to the default handler.
       (t (comint-simple-send proc command))))

    (defun my-shell-mode-hook ()
      "Shell mode customization."
      (local-set-key '[up] 'comint-previous-input)
      (local-set-key '[down] 'comint-next-input)
      (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)

      (ansi-color-for-comint-mode-on)
      (setq comint-input-sender 'my-shell-simple-send))))

;; ANSI & XTERM 256 color support
(use-package xterm-color
  :defines (compilation-environment
            eshell-preoutput-filter-functions
            eshell-output-filter-functions)
  :functions (compilation-filter my-advice-compilation-filter)
  :init
  ;; For shell and interpreters
  (setenv "TERM" "xterm-256color")
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Disable font-locking to improve performance
              (font-lock-mode -1)
              ;; Prevent font-locking from being re-enabled
              (make-local-variable 'font-lock-function)
              (setq font-lock-function #'ignore)))

  ;; For eshell
  (with-eval-after-load 'esh-mode
    (add-hook 'eshell-before-prompt-hook
              (lambda ()
                (setq xterm-color-preserve-properties t)))
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

  ;; For compilation buffers
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my-advice-compilation-filter (f proc string)
    (funcall f proc
             (if (eq major-mode 'rg-mode) ; compatible with `rg'
                 string
               (xterm-color-filter string))))
  (advice-add 'compilation-filter :around #'my-advice-compilation-filter)
  (advice-add 'gud-filter :around #'my-advice-compilation-filter))

;; Better terminal emulator
;; @see https://github.com/akermu/emacs-libvterm#installation
(when (and module-file-suffix           ; dynamic module
           (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make"))
  (use-package vterm
    :commands vterm--internal
    :bind (:map vterm-mode-map
           ([f9] . (lambda ()
                     (interactive)
                     (and (fboundp 'shell-pop)
                          (shell-pop nil)))))
    :init
    (setq vterm-always-compile-module t)

    (with-no-warnings
      (when (childframe-workable-p)
        (defvar vterm-posframe--frame nil)

        (defun vterm-posframe-hidehandler (_)
          "Hidehandler used by `vterm-posframe-toggle'."
          (not (eq (selected-frame) posframe--frame)))

        (defun vterm-posframe-toggle ()
          "Toggle `vterm' child frame."
          (interactive)
          (let ((buffer (vterm--internal #'ignore 100)))
            (if (and vterm-posframe--frame
                     (frame-live-p vterm-posframe--frame)
                     (frame-visible-p vterm-posframe--frame))
                (progn
                  (posframe-hide buffer)
                  ;; Focus the parent frame
                  (select-frame-set-input-focus (frame-parent vterm-posframe--frame)))
              (let ((width  (max 80 (/ (frame-width) 2)))
                    (height (/ (frame-height) 2)))
                (setq vterm-posframe--frame
                      (posframe-show
                       buffer
                       :poshandler #'posframe-poshandler-frame-center
                       :hidehandler #'vterm-posframe-hidehandler
                       :left-fringe 8
                       :right-fringe 8
                       :width width
                       :height height
                       :min-width width
                       :min-height height
                       :internal-border-width 3
                       :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                       :background-color (face-background 'tooltip nil t)
                       :override-parameters '((cursor-type . t))
                       :accept-focus t))
                ;; Blink cursor
                (with-current-buffer buffer
                  (save-excursion
                    (vterm-clear t))
                  (setq-local cursor-type 'box))
                ;; Focus the child frame
                (select-frame-set-input-focus vterm-posframe--frame)))))
        (bind-key "C-`" #'vterm-posframe-toggle)))))

;; Shell Pop
(use-package shell-pop
  :bind (("C-`" . (lambda ()
                    (interactive)
                    (if (fboundp 'vterm-posframe-toggle)
                        (vterm-posframe-toggle)
                      (shell-pop nil))))
         ([f9] . shell-pop))
  :init
  (setq shell-pop-window-size 30
        shell-pop-shell-type
        (cond ((fboundp 'vterm) '("vterm" "*vterm*" #'vterm))
              (sys/win32p '("eshell" "*eshell*" #'eshell))
              (t '("terminal" "*terminal*"
                   (lambda () (term shell-pop-term-shell)))))))

(provide 'init-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
