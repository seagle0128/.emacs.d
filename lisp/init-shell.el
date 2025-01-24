;; init-shell.el --- Initialize shell configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2024 Vincent Zhang

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
;; Shell configurations.
;;

;;; Code:

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
           (executable-find "libtool")  ; libtool-bin
           (executable-find "make"))
  (use-package vterm
    :bind (:map vterm-mode-map
           ([f9] . (lambda ()
                     (interactive)
                     (and (fboundp 'shell-pop-toggle)
                          (shell-pop-toggle)))))
    :init (setq vterm-always-compile-module t))

  (use-package multi-vterm
    :bind ("C-<f9>" . multi-vterm)
    :custom (multi-vterm-buffer-name "vterm")
    :config
    (with-no-warnings
      ;; Use `pop-to-buffer' instead of `switch-to-buffer'
      (defun my-multi-vterm ()
        "Create new vterm buffer."
        (interactive)
        (let ((vterm-buffer (multi-vterm-get-buffer)))
          (setq multi-vterm-buffer-list
                (nconc multi-vterm-buffer-list (list vterm-buffer)))
          (set-buffer vterm-buffer)
          (multi-vterm-internal)
          (pop-to-buffer vterm-buffer)))
      (advice-add #'multi-vterm :override #'my-multi-vterm))))

;; Shell Pop: leverage `popper'
(with-no-warnings
  (defvar shell-pop--frame nil)
  (defvar shell-pop--window nil)

  (defun shell-pop--shell (&optional arg)
    "Run shell and return the buffer."
    (cond ((fboundp 'vterm) (vterm arg))
          (sys/win32p (eshell arg))
          (t (shell))))

  (defun shell-pop--hide-frame ()
    "Hide child frame and refocus in parent frame."
    (when (and (childframe-workable-p)
               (frame-live-p shell-pop--frame)
               (frame-visible-p shell-pop--frame))
      (make-frame-invisible shell-pop--frame)
      (select-frame-set-input-focus (frame-parent shell-pop--frame))
      (setq shell-pop--frame nil)))

  (defun shell-pop-toggle ()
    "Toggle shell."
    (interactive)
    (shell-pop--hide-frame)
    (if (window-live-p shell-pop--window)
        (progn
          (delete-window shell-pop--window)
          (setq shell-pop--window nil))
      (setq shell-pop--window
            (get-buffer-window (shell-pop--shell)))))
  (bind-keys ([f9]  . shell-pop-toggle)
             ("C-`" . shell-pop-toggle))

  (when (childframe-workable-p)
    (defun shell-pop-posframe-hidehandler (_)
      "Hidehandler used by `shell-pop-posframe-toggle'."
      (not (eq (selected-frame) shell-pop--frame)))

    (defun shell-pop-posframe-toggle ()
      "Toggle shell in child frame."
      (interactive)
      (let* ((buffer (shell-pop--shell))
             (window (get-buffer-window buffer)))
        ;; Hide window: for `popper'
        (when (window-live-p window)
          (delete-window window))

        (if (and (frame-live-p shell-pop--frame)
                 (frame-visible-p shell-pop--frame))
            (progn
              ;; Hide child frame and refocus in parent frame
              (make-frame-invisible shell-pop--frame)
              (select-frame-set-input-focus (frame-parent shell-pop--frame))
              (setq shell-pop--frame nil))
          (let ((width  (max 100 (round (* (frame-width) 0.62))))
                (height (round (* (frame-height) 0.62))))
            ;; Shell pop in child frame
            (setq shell-pop--frame
                  (posframe-show
                   buffer
                   :poshandler #'posframe-poshandler-frame-center
                   :hidehandler #'shell-pop-posframe-hidehandler
                   :left-fringe 8
                   :right-fringe 8
                   :width width
                   :height height
                   :min-width width
                   :min-height height
                   :internal-border-width 3
                   :internal-border-color (face-background 'region nil t)
                   :background-color (face-background 'tooltip nil t)
                   :override-parameters '((cursor-type . t))
                   :respect-mode-line t
                   :accept-focus t))

            ;; Focus in child frame
            (select-frame-set-input-focus shell-pop--frame)

            (with-current-buffer buffer
              (setq-local cursor-type 'box) ; blink cursor
              (goto-char (point-max))
              (when (fboundp 'vterm-reset-cursor-point)
                (vterm-reset-cursor-point)))))))
    (bind-key "C-`" #'shell-pop-posframe-toggle)))

(provide 'init-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
