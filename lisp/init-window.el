;; init-window.el --- Initialize window configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Vincent Zhang

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
;; Window configurations.
;;

;;; Code:

;; Directional window-selection routines
(use-package windmove
  :ensure nil
  :hook (after-init . windmove-default-keybindings))

;; Restore old window configurations
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

;; Quickly switch windows
(use-package ace-window
  :bind ([remap other-window] . ace-window)
  :custom-face
  (aw-leading-char-face ((t (:inherit 'error :bold t :height 1.2))))
  (aw-mode-line-face ((t (:inherit 'mode-line-emphasis :bold t))))
  :hook (after-init . ace-window-display-mode)
  :config
  ;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  (when (package-installed-p 'hydra)
    ;; https://github.com/abo-abo/ace-window/wiki/Hydra
    ;; hydra-frame-window is designed from ace-window (C-x o w) and
    ;; matches aw-dispatch-alist with a few extra
    (defhydra hydra-frame-window (:color red :hint nil)
      "
^Delete^                       ^Frame resize^             ^Window^                Window Size^^^^^^   ^Text^                         (__)
_0_: delete-frame              _g_: resize-frame-right    _t_: toggle               ^ ^ _k_ ^ ^        _K_                           (oo)
_1_: delete-other-frames       _H_: resize-frame-left     _e_: ace-swap-win         _h_ ^+^ _l_        ^+^                     /------\\/
_2_: make-frame                _F_: fullscreen            ^ ^                       ^ ^ _j_ ^ ^        _J_                    / |    ||
_d_: kill-and-delete-frame     _n_: make-frame            _w_: ace-delete-window    _b_alance^^^^      ^ ^                 *  /\\---/\\  ~~  C-x o w ;
"
      ("0" delete-frame :exit t)
      ("1" delete-other-frames :exit t)
      ("2" make-frame  :exit t)
      ("b" balance-windows)
      ("d" kill-and-delete-frame :exit t)
      ("e" ace-swap-window)
      ("F" toggle-frame-fullscreen)   ;; is <f11>
      ("g" resize-frame-right :exit t)
      ("H" resize-frame-left :exit t)  ;; aw-dispatch-alist uses h, I rebind here so hjkl can be used for size
      ("n" make-frame :exit t)
      ;; ("r" reverse-windows)
      ("t" toggle-window-spilt)
      ("w" ace-delete-window :exit t)
      ("x" delete-frame :exit t)
      ("K" text-scale-decrease)
      ("J" text-scale-increase)
      ("h" shrink-window-horizontally)
      ("k" shrink-window)
      ("j" enlarge-window)
      ("l" enlarge-window-horizontally)
      ("q" nil "quit"))
    (add-to-list 'aw-dispatch-alist '(?w hydra-frame-window/body) t)))

;; Enforce rules for popups
(use-package shackle
  :hook (after-init . shackle-mode)
  :config
  (defun view-last-popup-buffer ()
    "View last popup buffer."
    (interactive)
    (ignore-errors
      (display-buffer shackle-last-buffer)))
  (bind-key "C-h z" #'view-last-popup-buffer)

  ;; Add keyword: `autoclose'
  (defvar shackle--popup-window-list nil) ; all popup windows
  (defvar-local shackle--current-popup-window nil) ; current popup window
  (put 'shackle--current-popup-window 'permanent-local t)

  (defun shackle-display-buffer-hack (fn buffer alist plist)
    (let ((window (funcall fn buffer alist plist)))
      (setq shackle--current-popup-window window)

      (when (plist-get plist :autoclose)
        (push (cons window buffer) shackle--popup-window-list))
      window))

  (defun shackle-close-popup-window-hack (&rest _)
    "Close current popup window via `C-g'."
    (setq shackle--popup-window-list
          (cl-loop for (window . buffer) in shackle--popup-window-list
                   if (and (window-live-p window)
                           (equal (window-buffer window) buffer))
                   collect (cons window buffer)))
    ;; `C-g' can deactivate region
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p)))
      (let (window buffer)
        (if (one-window-p)
            (progn
              (setq window (selected-window))
              (when (equal (buffer-local-value 'shackle--current-popup-window
                                               (window-buffer window))
                           window)
                (winner-undo)))
          (setq window (caar shackle--popup-window-list))
          (setq buffer (cdar shackle--popup-window-list))
          (when (and (window-live-p window)
                     (equal (window-buffer window) buffer))
            (delete-window window)

            (pop shackle--popup-window-list))))))

  (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
  (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack)

  ;; rules
  (setq shackle-default-size 0.3)
  (setq shackle-rules
        '(("*Help*" :select t :align 'below :autoclose t)
          ("*compilation*" :size 0.25 :align 'below :autoclose t)
          ("*Completions*" :size 0.3 :align 'below :autoclose t)
          ("*Pp Eval Output*" :size 0.25 :align 'below :autoclose t)
          ("*ert*" :align 'below :autoclose t)
          ("*Ibuffer*" :select t :inhibit-window-quit t :same t)
          ("*info*" :select t :inhibit-window-quit t :same t)
          ("*Backtrace*" :select t :size 20 :align 'below)
          ("*Warnings*" :size 12 :align 'below :autoclose t)
          ("*Messages*" :size 12 :align 'below :autoclose t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :other t :inhibit-window-quit t)
          ("*Calendar*" :select t :size 0.3 :align 'below)
          (" *undo-tree*" :select t)
          (apropos-mode :size 0.3 :align 'below :autoclose t)
          (Buffer-menu-mode :size 20 :align 'below :autoclose t)
          (comint-mode :align 'below)
          (grep-mode :size 25 :align 'below :autoclose t)
          (profiler-report-mode :popup t)
          (tabulated-list-mode :align 'below)
          ("^ ?\\*" :regexp t :select t :align 'below :autoclose t))))

(provide 'init-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-window.el ends here
