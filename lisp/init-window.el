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
  (aw-leading-char-face ((t (:inherit 'font-lock-keyword-face :height 2.0))))
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

;; A *visual* way to switch window
(use-package switch-window
  :disabled
  :bind (([remap other-window] . switch-window)
         ([remap dired-other-window] . switch-window-then-dired)
         ([remap find-file-other-window] . switch-window-then-find-file)
         ([remap find-file-read-only-other-window] . switch-window-then-find-file-read-only)
         ([remap display-buffer] . switch-window-then-display-buffer)
         ([remap kill-buffer-and-window] . switch-window-then-kill-buffer)
         ("C-x 4 s" . switch-window-then-swap-buffer)
         :map switch-window-extra-map
         ("u" . winner-undo)
         ("r" . winner-redo))
  :custom-face
  (switch-window-label ((t (:inherit font-lock-keyword-face :height 3.0))))
  :config
  ;; (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-minibuffer-shortcut ?0)
  (setq switch-window-multiple-frames t)
  (with-eval-after-load 'ivy
    (setq switch-window-preferred 'ivy)))

;; Navigate windows and frames using numbers
(use-package winum
  :disabled
  :bind (:map winum-keymap
              ("C-`" . winum-select-window-by-number)
              ("C-²" . winum-select-window-by-number)
              ("M-0" . winum-select-window-0-or-10)
              ("M-1" . winum-select-window-1)
              ("M-2" . winum-select-window-2)
              ("M-3" . winum-select-window-3)
              ("M-4" . winum-select-window-4)
              ("M-5" . winum-select-window-5)
              ("M-6" . winum-select-window-6)
              ("M-7" . winum-select-window-7)
              ("M-8" . winum-select-window-8)
              ("M-9" . winum-select-window-9))
  :hook (after-init . winum-mode)
  :config
  (setq winum-auto-setup-mode-line nil)
  (add-to-list 'winum-assign-functions
               (lambda ()
                 (cond
                  ((equal (buffer-name) " *Treemacs-Framebuffer-1*") 9)
                  ((equal (buffer-name) "*Flycheck errors*") 8)))))

;; Enforce rules for popups
(use-package shackle
  :hook (after-init . shackle-mode)
  :config
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
          (loop for (window . buffer) in shackle--popup-window-list
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
  (setq shackle-default-size 0.4)
  (setq shackle-rules
        '(("*Help*" :select t :autoclose t :align 'below)
          ("*compilation*" :size 0.25 :align 'below :autoclose t)
          ("*Completions*" :size 0.3 :align 'below :autoclose t)
          ("*Pp Eval Output*" :size 0.25 :align 'below :autoclose t)
          ("*ert*" :same t)
          ("*info*" :size 0.5 :select t :popup t)
          ("*Backtrace*" :size 20 :align 'below)
          ("*Warnings*" :size 12 :align 'below)
          ("*Messages*" :size 12 :align 'below)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :other t)
          ("*Calendar*" :select t :size 0.3 :align 'below)
          (" *undo-tree*" :select t :popup t)
          (apropos-mode :size 0.3 :align 'below :autoclose t)
          (Buffer-menu-mode :size 20 :align 'below :autoclose t)
          (comint-mode :noesc t :align 'below)
          (grep-mode :size 25 :align 'below :autoclose t)
          (profiler-report-mode :popup t)
          (tabulated-list-mode :noesc t :align 'below)
          ("^ ?\\*" :regexp t :select t :align 'below :autoclose t))))

;; Easy window config switching
(use-package eyebrowse
  :hook (after-init . eyebrowse-mode))

(provide 'init-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-window.el ends here
