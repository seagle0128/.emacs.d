;; init-window.el --- Initialize window configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 3.3.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Window configurations.
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

;; Directional window-selection routines
(use-package windmove
  :ensure nil
  :init (add-hook 'after-init-hook #'windmove-default-keybindings))

;; Restore old window configurations
(use-package winner
  :ensure nil
  :init
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*"))
  (add-hook 'after-init-hook #'winner-mode))

;; Quickly switch windows
(use-package ace-window
  :bind ([remap other-window] . ace-window))

;; Numbered window shortcuts
(use-package window-numbering
  :init (add-hook 'after-init-hook #'window-numbering-mode))

;; Popup Window Manager
(use-package popwin
  :commands popwin-mode
  :init (add-hook 'after-init-hook #'popwin-mode)
  :config
  (bind-key "C-z" popwin:keymap)

  ;; don't use default value but manage it ourselves
  (setq popwin:special-display-config
        '(;; Emacs
          ("*Help*" :dedicated t :position bottom :stick nil :noselect nil)
          ("*compilation*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
          ("*Compile-Log*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
          ("*Warnings*" :dedicated t :position bottom :stick t :noselect t)
          ("*Completions*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil)
          ("\*Async Shell Command\*.+" :regexp t :position bottom :stick t :noselect nil)
          ("^*Man.+*$" :regexp t :position bottom :stick nil :noselect nil :height 0.4)
          ("^*WoMan.+*$" :regexp t :position bottom)
          ("^*Backtrace.+*$" :regexp t :dedicated t :position bottom :stick t :noselect nil)
          ("^*helpful .+*$" :regexp t :position bottom :stick nil :noselect nil :height 0.4)

          ;; Kill Ring
          ("*Kill Ring*" :dedicated t :position bottom)

          ;; Flycheck
          ("\*flycheck errors\*.+*$" :regexp t :position bottom :stick t :noselect nil)

          ;; Youdao dict
          ("*Youdao Dictionary*" :dedicated t :position bottom)

          ;; Paradox
          ("*Paradox Report*" :dedicated t :position bottom :noselect nil)

          ;; List
          ("*Colors*" :dedicated t :position bottom)
          ("*Process List*" :dedicated t :position bottom)
          ("*Process-Environment*" :dedicated t :position bottom)

          ;; undo-tree
          (" *undo-tree*" :dedicated t :position right :stick t :noselect nil :width 60)

          ;; Search
          ("*grep*" :dedicated t :position bottom :stick t :noselect nil)
          ("*ag search*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*rg*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*pt-search*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*Occur*" :dedicated t :position bottom :stick t :noselect nil)
          ("\*ivy-occur.+*$" :regexp t :position bottom :stick t :noselect nil)
          ("*xref*" :dedicated t :position bottom :stick nil :noselect nil)

          ;; VC
          ("*vc-diff*" :dedicated t :position bottom :stick t :noselect nil)
          ("*vc-change-log*" :dedicated t :position bottom :stick t :noselect nil)

          ;; Magit
          ;; (magit-status-mode :dedicated t :position bottom :stick t :height 0.5)
          ;; (magit-diff-mode :dedicated t :position bottom :stick t :noselect t :height 0.5)

          ;; Script
          ("*shell*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Python*" :dedicated t :position bottom :stick t :noselect t)
          ("*Ruby*" :dedicated t :position bottom :stick t :noselect t)
          ("*quickrun*" :dedicated t :position bottom :stick t :noselect t)

          ;; Go
          ("^*godoc.+*$" :regexp t :position bottom :stick nil :noselect nil)
          ("*golint*" :dedicated t :position bottom :stick t :noselect nil)
          ("*govet*" :dedicated t :position bottom :stick t :noselect nil)
          ("*go-guru-output*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Gofmt Errors*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Go Test*" :dedicated t :position bottom :stick t :noselect nil)

          ;; Test
          ("*ert*" :dedicated t :position bottom :stick t :noselect nil)
          ("*nosetests*" :dedicated t :position bottom :stick t :noselect nil))))

;; Easy window config switching
(use-package eyebrowse
  :init (add-hook 'after-init-hook #'eyebrowse-mode))

(provide 'init-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-window.el ends here
