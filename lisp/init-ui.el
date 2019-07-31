;; init-ui.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Vincent Zhang

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
;; Visual (UI) configurations for better lookings and appearances.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Suppress warnings in hydra
(declare-function centaur-compatible-theme-p 'init-funcs)
(declare-function centaur-load-theme 'init-funcs)

;; Logo
(setq fancy-splash-image centaur-logo)

;; Title
(setq frame-title-format '("Centaur Emacs - %b")
      icon-title-format frame-title-format)

(when sys/mac-x-p
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))

;; Menu/Tool/Scroll bars
(unless emacs/>=27p        ; Move to early init-file in 27
  (unless sys/mac-x-p
    (push '(menu-bar-lines . 0) default-frame-alist))
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist))

;; Theme
(if (centaur-compatible-theme-p centaur-theme)
    (progn
      (use-package doom-themes
        :init (centaur-load-theme centaur-theme)
        :config
        ;; Enable flashing mode-line on errors
        (doom-themes-visual-bell-config)
        (set-face-attribute 'doom-visual-bell nil
                            :background (face-foreground 'error)
                            :foreground (face-background 'default)
                            :inverse-video nil)
        ;; Corrects (and improves) org-mode's native fontification.
        (doom-themes-org-config)
        ;; Enable custom treemacs theme (all-the-icons must be installed!)
        (doom-themes-treemacs-config)

        ;; Colorful icon theme
	    (with-eval-after-load 'treemacs
          (with-no-warnings
            (when (require 'all-the-icons nil t)
              (treemacs-create-theme
               "centaur"
               :config
               (let ((face-spec '(:inherit font-lock-doc-face :slant normal)))
                 (treemacs-create-icon
                  :icon (format " %s\t" (all-the-icons-octicon "repo" :v-adjust -0.1 :face face-spec))
                  :extensions (root))
                 (treemacs-create-icon
                  :icon (format "%s\t%s\t"
                                (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1)
                                (all-the-icons-octicon "file-directory" :v-adjust 0))
                  :extensions (dir-open))
                 (treemacs-create-icon
                  :icon (format "%s\t%s\t"
                                (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face face-spec)
                                (all-the-icons-octicon "file-directory" :v-adjust 0 :face face-spec))
                  :extensions (dir-closed))
                 (treemacs-create-icon
                  :icon (format "  %s\t%s\t"
                                (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1)
                                (all-the-icons-octicon "package" :v-adjust 0)) :extensions (tag-open))
                 (treemacs-create-icon
                  :icon (format "  %s\t%s\t"
                                (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face face-spec)
                                (all-the-icons-octicon "package" :v-adjust 0 :face face-spec))
                  :extensions (tag-closed))
                 (treemacs-create-icon
                  :icon (format "  %s\t" (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0 :face face-spec))
                  :extensions (tag-leaf))
                 (treemacs-create-icon
                  :icon (format "  %s\t" (all-the-icons-octicon "flame" :v-adjust 0 :face face-spec))
                  :extensions (error))
                 (treemacs-create-icon
                  :icon (format "  %s\t" (all-the-icons-octicon "stop" :v-adjust 0 :face face-spec))
                  :extensions (warning))
                 (treemacs-create-icon
                  :icon (format "  %s\t" (all-the-icons-octicon "info" :height 0.75 :v-adjust 0.1 :face face-spec))
                  :extensions (info))
                 (treemacs-create-icon
                  :icon (format "  %s\t" (all-the-icons-octicon "file-binary" :v-adjust 0 :face face-spec))
                  :extensions ("exe" "obj" "so" "o" "out"))
                 (treemacs-create-icon
                  :icon (format "  %s\t" (all-the-icons-octicon "file-zip" :v-adjust 0 :face 'all-the-icons-lmaroon))
                  :extensions ("tar" "rar" "tgz"))
                 (treemacs-create-icon
                  :icon (format "  %s\t" (all-the-icons-faicon "file-o" :height 0.8 :v-adjust 0 :face 'all-the-icons-dsilver))
                  :extensions (fallback))

                 (defun get-extenstions (ext)
                   "Transfer EXT regexps to extension list."
                   (let* ((e (s-replace-all
                              '((".\\?" . "") ("\\?" . "") ("\\." . "")
                                ("\\" . "") ("^" . "") ("$" . "")
                                ("'" . "") ("*." . "") ("*" . ""))
                              ext))
                          (exts (list e)))
                     ;; Handle "[]"
                     (when-let* ((s (s-split "\\[\\|\\]" e))
                                 (f (car s))
                                 (m (cadr s))
                                 (l (caddr s))
                                 (mcs (delete "" (s-split "" m))))
                       (setq exts nil)
                       (dolist (c mcs)
                         (push (s-concat f c l) exts)))
                     ;; Handle '?
                     (dolist (ext exts)
                       (when (s-match "?" ext)
                         (when-let ((s (s-split "?" ext)))
                           (setq exts nil)
                           (push (s-join "" s) exts)
                           (push (s-concat (if (> (length (car s)) 1)
                                               (substring (car s) 0 -1))
                                           (cadr s)) exts))))
                     exts))

                 (dolist (item all-the-icons-icon-alist)
                   (let* ((extensions (get-extenstions (car item)))
                          (func (cadr item))
                          (args (append (list (caddr item)) '(:v-adjust -0.05) (cdddr item)))
                          (icon (apply func args)))
                     (let* ((icon-pair (cons (format "  %s\t" icon) " "))
                            (gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
                            (tui-icons (treemacs-theme->tui-icons treemacs--current-theme))
                            (gui-icon  (car icon-pair))
                            (tui-icon  (cdr icon-pair)))
                       (--each extensions
                         (ht-set! gui-icons it gui-icon)
                         (ht-set! tui-icons it tui-icon)))))))

              (treemacs-load-theme "centaur")))))

      ;; Make certain buffers grossly incandescent
      (use-package solaire-mode
        :functions persp-load-state-from-file
        :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
               (minibuffer-setup . solaire-mode-in-minibuffer)
               (after-load-theme . solaire-mode-swap-bg))
        :config
        (setq solaire-mode-remap-fringe nil)
        (solaire-global-mode 1)
        (solaire-mode-swap-bg)
        (advice-add #'persp-load-state-from-file
                    :after #'solaire-mode-restore-persp-mode-buffers)))
  (progn
    (warn "The current theme may not be compatible with Centaur!")
    (centaur-load-theme centaur-theme)))

;; Mode-line
(use-package doom-modeline
  :custom-face (mode-line ((t (:foreground ,(face-foreground 'default)))))
  :hook ((after-init . doom-modeline-mode)
         (after-load-theme . (lambda ()
                               (set-face-foreground
                                'mode-line
                                (face-foreground 'default)))))
  :init
  ;; prevent flash of unstyled modeline at startup
  (unless after-init-time
    (setq doom-modeline--old-format mode-line-format)
    (setq-default mode-line-format nil))

  (setq doom-modeline-major-mode-color-icon t
        doom-modeline-minor-modes nil
        doom-modeline-mu4e nil)
  :bind ("C-<f6>" . doom-modeline-hydra/body)
  :pretty-hydra
  ((:title (pretty-hydra-title "Mode Line" 'fileicon "emacs")
    :color amaranth :quit-key "q")
   ("Icon"
    (("i" (setq doom-modeline-icon (not doom-modeline-icon))
      "display icons" :toggle doom-modeline-icon)
     ("m" (setq doom-modeline-major-mode-icon (not doom-modeline-major-mode-icon))
      "major mode" :toggle doom-modeline-major-mode-icon)
     ("c" (setq doom-modeline-major-mode-color-icon (not doom-modeline-major-mode-color-icon))
      "colorful major mode" :toggle doom-modeline-major-mode-color-icon)
     ("s" (setq doom-modeline-buffer-state-icon (not doom-modeline-buffer-state-icon))
      "buffer state" :toggle doom-modeline-buffer-state-icon)
     ("d" (setq doom-modeline-buffer-modification-icon (not doom-modeline-buffer-modification-icon))
      "modification" :toggle doom-modeline-buffer-modification-icon)
     ("p" (setq doom-modeline-persp-name-icon (not doom-modeline-persp-name-icon))
      "perspective" :toggle doom-modeline-persp-name-icon))
    "Segment"
    (("M" (setq doom-modeline-minor-modes (not doom-modeline-minor-modes))
      "minor modes" :toggle doom-modeline-minor-modes)
     ("W" (setq doom-modeline-enable-word-count (not doom-modeline-enable-word-count))
      "word count" :toggle doom-modeline-enable-word-count)
     ("E" (setq doom-modeline-buffer-encoding (not doom-modeline-buffer-encoding))
      "encoding" :toggle doom-modeline-buffer-encoding)
     ("I" (setq doom-modeline-indent-info (not doom-modeline-indent-info))
      "indent" :toggle doom-modeline-indent-info)
     ("L" (setq doom-modeline-lsp (not doom-modeline-lsp))
      "lsp" :toggle doom-modeline-lsp)
     ("P" (setq doom-modeline-persp-name (not doom-modeline-persp-name))
      "perspective" :toggle doom-modeline-persp-name)
     ("G" (setq doom-modeline-github (not doom-modeline-github))
      "github" :toggle doom-modeline-github)
     ("U" (setq doom-modeline-mu4e (not doom-modeline-mu4e))
      "mu4e" :toggle doom-modeline-mu4e)
     ("R" (setq doom-modeline-irc (not doom-modeline-irc))
      "irc" :toggle doom-modeline-irc)
     ("S" (setq doom-modeline-checker-simple-format (not doom-modeline-checker-simple-format))
      "simple checker" :toggle doom-modeline-checker-simple-format)
     ("V" (setq doom-modeline-env-version (not doom-modeline-env-version))
      "version" :toggle doom-modeline-env-version))
    "Style"
    (("t u" (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
      "truncate upto project"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-upto-project))
     ("t f" (setq doom-modeline-buffer-file-name-style 'truncate-from-project)
      "truncate from project"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-from-project))
     ("t w" (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
      "truncate with project"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-with-project))
     ("t e" (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
      "truncate except project"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-except-project))
     ("t r" (setq doom-modeline-buffer-file-name-style 'truncate-upto-root)
      "truncate upto root"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-upto-root))
     ("t a" (setq doom-modeline-buffer-file-name-style 'truncate-all)
      "truncate all"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-all))
     ("r f" (setq doom-modeline-buffer-file-name-style 'relative-from-project)
      "relative from project"
      :toggle (eq doom-modeline-buffer-file-name-style 'relative-from-project))
     ("r t" (setq doom-modeline-buffer-file-name-style 'relative-to-project)
      "relative to project"
      :toggle (eq doom-modeline-buffer-file-name-style 'relative-to-project))
     ("f" (setq doom-modeline-buffer-file-name-style 'file-name)
      "file name"
      :toggle (eq doom-modeline-buffer-file-name-style 'file-name))
     ("b" (setq doom-modeline-buffer-file-name-style 'buffer-name)
      "buffer name"
      :toggle (eq doom-modeline-buffer-file-name-style 'buffer-name))))))

(use-package hide-mode-line
  :hook (((completion-list-mode completion-in-region-mode) . hide-mode-line-mode)))

;; Icons
;; NOTE: Must run `M-x all-the-icons-install-fonts', and install fonts manually on Windows
(use-package all-the-icons
  :if (display-graphic-p)
  :init (unless (or sys/win32p (member "all-the-icons" (font-family-list)))
          (all-the-icons-install-fonts t))
  :config
  (add-to-list 'all-the-icons-mode-icon-alist
               '(vterm-mode all-the-icons-octicon "terminal" :v-adjust 0.2))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))

  (add-to-list 'all-the-icons-icon-alist
               '("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-dyellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-toml-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-dyellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.lua$" all-the-icons-fileicon "lua" :face all-the-icons-dblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(lua-mode all-the-icons-fileicon "lua" :face all-the-icons-dblue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.go$" all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(go-mode all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
  (add-to-list 'all-the-icons-icon-alist
               '("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
  (add-to-list 'all-the-icons-icon-alist
               '("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(cask-mode all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-blue)))

;; Show native line numbers if possible, otherwise use linum
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :hook (prog-mode . display-line-numbers-mode))
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :config
    (setq linum-format "%4d ")

    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :hook (global-linum-mode . hlinum-activate)
      :custom-face
      (linum-highlight-face ((t `(:inherit default :background nil :foreground nil))))
      :init (setq linum-highlight-in-all-buffersp t))))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

(when sys/macp
  ;; Render thinner fonts
  (setq ns-use-thin-smoothing t)
  ;; Don't open a file in a new frame
  (setq ns-pop-up-frames nil))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
