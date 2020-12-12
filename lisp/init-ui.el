;; init-ui.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2020 Vincent Zhang

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

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Logo
(setq fancy-splash-image centaur-logo)

;; Title
(setq frame-title-format '("Centaur Emacs - %b")
      icon-title-format frame-title-format)

(when (and sys/mac-ns-p sys/mac-x-p)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Menu/Tool/Scroll bars
(unless emacs/>=27p
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist))

;; Theme
(if (centaur-compatible-theme-p centaur-theme)
    (progn
      ;; Make certain buffers grossly incandescent
      ;; Must before loading the theme
      (use-package solaire-mode
        :functions persp-load-state-from-file
        :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
               (minibuffer-setup . solaire-mode-in-minibuffer))
        :init
        (solaire-global-mode 1)
        (advice-add #'persp-load-state-from-file :after #'solaire-mode-restore-persp-mode-buffers))

      (use-package doom-themes
        :custom-face
        (doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
        :custom
        (doom-themes-treemacs-theme "doom-colors")
        :init (centaur-load-theme centaur-theme t)
        :config
        ;; Enable flashing mode-line on errors
        (doom-themes-visual-bell-config)

        ;; Enable customized theme
        (doom-themes-treemacs-config)))
  (progn
    (warn "The current theme may not be compatible!")
    (centaur-load-theme centaur-theme t)))

;; Mode-line
(use-package doom-modeline
  :custom
  (doom-modeline-icon centaur-icon)
  (doom-modeline-minor-modes t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-mu4e nil)
  :hook (after-init . doom-modeline-mode)
  :init
  ;; Prevent flash of unstyled modeline at startup
  (unless after-init-time
    (setq doom-modeline--default-format mode-line-format)
    (setq-default mode-line-format nil))
  :bind (:map doom-modeline-mode-map
         ("C-<f6>" . doom-modeline-hydra/body))
  :pretty-hydra
  ((:title (pretty-hydra-title "Mode Line" 'fileicon "emacs" :face 'all-the-icons-purple :v-adjust -0.1)
    :color amaranth :quit-key "q")
   ("Icon"
    (("i" (setq doom-modeline-icon (not doom-modeline-icon))
      "display icons" :toggle doom-modeline-icon)
     ("u" (setq doom-modeline-unicode-fallback (not doom-modeline-unicode-fallback))
      "unicode fallback" :toggle doom-modeline-unicode-fallback)
     ("m" (setq doom-modeline-major-mode-icon (not doom-modeline-major-mode-icon))
      "major mode" :toggle doom-modeline-major-mode-icon)
     ("c" (setq doom-modeline-major-mode-color-icon (not doom-modeline-major-mode-color-icon))
      "colorful major mode" :toggle doom-modeline-major-mode-color-icon)
     ("s" (setq doom-modeline-buffer-state-icon (not doom-modeline-buffer-state-icon))
      "buffer state" :toggle doom-modeline-buffer-state-icon)
     ("o" (setq doom-modeline-buffer-modification-icon (not doom-modeline-buffer-modification-icon))
      "modification" :toggle doom-modeline-buffer-modification-icon)
     ("v" (setq doom-modeline-modal-icon (not doom-modeline-modal-icon))
      "modal" :toggle doom-modeline-modal-icon))
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
     ("N" (setq doom-modeline-gnus (not doom-modeline-gnus))
      "gnus" :toggle doom-modeline-gnus)
     ("U" (setq doom-modeline-mu4e (not doom-modeline-mu4e))
      "mu4e" :toggle doom-modeline-mu4e)
     ("R" (setq doom-modeline-irc (not doom-modeline-irc))
      "irc" :toggle doom-modeline-irc)
     ("F" (setq doom-modeline-irc-buffers (not doom-modeline-irc-buffers))
      "irc buffers" :toggle doom-modeline-irc-buffers)
     ("S" (progn
            (setq doom-modeline-checker-simple-format (not doom-modeline-checker-simple-format))
            (and (bound-and-true-p flycheck-mode) (flycheck-buffer)))
      "simple checker" :toggle doom-modeline-checker-simple-format)
     ("V" (setq doom-modeline-env-version (not doom-modeline-env-version))
      "version" :toggle doom-modeline-env-version))
    "Style"
    (("a" (setq doom-modeline-buffer-file-name-style 'auto)
      "auto"
      :toggle (eq doom-modeline-buffer-file-name-style 'auto))
     ("b" (setq doom-modeline-buffer-file-name-style 'buffer-name)
      "buffer name"
      :toggle (eq doom-modeline-buffer-file-name-style 'buffer-name))
     ("f" (setq doom-modeline-buffer-file-name-style 'file-name)
      "file name"
      :toggle (eq doom-modeline-buffer-file-name-style 'file-name))
     ("t u" (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
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
     ("t n" (setq doom-modeline-buffer-file-name-style 'truncate-nil)
      "truncate none"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-nil))
     ("r f" (setq doom-modeline-buffer-file-name-style 'relative-from-project)
      "relative from project"
      :toggle (eq doom-modeline-buffer-file-name-style 'relative-from-project))
     ("r t" (setq doom-modeline-buffer-file-name-style 'relative-to-project)
      "relative to project"
      :toggle (eq doom-modeline-buffer-file-name-style 'relative-to-project)))
    "Project Detection"
    (("p f" (setq doom-modeline-project-detection 'ffip)
      "ffip"
      :toggle (eq doom-modeline-project-detection 'ffip))
     ("p t" (setq doom-modeline-project-detection 'projectile)
      "projectile"
      :toggle (eq doom-modeline-project-detection 'projectile))
     ("p p" (setq doom-modeline-project-detection 'project)
      "project"
      :toggle (eq doom-modeline-project-detection 'project))
     ("p n" (setq doom-modeline-project-detection nil)
      "disable"
      :toggle (eq doom-modeline-project-detection nil)))
    "Misc"
    (("g" (progn
            (message "Fetching GitHub notifications...")
            (run-with-timer 300 nil #'doom-modeline--github-fetch-notifications)
            (browse-url "https://github.com/notifications"))
      "github notifications" :exit t)
     ("e" (if (bound-and-true-p flycheck-mode)
              (flycheck-list-errors)
            (flymake-show-diagnostics-buffer))
      "list errors" :exit t)
     ("B" (if (bound-and-true-p grip-mode)
              (grip-browse-preview)
            (message "Not in preview"))
      "browse preview" :exit t)
     ("z h" (counsel-read-setq-expression 'doom-modeline-height) "set height")
     ("z w" (counsel-read-setq-expression 'doom-modeline-bar-width) "set bar width")
     ("z g" (counsel-read-setq-expression 'doom-modeline-github-interval) "set github interval")
     ("z n" (counsel-read-setq-expression 'doom-modeline-gnus-timer) "set gnus interval")))))

(use-package hide-mode-line
  :hook (((completion-list-mode
           completion-in-region-mode
           pdf-annot-list-mode
           flycheck-error-list-mode) . hide-mode-line-mode)))

;; A minor-mode menu for mode-line
(when emacs/>=25.2p
  (use-package minions
    :hook (doom-modeline-mode . minions-mode)))

;; Icons
;; NOTE: Must run `M-x all-the-icons-install-fonts', and install fonts manually on Windows
(use-package all-the-icons
  :if (and centaur-icon (display-graphic-p))
  :init (unless (or sys/win32p (font-installed-p "all-the-icons"))
          (all-the-icons-install-fonts t))
  :config
  (declare-function memoize 'memoize)
  (declare-function memoize-restore 'memoize)
  (defun all-the-icons-reset ()
    "Reset (unmemoize/memoize) the icons."
    (interactive)
    (ignore-errors
      (dolist (f '(all-the-icons-icon-for-file
                   all-the-icons-icon-for-mode
                   all-the-icons-icon-for-url
                   all-the-icons-icon-family-for-file
                   all-the-icons-icon-family-for-mode
                   all-the-icons-icon-family))
        (memoize-restore f)
        (memoize f)))
    (message "Reset all-the-icons"))

  (add-to-list 'all-the-icons-icon-alist
               '("^Rakefile$" all-the-icons-alltheicon "ruby-alt" :face all-the-icons-red))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.go$" all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\go.mod$" all-the-icons-fileicon "go" :face all-the-icons-dblue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\go.sum$" all-the-icons-fileicon "go" :face all-the-icons-dpurple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(go-mode all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(xwidget-webkit-mode all-the-icons-faicon "chrome" :v-adjust -0.1 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-playlist-mode all-the-icons-material "queue_music" :height 1.2 :face 'all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-library-mode all-the-icons-material "library_music" :height 1.1 :face 'all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-group-mode all-the-icons-fileicon "gnu" :face 'all-the-icons-silver))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-summary-mode all-the-icons-octicon "inbox" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-article-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(message-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(diff-mode all-the-icons-octicon "git-compare" :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(flycheck-error-list-mode all-the-icons-octicon "checklist" :height 1.1 :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.rss$" all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-search-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-show-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-list-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-item-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.[bB][iI][nN]$" all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.c?make$" all-the-icons-fileicon "gnu" :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.conf$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-space-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(forge-topic-mode all-the-icons-alltheicon "git" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(helpful-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
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
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-lblue)))

;; Show native line numbers if possible, otherwise use `linum'
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :hook (prog-mode . display-line-numbers-mode))
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :init (setq linum-format "%4d ")
    :config
    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :custom-face (linum-highlight-face ((t (:inherit default :background nil :foreground nil))))
      :hook (global-linum-mode . hlinum-activate)
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

;; Easily adjust the font size in all frames
(use-package default-text-scale
  :hook (after-init . default-text-scale-mode)
  :bind (:map default-text-scale-mode-map
         ("s-=" . default-text-scale-increase)
         ("s--" . default-text-scale-decrease)
         ("s-0" . default-text-scale-reset)
         ("C-s-=" . default-text-scale-increase)
         ("C-s--" . default-text-scale-decrease)
         ("C-s-0" . default-text-scale-reset)))

;; Use fixed pitch where it's sensible
(use-package mixed-pitch
  :diminish)

(with-no-warnings
  (when sys/macp
    ;; Render thinner fonts
    (setq ns-use-thin-smoothing t)
    ;; Don't open a file in a new frame
    (setq ns-pop-up-frames nil)))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; When `centaur-prettify-symbols-alist' is `nil' use font supported ligatures
(use-package composite
  :ensure nil
  :unless centaur-prettify-symbols-alist
  :init (defvar composition-ligature-table (make-char-table nil))
  :hook (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
          . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
  ;; support ligatures, some toned down to prevent hang
  (when emacs/>=27p
    (let ((alist
           '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
             (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
             (36 . ".\\(?:\\(>\\)>?\\)")
             (37 . ".\\(?:\\(%\\)%?\\)")
             (38 . ".\\(?:\\(&\\)&?\\)")
             (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
             ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
             (43 . ".\\(?:\\([>]\\)>?\\)")
             ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
             (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
             ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
             (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
             (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
             ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
             (48 . ".\\(?:x[a-zA-Z]\\)")
             (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
             (59 . ".\\(?:\\(;\\);?\\)")
             (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
             (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
             (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
             (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
             (91 . ".\\(?:\\(|\\)[]|]?\\)")
             ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
             (94 . ".\\(?:\\(=\\)=?\\)")
             (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
             (119 . ".\\(?:\\(ww\\)w?\\)")
             (123 . ".\\(?:\\(|\\)[|}]?\\)")
             (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
             (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table)))

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
