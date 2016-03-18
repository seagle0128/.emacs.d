;;; init-package.el --- Initialize package configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Package configurations.
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

(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(unless (package-installed-p 'package+)
  (unless (assoc 'package+ package-archive-contents)
    (package-refresh-contents))
  (package-install 'package+))

(package-manifest

 ;; ace
 'ace-jump-mode 'ace-link

 ;; autocomplete
 'auto-complete 'ac-inf-ruby 'ac-js2

 ;; company
 'company 'company-anaconda 'company-flx 'company-quickhelp 'company-shell

 ;; edit
 'anzu 'aggressive-indent 'auto-indent-mode 'back-button
 'comment-dwim-2 'expand-region 'easy-kill 'iedit
 'mwim 'move-text 'multiple-cursors 'pager-default-keybindings
 'smartparens 'undo-tree

 ;; font
 'chinese-fonts-setup

 ;; flycheck
 'flycheck 'flycheck-pos-tip

 ;; helm
 'helm 'helm-ag 'helm-bm 'helm-cmd-t 'helm-descbinds 'helm-flycheck
 'helm-ls-git 'helm-projectile 'helm-mt 'helm-swoop

 ;; highlight
 'diff-hl 'highlight-indentation 'highlight-symbol 'volatile-highlights
 'rainbow-delimiters 'rainbow-mode 'color-identifiers-mode 'fic-mode

 ;; ido
 'ido-at-point 'ido-complete-space-or-hyphen 'ido-load-library 'idomenu
 'ido-sort-mtime 'ido-ubiquitous'ido-vertical-mode 'flx-ido 'smex

 ;; imenu
 'imenu-list

 ;; kill ring
 'browse-kill-ring 'popup-kill-ring

 ;; org
 'org

 ;; package
 'package+ 'paradox

 ;; programming mode
 'coffee-mode 'csharp-mode 'dos 'fish-mode 'js2-mode 'json-mode
 'haml-mode 'less-css-mode 'markdown-mode 'php-mode 'powershell
 'scss-mode'web-beautify 'web-mode

 ;; project
 'projectile 'projectile-rails

 ;; python
 'anaconda-mode 'py-autopep8

 ;; ruby
 'robe 'inf-ruby 'yaml-mode 'yard-mode 'yari

 ;; scm
 'magit 'psvn

 ;; theme
 'monokai-theme 'powerline 'spaceline

 ;; utils
 'ack 'ag 'browse-url-dwim 'cal-china-x 'ctags-update
 'dash-at-point 'diminish 'dropdown-list
 'exec-path-from-shell 'htmlize 'list-environment
 'multi-term 'neotree 'persistent-scratch 'smooth-scrolling
 'swoop 'which-key 'yasnippet 'youdao-dictionary

 ;; window
 'switch-window 'zoom-window

 ;; benchmark
 'benchmark-init
 )

(setq paradox-github-token t)

(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
