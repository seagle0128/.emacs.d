;; init-ruby.el --- Initialize ruby configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2010-2020 Vincent Zhang

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
;; Ruby configurations.
;;

;;; Code:

;; Integrate rbenv
(use-package rbenv
  :hook (after-init . global-rbenv-mode)
  :init (setq rbenv-show-active-ruby-in-modeline nil
              rbenv-executable "rbenv"))

;; YAML mode
(use-package yaml-mode)

;; Run a Ruby process in a buffer
(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-auto-enter)))

;; Ruby YARD comments
(use-package yard-mode
  :diminish
  :hook (ruby-mode . yard-mode))

;; Ruby refactoring helpers
(use-package ruby-refactor
  :diminish
  :hook (ruby-mode . ruby-refactor-mode-launch))

;; Yet Another RI interface for Emacs
(use-package yari
  :bind (:map ruby-mode-map ([f1] . yari)))

;; RSpec
(use-package rspec-mode
  :diminish
  :commands rspec-install-snippets
  :hook (dired-mode . rspec-dired-mode)
  :config (with-eval-after-load 'yasnippet
            (rspec-install-snippets)))

;; Rails
(use-package projectile-rails
  :diminish
  :hook (projectile-mode . projectile-rails-global-mode))

(provide 'init-ruby)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ruby.el ends here
