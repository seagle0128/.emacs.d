;; init-ruby.el --- Initialize ruby configurations.	-*- lexical-binding: t -*-

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
;; Ruby configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package ruby-mode
  :ensure nil
  :mode "\\.\\(rb\\|rake\\|\\gemspec\\|ru\\|\\(Rake\\|Gem\\|Guard\\|Cap\\|Vagrant\\)file\\)$"
  :interpreter "ruby"
  :config
  ;; Code navigation, documentation lookup and completion for Ruby
  (unless centaur-lsp
    (use-package robe
      :diminish robe-mode
      :defines company-backends
      :hook (ruby-mode . robe-mode)
      :config
      (with-eval-after-load 'company
        (cl-pushnew 'company-robe company-backends))))

  ;; Ruby refactoring helpers
  (use-package ruby-refactor
    :diminish ruby-refactor-mode
    :hook (ruby-mode . ruby-refactor-mode-launch))

  ;; Run a Ruby process in a buffer
  (use-package inf-ruby
    :hook ((ruby-mode . inf-ruby-minor-mode)
           (compilation-filter . inf-ruby-auto-enter)))

  ;; Rubocop
  ;; Install: gem install rubocop
  (use-package rubocop
    :diminish rubocop-mode
    :hook (ruby-mode . rubocop-mode))

  ;; RSpec
  (use-package rspec-mode
    :diminish rspec-mode
    :commands rspec-install-snippets
    :hook (dired-mode . rspec-dired-mode)
    :config (with-eval-after-load 'yasnippet
              (rspec-install-snippets)))

  ;; Coverage for SimpleCov
  (use-package coverage)

  ;; Yet Another RI interface for Emacs
  (use-package yari
    :bind (:map ruby-mode-map ([f1] . yari)))

  ;; Ruby YARD comments
  (use-package yard-mode
    :diminish yard-mode
    :hook (ruby-mode . yard-mode)))

;; YAML mode
(use-package yaml-mode)

(provide 'init-ruby)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ruby.el ends here
