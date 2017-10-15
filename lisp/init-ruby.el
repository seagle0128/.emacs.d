;; init-ruby.el --- Initialize ruby configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 3.0.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Ruby configurations.
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

(use-package ruby-mode
  :ensure nil
  :mode "\\.\\(rb\\|rake\\|\\gemspec\\|ru\\|\\(Rake\\|Gem\\|Guard\\|Cap\\|Vagrant\\)file\\)$"
  :interpreter "ruby"
  :config
  ;; Code navigation, documentation lookup and completion for Ruby
  (use-package robe
    :diminish robe-mode
    :init
    (add-hook 'ruby-mode-hook #'robe-mode)

    (with-eval-after-load 'company
      (push '(company-robe :with company-yasnippet) company-backends)))

  ;; Ruby refactoring helpers
  (use-package ruby-refactor
    :diminish ruby-refactor-mode
    :init (add-hook 'ruby-mode-hook #'ruby-refactor-mode-launch))

  ;; Run a Ruby process in a buffer
  (use-package inf-ruby
    :init
    (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
    (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter))

  ;; Rubocop
  (use-package rubocop
    :diminish rubocop-mode
    :init (add-hook 'ruby-mode-hook #'rubocop-mode))

  ;; RSpec
  (use-package rspec-mode
    :diminish rspec-mode
    :commands rspec-install-snippets
    :init (add-hook 'dired-mode-hook #'rspec-dired-mode)
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
    :init (add-hook 'ruby-mode-hook #'yard-mode)))

;; YAML mode
(use-package yaml-mode)

(provide 'init-ruby)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ruby.el ends here
