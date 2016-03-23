;; init-ruby.el --- Initialize ruby configurations.
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

;; Ruby mode
(use-package ruby-mode
  :defer t
  :mode "\\.\\(rake\\|\\gemspec\\|ru\\|\\(Rake\\|Gem\\|Guard\\|Cap\\|Vagrant\\)file\\)$"
  :config
  (progn
    ;; Robe mode
    (use-package robe
      :defer t
      :diminish robe-mode
      :defines ac-modes company-backends
      :init
      (progn
        (add-hook 'ruby-mode-hook 'robe-mode)

        ;; auto complete
        (eval-after-load 'auto-complete
          '(add-hook 'robe-mode-hook 'ac-robe-setup))
        (eval-after-load 'auto-complete
          '(add-to-list 'ac-modes 'inf-ruby-minor-mode))
        (eval-after-load 'auto-complete
          '(add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable))
        (eval-after-load 'auto-complete
          '(define-key inf-ruby-mode-map (kbd "TAB") 'auto-complete))

        ;; company
        (eval-after-load 'company
          '(push 'company-robe company-backends))))

    ;; inf-ruby
    (use-package inf-ruby
      :defer t
      :init
      (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
      (add-hook 'after-init-hook 'inf-ruby-switch-setup)
      (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter))

    ;; Yari
    (use-package yari
      :defer t
      :bind (:map ruby-mode-map ([f1] . yari))
      :config
      (eval-after-load 'helm
        '(bind-key [f1] 'yari-helm ruby-mode-map)))

    ;; Yard mode
    (use-package yard-mode
      :defer t
      :diminish yard-mode
      :init (add-hook 'ruby-mode-hook 'yard-mode))
    ))

;; YAML mode
(use-package yaml-mode
  :defer t
  :mode "\\.yml$")

(provide 'init-ruby)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ruby.el ends here
