;; init-auto-complete.el --- Initialize auto-complete configurations.
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
;;             Auto complete configurations.
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

(use-package auto-complete
  :defer t
  :diminish auto-complete-mode
  :functions ac-set-trigger-key
  :bind (("\M-/" . ac-start)
         :map ac-completing-map ("\M-/" . ac-stop)
         :map ac-mode-map ("M-TAB" . auto-complete))
  :config
  (progn
    (ac-config-default)
    (setq ac-use-menu-map t)
    (ac-set-trigger-key "TAB")
    (setq ac-delay 0.3)

    (setq ac-sources
          '(ac-source-yasnippet
            ac-source-imenu
            ac-source-abbrev
            ac-source-words-in-same-mode-buffers
            ac-source-files-in-current-dir
            ac-source-filename ))

    (if (featurep 'fish-mode)
        (add-hook 'fish-mode-hook 'auto-complete-mode))

    (use-package ac-inf-ruby :defer t)
    (use-package ac-js2 :defer t)
    ))

(provide 'init-auto-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-auto-complete.el ends here
