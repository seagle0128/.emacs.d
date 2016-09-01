;; init-auto-complete.el --- Initialize auto-complete configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 2.0.0
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
  :bind (("M-/" . ac-start))
  :init (add-hook 'after-init-hook 'ac-config-default)
  :config
  (progn
    (setq ac-use-menu-map t)
    (ac-set-trigger-key "TAB")
    (setq ac-delay 0.3)

    (bind-key "M-/" 'ac-stop ac-completing-map)
    (bind-key "M-TAB" 'auto-complete ac-mode-map)

    (setq ac-sources
          '(ac-source-yasnippet
            ac-source-imenu
            ac-source-abbrev
            ac-source-words-in-same-mode-buffers
            ac-source-files-in-current-dir
            ac-source-filename))
    ))

(provide 'init-auto-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-auto-complete.el ends here
