;; init-kill-ring.el --- Initialize kill-ring configurations.
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
;;             Kill ring configurations.
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

(setq kill-ring-max 200)

(use-package easy-kill
  :defer t
  :bind ((:map kill-ring-save easy-kill)
         (:map mark-sexp easy-mark)))

;; Only use with ido
(eval-after-load 'ido
  (progn
    (use-package browse-kill-ring
      :defer t
      :bind ("C-c k" . browse-kill-ring))
    
    (use-package popup-kill-ring
      :defer t
      :bind ("M-y" . popup-kill-ring))))

(provide 'init-kill-ring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-kill-ring.el ends here
