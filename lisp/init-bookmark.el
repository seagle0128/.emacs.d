;; init-bookmark.el --- Initialize bookmark configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 2.2.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Bookmark configurations.
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

;; Extensions to `Bookmark'
(use-package bookmark+
  :bind (;; M$ Visual Studio key setup.
         ("<C-f2>" . bmkp-toggle-autonamed-bookmark-set/delete)
         ("<f2>" . bmkp-next-autonamed-bookmark-repeat)
         ("<S-f2>" . bmkp-previous-autonamed-bookmark-repeat))
  :init
  (setq bmkp-last-as-first-bookmark-file nil)
  (setq bmkp-auto-light-when-set 'all-in-buffer)
  (setq bmkp-auto-light-when-jump 'all-in-buffer)
  (unless (and (fboundp 'fringe-columns) (boundp 'fringe-bitmaps))
    (setq bmkp-light-style-autonamed 'line)
    (setq bmkp-light-style-non-autonamed 'line)))

(provide 'init-bookmark)

;;; init-bookmark.el ends here
