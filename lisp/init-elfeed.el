;; init-elfeed.el --- Initialize elfeed.	-*- lexical-binding: t -*-

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
;; A RSS feed reader.
;;

;;; Code:

(use-package elfeed
  :bind (("C-x w" . elfeed)
         :map elfeed-search-mode-map
         ("U" . elfeed-update))
  :config
  (setq elfeed-db-directory (expand-file-name ".elfeed" user-emacs-directory))
  (setq elfeed-feeds
        '("http://planet.emacsen.org/atom.xml"
          "http://www.masteringemacs.org/feed/"
          "https://oremacs.com/atom.xml"
          "https://pinecast.com/feed/emacscast")))

(provide 'init-elfeed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-elfeed.el ends here
