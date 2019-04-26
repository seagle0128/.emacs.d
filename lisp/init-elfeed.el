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
         :map elfeed-show-mode-map
         ("o" . ace-link)
         ("q" . delete-window))
  :config
  (setq elfeed-db-directory (locate-user-emacs-file ".elfeed")
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'delete-window)

  (setq elfeed-feeds
        '("http://planet.emacsen.org/atom.xml"
          "http://www.masteringemacs.org/feed/"
          "https://oremacs.com/atom.xml"
          "https://pinecast.com/feed/emacscast"
          "https://www.reddit.com/r/emacs.rss"))

  (defhydra hydra-elfeed (:color red :hint none)
    "
^Search^                   ^Filter^                 ^Article^
^^─────────────────────────^^───────────────────────^^────────────────────
_g_: Refresh               _s_: Live filter         _b_: Browse
_G_: Update                _S_: Set filter          _n_/_C-n_: Next
_y_: Copy URL              _*_: Starred             _p_/_C-p_: Previous
_+_: Tag all               _A_: All                 _u_: Mark read
_-_: Untag all             _T_: Today               _r_: Mark unread
_Q_: Quit
"
    ("G" elfeed-search-fetch)
    ("Q" elfeed-search-quit-window :exit t)
    ("S" elfeed-search-set-filter)
    ("A" (elfeed-search-set-filter "@6-months-ago"))
    ("T" (elfeed-search-set-filter "@1-day-ago"))
    ("*" (elfeed-search-set-filter "@6-months-ago +star"))
    ("+" elfeed-search-tag-all)
    ("-" elfeed-search-untag-all)
    ("b" elfeed-search-browse-url)
    ("g" elfeed-search-update--force)
    ("n" next-line)
    ("C-n" next-line)
    ("p" previous-line)
    ("C-p" previous-line)
    ("r" elfeed-search-untag-all-unread)
    ("s" elfeed-search-live-filter)
    ("u" elfeed-search-tag-all-unread)
    ("y" elfeed-search-yank)
    ("RET" elfeed-search-show-entry)
    ("q" nil "quit" :exit t)
    ("C-g" nil "quit" :exit t))
  (bind-keys :map elfeed-search-mode-map
             ("h" . hydra-elfeed/body)
             ("?" . hydra-elfeed/body)))

(provide 'init-elfeed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-elfeed.el ends here
