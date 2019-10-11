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

(unless (version< (org-version) "9.0")
  (use-package elfeed
    :pretty-hydra
    ((:title (pretty-hydra-title "Elfeed" 'faicon "rss-square")
      :color amaranth :quit-key "q")
     ("Search"
      (("g" elfeed-search-update--force "refresh")
       ("G" elfeed-search-fetch "update")
       ("y" elfeed-search-yank "copy URL")
       ("+" elfeed-search-tag-all "tag all")
       ("-" elfeed-search-untag-all "untag all"))
      "Filter"
      (("s" elfeed-search-live-filter "live filter")
       ("S" elfeed-search-set-filter "set filter")
       ("*" (elfeed-search-set-filter "@6-months-ago +star") "starred")
       ("A" (elfeed-search-set-filter "@6-months-ago" "all"))
       ("T" (elfeed-search-set-filter "@1-day-ago" "today")))
      "Article"
      (("b" elfeed-search-browse-url "browse")
       ("n" next-line "next")
       ("p" previous-line "previous")
       ("u" elfeed-search-tag-all-unread "mark unread")
       ("r" elfeed-search-untag-all-unread "mark read")
       ("RET" elfeed-search-show-entry "show"))))
    :bind (("C-x w" . elfeed)
           :map elfeed-search-mode-map
           ("?" . elfeed-hydra/body)
           :map elfeed-show-mode-map
           ("o" . ace-link)
           ("q" . delete-window))
    :init (setq elfeed-db-directory (locate-user-emacs-file ".elfeed")
                elfeed-show-entry-switch #'pop-to-buffer
                elfeed-show-entry-delete #'delete-window
                elfeed-feeds '("http://planet.emacsen.org/atom.xml"
                               "http://www.masteringemacs.org/feed/"
                               "https://oremacs.com/atom.xml"
                               "https://pinecast.com/feed/emacscast"
                               "https://www.reddit.com/r/emacs.rss"))
    :config (push elfeed-db-directory recentf-exclude)))

  (provide 'init-elfeed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-elfeed.el ends here
