;; init-calendar.el --- Initialize calendar configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2020 Vincent Zhang

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
;; Calendar configuration.
;;

;;; Code:

(require 'init-custom)

;; Chinese calendar
;; `pC' can show lunar details
(when centaur-chinese-calendar
  (use-package cal-china-x
    :after calendar
    :commands cal-china-x-setup
    :init (cal-china-x-setup)
    :config
    ;; Holidays
    (setq calendar-mark-holidays-flag t
          cal-china-x-important-holidays cal-china-x-chinese-holidays
          cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")
                                         (holiday-lunar 7 7 "七夕节")
                                         (holiday-fixed 3 8 "妇女节")
                                         (holiday-fixed 3 12 "植树节")
                                         (holiday-fixed 5 4 "青年节")
                                         (holiday-fixed 6 1 "儿童节")
                                         (holiday-fixed 9 10 "教师节"))
          holiday-other-holidays '((holiday-fixed 2 14 "情人节")
                                   (holiday-fixed 4 1 "愚人节")
                                   (holiday-fixed 12 25 "圣诞节")
                                   (holiday-float 5 0 2 "母亲节")
                                   (holiday-float 6 0 3 "父亲节")
                                   (holiday-float 11 4 4 "感恩节"))
          calendar-holidays (append cal-china-x-important-holidays
                                    cal-china-x-general-holidays
                                    holiday-other-holidays))))

;; Better views of calendar
(use-package calfw
  :commands cfw:open-calendar-buffer
  :bind ("<C-f12>" . open-calendar)
  :init
  (use-package calfw-org
    :commands (cfw:open-org-calendar cfw:org-create-source))

  (use-package calfw-ical
    :commands (cfw:open-ical-calendar cfw:ical-create-source))

  (defun open-calendar ()
    "Open calendar."
    (interactive)
    (unless (ignore-errors
              (cfw:open-calendar-buffer
               :contents-sources
               (list
                (when org-agenda-files
                  (cfw:org-create-source "YellowGreen"))
                (when (bound-and-true-p centaur-ical)
                  (cfw:ical-create-source "gcal" centaur-ical "IndianRed")))))
      (cfw:open-calendar-buffer)))
  (defalias 'centaur-open-calendar #'open-calendar))

(provide 'init-calendar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-calendar.el ends here
