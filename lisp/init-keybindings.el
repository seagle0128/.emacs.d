;; init-keybindings.el --- Initialize c configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2022 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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
;; keybindings configuration.
;;

(global-set-key (kbd "s-a") 'mark-whole-buffer) ;; 对应Windows上面的Ctrl-a 全选
(global-set-key (kbd "s-c") 'kill-ring-save)    ;; 对应Windows上面的Ctrl-c 复制
(global-set-key (kbd "s-s") 'save-buffer)       ;; 对应Windows上面的Ctrl-s 保存
(global-set-key (kbd "s-v") 'yank)              ;; 对应Windows上面的Ctrl-v 粘贴
(global-set-key (kbd "s-z") 'undo)              ;; 对应Windows上面的Ctrol-z 撤销
(global-set-key (kbd "s-x") 'kill-region)       ;; 对应Windows上面的Ctrol-x 剪切

(provide 'init-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keybindings.el ends here
