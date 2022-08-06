;; init-lua.el --- Initialize c configurations.	-*- lexical-binding: t -*-

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
;; sis configuration.
;;

;;; Code:

;; sis Mode

(use-package sis
  :init
  ;; `C-s/r' 默认优先使用英文 必须在 sis-global-respect-mode 前配置
  (setq sis-respect-go-english-triggers
	    (list 'isearch-forward 'isearch-backward) ; isearch-forward 命令时默认进入en
	    sis-respect-restore-triggers
	    (list 'isearch-exit 'isearch-abort)) ; isearch-forward 恢复, isearch-exit `<Enter>', isearch-abor `C-g'
  :config
  (sis-ism-lazyman-config "1033" "2052" 'im-select) ; 输入码 1033/英文，2052/中文小狼毫
  ;; enable the /cursor color/ mode 中英文光标颜色模式
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode buffer 输入法状态记忆模式
  (sis-global-respect-mode t)
  ;; enable the /follow context/ mode for all buffers
  (sis-global-follow-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t) ; 中文输入法状态下，中文后<spc>自动切换英文，结束后自动切回中文
  ;; (global-set-key (kbd "<f9>") 'sis-log-mode) ; 开启日志
  ;; 特殊定制
  (setq sis-default-cursor-color "green yellow" ; 英文光标色
   	    sis-other-cursor-color "#FF2121" ; 中文光标色
  	    ;; sis-inline-tighten-head-rule 'all ; 删除头部空格，默认1，删除一个空格，1/0/'all
	    sis-inline-tighten-tail-rule 'all ; 删除尾部空格，默认1，删除一个空格，1/0/'all
	    sis-inline-with-english t ; 默认是t, 中文context下输入<spc>进入内联英文
	    sis-inline-with-other t) ; 默认是nil，而且prog-mode不建议开启, 英文context下输入<spc><spc>进行内联中文
  ;; 特殊buffer禁用sis前缀,使用Emacs原生快捷键  setqsis-prefix-override-buffer-disable-predicates
  (setq sis-prefix-override-buffer-disable-predicates
	    (list 'minibufferp
	          (lambda (buffer) ; magit revision magit的keymap是基于text property的，优先级比sis更高。进入 magit 后，disable sis的映射
		        (sis--string-match-p "^magit-revision:" (buffer-name buffer)))
	          (lambda (buffer) ; special buffer，所有*打头的buffer，但是不包括*Scratch* *New, *About GNU等buffer
		        (and (sis--string-match-p "^\*" (buffer-name buffer))
		             (not (sis--string-match-p "^\*About GNU Emacs" (buffer-name buffer))) ; *About GNU Emacs" 仍可使用 C-h/C-x/C-c 前缀
		             (not (sis--string-match-p "^\*New" (buffer-name buffer)))
		             (not (sis--string-match-p "^\*Scratch" (buffer-name buffer))))))) ; *Scratch*  仍可使用 C-h/C-x/C-c 前缀
  )

(provide 'init-sis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-sis.el ends here
