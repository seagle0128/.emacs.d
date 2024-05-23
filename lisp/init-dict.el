;; init-dict.el --- Initialize dictionaries.	-*- lexical-binding: t -*-

;; Copyright (C) 2021 Vincent Zhang

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
;; Multiple dictionaries.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; A multi dictionaries interface
(use-package fanyi
  :bind (("C-c d f" . fanyi-dwim)
         ("C-c d d" . fanyi-dwim2)
         ("C-c d h" . fanyi-from-history))
  :custom (fanyi-providers '(fanyi-haici-provider
                             fanyi-youdao-thesaurus-provider
                             fanyi-etymon-provider
                             fanyi-longman-provider)))

(when emacs/>=28p
  (use-package go-translate
    :bind (("C-c g"   . gt-do-translate)
           ("C-c G"   . gt-multi-dict-translate)
           ("C-c u"   . gt-do-text-utility)
           ("C-c y"   . gt-youdao-dict-translate-dwim)
           ("C-c Y"   . gt-youdao-dict-translate)
           ("C-c d b" . gt-bing-translate-dwim)
           ("C-c d B" . gt-bing-translate)
           ("C-c d g" . gt-do-translate)
           ("C-c d G" . gt-multi-dict-translate)
           ("C-c d m" . gt-multi-dict-translate-dwim)
           ("C-c d M" . gt-multi-dict-translate)
           ("C-c d y" . gt-youdao-dict-translate-dwim)
           ("C-c d Y" . gt-youdao-dict-translate)
           ("C-c d s" . gt-do-setup)
           ("C-c d u" . gt-do-text-utility))
    :init
    (setq gt-langs '(en zh)
          gt-buffer-render-follow-p t
          gt-buffer-render-window-config
          '((display-buffer-reuse-window display-buffer-in-direction)
            (direction . bottom)
            (window-height . 0.4)))

    (setq gt-pop-posframe-forecolor (face-foreground 'tooltip nil t)
          gt-pop-posframe-backcolor (face-background 'tooltip nil t))
    (when (facep 'posframe-border)
      (setq gt-pin-posframe-bdcolor (face-background 'posframe-border nil t)))
    :config
    ;; Tweak child frame
    (with-no-warnings
      ;; Translators
      (setq gt-preset-translators
            `((default          . ,(gt-translator :taker   (cdar (gt-ensure-plain gt-preset-takers))
                                                  :engines (cdar (gt-ensure-plain gt-preset-engines))
                                                  :render  (cdar (gt-ensure-plain gt-preset-renders))))
              (youdao-dict      . ,(gt-translator :taker (gt-taker :langs '(en zh) :text 'word :prompt t)
                                                  :engines (gt-youdao-dict-engine)
                                                  :render (gt-buffer-render)))
              (youdao-dict-dwim . ,(gt-translator :taker (gt-taker :langs '(en zh) :text 'word)
                                                  :engines (gt-youdao-dict-engine)
                                                  :render (if (display-graphic-p)
                                                              (gt-posframe-pop-render
                                                               :frame-params (list :accept-focus nil
                                                                                   :width 70
                                                                                   :height 15
                                                                                   :left-fringe 16
                                                                                   :right-fringe 16
                                                                                   :border-width 1
                                                                                   :border-color gt-pin-posframe-bdcolor))
                                                            (gt-buffer-render))))
              (bing             . ,(gt-translator :taker (gt-taker :langs '(en zh) :text 'word :prompt t)
                                                  :engines (gt-bing-engine)
                                                  :render (gt-buffer-render)))
              (bing-dwim        . ,(gt-translator :taker (gt-taker :langs '(en zh) :text 'word)
                                                  :engines (gt-bing-engine)
                                                  :render (if (display-graphic-p)
                                                              (gt-posframe-pop-render
                                                               :frame-params (list :accept-focus nil
                                                                                   :width 70
                                                                                   :height 15
                                                                                   :left-fringe 16
                                                                                   :right-fringe 16
                                                                                   :border-width 1
                                                                                   :border-color gt-pin-posframe-bdcolor))
                                                            (gt-buffer-render))))
              (multi-dict       . ,(gt-translator :taker (gt-taker :langs '(en zh) :prompt t)
                                                  :engines (list (gt-bing-engine)
                                                                 (gt-youdao-dict-engine)
                                                                 (gt-youdao-suggest-engine)
                                                                 (gt-google-engine))
                                                  :render (gt-buffer-render)))
              (multi-dict-dwim  . ,(gt-translator :taker (gt-taker :langs '(en zh))
                                                  :engines (list (gt-bing-engine)
                                                                 (gt-youdao-dict-engine)
                                                                 (gt-youdao-suggest-engine)
                                                                 (gt-google-engine))
                                                  :render (gt-buffer-render)))
              (Text-Utility     . ,(gt-text-utility :taker (gt-taker :pick nil)
                                                    :render (gt-buffer-render)))))
      (setq gt-default-translator (alist-get 'multi-dict-dwim gt-preset-translators))

      (defun gt--do-translate (dict)
        "Translate using DICT from preset tranlators."
        (gt-start (alist-get dict gt-preset-translators)))

      (defun gt-youdao-dict-translate ()
        "Translate using Youdao dictionary."
        (interactive)
        (gt--do-translate 'youdao-dict))

      (defun gt-youdao-dict-translate-dwim ()
        "Translate using Youdao dictionary without any prompt."
        (interactive)
        (gt--do-translate 'youdao-dict-dwim))

      (defun gt-bing-translate ()
        "Translate using Bing."
        (interactive)
        (gt--do-translate 'bing))

      (defun gt-bing-translate-dwim ()
        "Translate using Bing without any prompt."
        (interactive)
        (gt--do-translate 'bing-dwim))

      (defun gt-multi-dict-translate ()
        "Translate using multiple dictionaries."
        (interactive)
        (gt--do-translate 'multi-dict))

      (defun gt-multi-dict-translate-dwim ()
        "Translate using multiple dictionaries without any prompt."
        (interactive)
        (gt--do-translate 'multi-dict-dwim))

      (defun gt-do-text-utility ()
        "Handle the texts with the utilities."
        (interactive)
        (gt--do-translate 'Text-Utility)))))

;; OSX dictionary
(when sys/macp
  (use-package osx-dictionary
    :bind (("C-c d i" . osx-dictionary-search-input)
           ("C-c d x" . osx-dictionary-search-pointer))))

(provide 'init-dict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dict.el ends here
