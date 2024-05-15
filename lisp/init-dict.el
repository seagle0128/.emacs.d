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
           ("C-c u"   . gt-do-text-utiliuty)
           ("C-c y"   . gt-youdao-dict-translate-dwim)
           ("C-c d b" . gt-bing-translate)
           ("C-c d B" . gt-bing-translate-dwim)
           ("C-c d g" . gt-do-translate)
           ("C-c d m" . gt-multi-dict-translate-dwim)
           ("C-c d M" . gt-multi-dict-translate)
           ("C-c d y" . gt-youdao-dict-translate)
           ("C-c d Y" . gt-youdao-dict-translate-dwim)
           ("C-c d s" . gt-do-setup)
           ("C-c d u" . gt-do-text-utility))
    :init
    (setq gt-langs '(en zh)
          gt-buffer-render-window-config
          '((display-buffer-reuse-window display-buffer-in-side-window)
            (side . bottom)
            (slot . 1)
            (window-height . 0.4)))
    (when (facep 'posframe-border)
      (setq gt-pin-posframe-bdcolor (face-background 'posframe-border nil t)))
    :config
    ;; Same behavior with `popper'
    (add-hook 'gt-buffer-render-output-hook
              (lambda ()
                "Focus in the dict result window."
                (when-let ((win (get-buffer-window gt-buffer-render-buffer-name)))
                  (and (window-live-p win) (select-window win)))))
    (advice-add #'keyboard-quit :before
                (lambda (&rest _)
                  "Close dict result window via `C-g'."
                  (when-let ((win (get-buffer-window gt-buffer-render-buffer-name)))
                    (and (window-live-p win) (delete-window win)))))

    ;; Tweak child frame
    (defclass gt-posframe-pop-render (gt-buffer-render)
      ((width       :initarg :width        :initform 70)
       (height      :initarg :height       :initform 15)
       (forecolor   :initarg :forecolor    :initform nil)
       (backcolor   :initarg :backcolor    :initform nil)
       (padding     :initarg :padding      :initform 16)
       (bd-width    :initarg :bd-width     :initform 1)
       (bd-color    :initarg :bd-color     :initform nil))
      "Pop up a childframe to show the result.
The frame will disappear when do do anything but focus in it.
Manually close the frame with `q'.")

    (cl-defmethod gt-init ((render gt-posframe-pop-render) translator)
      (with-slots (width height bd-width forecolor backcolor bd-color padding) render
        (let ((inhibit-read-only t)
              (buf gt-posframe-pop-render-buffer))
          ;; create
          (unless (buffer-live-p (get-buffer buf))
            (posframe-show buf
                           :string "Loading..."
                           :timeout gt-posframe-pop-render-timeout
                           :width width
                           :height height
                           :min-width width
                           :min-height height
                           :foreground-color (or forecolor (face-foreground 'tooltip nil t))
                           :background-color (or backcolor (face-background 'tooltip nil t))
                           :internal-border-width bd-width
                           :border-color (or bd-color (face-background 'posframe-border nil t))
                           :left-fringe padding
                           :right-fringe padding
                           :position (point)
                           :poshandler gt-posframe-pop-render-poshandler))
          ;; render
          (gt-buffer-render-init buf render translator)
          (posframe-refresh buf)
          ;; setup
          (with-current-buffer buf
            (gt-buffer-render-key ("q" "Close") (posframe-delete buf))))))

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
                                                            (gt-posframe-pop-render)
                                                          (gt-buffer-render))))
            (bing             . ,(gt-translator :taker (gt-taker :langs '(en zh) :text 'word :prompt t)
                                                :engines (gt-bing-engine)
                                                :render (gt-buffer-render)))
            (bing-dwim        . ,(gt-translator :taker (gt-taker :langs '(en zh) :text 'word)
                                                :engines (gt-bing-engine)
                                                :render (if (display-graphic-p)
                                                            (gt-posframe-pop-render)
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

    (defun gt-youdao-dict-translate ()
      (interactive)
      (let ((gt-default-translator (alist-get 'youdao-dict gt-preset-translators)))
        (gt-do-translate)))

    (defun gt-youdao-dict-translate-dwim ()
      (interactive)
      (let ((gt-default-translator (alist-get 'youdao-dict-dwim gt-preset-translators)))
        (gt-do-translate)))

    (defun gt-bing-translate ()
      (interactive)
      (let ((gt-default-translator (alist-get 'bing gt-preset-translators)))
        (gt-do-translate)))

    (defun gt-bing-translate-dwim ()
      (interactive)
      (let ((gt-default-translator (alist-get 'bing-dwim gt-preset-translators)))
        (gt-do-translate)))

    (defun gt-multi-dict-translate ()
      (interactive)
      (let ((gt-default-translator (alist-get 'multi-dict gt-preset-translators)))
        (gt-do-translate)))

    (defun gt-multi-dict-translate-dwim ()
      (interactive)
      (let ((gt-default-translator (alist-get 'multi-dict-dwim gt-preset-translators)))
        (gt-do-translate)))

    (defun gt-do-text-utility ()
      (interactive)
      (let ((gt-default-translator (alist-get 'Text-Utility gt-preset-translators)))
        (gt-do-translate)))))

;; OSX dictionary
(when sys/macp
  (use-package osx-dictionary
    :bind (("C-c d i" . osx-dictionary-search-input)
           ("C-c d x" . osx-dictionary-search-pointer))))

(provide 'init-dict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dict.el ends here
