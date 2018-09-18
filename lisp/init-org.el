;; init-org.el --- Initialize org configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Vincent Zhang

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
;; Org configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package org
  :ensure nil
  :commands org-try-structure-completion
  :functions hydra-org-template/body
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb))
  :hook ((org-mode . org-indent-mode)
         (org-indent-mode . (lambda() (diminish 'org-indent-mode))))
  :config
  (setq org-agenda-files '("~/org"))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")))
  (setq org-log-done 'time)
  (setq org-src-fontify-natively t)
  (add-to-list 'org-export-backends 'md)

  ;; More fancy UI
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode))

  (unless sys/win32p
    (use-package org-fancy-priorities
      :diminish
      :defines org-fancy-priorities-list
      :hook (org-mode . org-fancy-priorities-mode)
      :config (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))))

  ;; Babel
  (setq org-confirm-babel-evaluate nil)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (plantuml . t)))
  (use-package ob-go
    :init
    (if (executable-find "go")
        (cl-pushnew '(go . t) load-language-list)))
  (use-package ob-ipython
    :init
    (if (executable-find "jupyter")
        (cl-pushnew '(ipython . t) load-language-list)))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; Preview
  (use-package org-preview-html
    :diminish org-preview-html-mode)

  ;; Presentation
  (use-package org-tree-slide
    :functions (org-display-inline-images org-remove-inline-images)
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-set 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-set 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1)))))

  ;; Pomodoro
  (use-package org-pomodoro
    :after org-agenda
    :bind (:map org-agenda-mode-map
                ("P" . org-pomodoro)))

  ;; Visually summarize progress
  (use-package org-dashboard)

  (with-eval-after-load 'hydra
    (eval-and-compile
      (defun hot-expand (str &optional mod)
        "Expand org template."
        (let (text)
          (when (region-active-p)
            (setq text (buffer-substring (region-beginning) (region-end)))
            (delete-region (region-beginning) (region-end)))
          (insert str)
          (org-try-structure-completion)
          (when mod (insert mod) (forward-line))
          (when text (insert text)))))

    (defhydra hydra-org-template (:color blue :hint nil)
      "
_c_enter  qu_o_te     _e_macs-lisp    _L_aTeX:
_l_atex   _E_xample   p_y_thon        _i_ndex:
_a_scii   _v_erse     ip_Y_thon       _I_NCLUDE:
_s_rc     _g_o        _r_uby          _H_TML:
_h_tml    _S_HELL     _p_erl          _A_SCII:
^ ^       ^ ^         _P_erl tangled  plant_u_ml
"
      ("s" (hot-expand "<s"))
      ("E" (hot-expand "<e"))
      ("o" (hot-expand "<q"))
      ("v" (hot-expand "<v"))
      ("c" (hot-expand "<c"))
      ("l" (hot-expand "<l"))
      ("h" (hot-expand "<h"))
      ("a" (hot-expand "<a"))
      ("L" (hot-expand "<L"))
      ("i" (hot-expand "<i"))
      ("e" (hot-expand "<s" "emacs-lisp"))
      ("y" (hot-expand "<s" "python :results output"))
      ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0"))
      ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)"))
      ("p" (hot-expand "<s" "perl"))
      ("r" (hot-expand "<s" "ruby"))
      ("S" (hot-expand "<s" "sh"))
      ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
      ("P" (progn
             (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
             (hot-expand "<s" "perl")))
      ("I" (hot-expand "<I"))
      ("H" (hot-expand "<H"))
      ("A" (hot-expand "<A"))
      ("<" self-insert-command "ins")
      ("q" nil "quit"))

    (bind-key "<"
              (lambda () (interactive)
                (if (or (region-active-p) (looking-back "^" 1))
                    (hydra-org-template/body)
                  (self-insert-command 1)))
              org-mode-map)))

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
