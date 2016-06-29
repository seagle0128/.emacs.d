;; init-org.el --- Initialize org configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Org configurations.
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

(use-package org
  :defer t
  :bind (("C-c a" . org-agenda))
  :init
  (progn
    (setq org-agenda-files '("~/org"))
    (setq org-src-fontify-natively t)
    (setq org-todo-keywords '((sequence "TODO" "ONGOING" "PENDING" "DONE")))
    (add-hook 'org-indent-mode-hook '(lambda () (diminish 'org-indent-mode)))

    (use-package hydra
      :config
      (progn
        (defhydra hydra-org-template (:color blue :hint nil)
          "
_c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
_l_atex   _E_xample   _p_erl          _i_ndex:
_a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
_s_rc     ^ ^         plant_u_ml      _H_TML:
_h_tml    ^ ^         ^ ^             _A_SCII:
"
          ("s" (hot-expand "<s"))
          ("E" (hot-expand "<e"))
          ("q" (hot-expand "<q"))
          ("v" (hot-expand "<v"))
          ("c" (hot-expand "<c"))
          ("l" (hot-expand "<l"))
          ("h" (hot-expand "<h"))
          ("a" (hot-expand "<a"))
          ("L" (hot-expand "<L"))
          ("i" (hot-expand "<i"))
          ("e" (hot-expand "<s" "emacs-lisp"))
          ("r" (hot-expand "<s" "ruby"))
          ("y" (hot-expand "<s" "python"))
          ("p" (hot-expand "<s" "perl"))
          ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
          ("P" (progn
                 (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
                 (hot-expand "<s" "perl")))
          ("I" (hot-expand "<I"))
          ("H" (hot-expand "<H"))
          ("A" (hot-expand "<A"))
          ("<" self-insert-command "ins")
          ("o" nil "quit"))

        (defun hot-expand (str &optional mod)
          "Expand org template."
          (let (text)
            (when (region-active-p)
              (setq text (buffer-substring (region-beginning) (region-end)))
              (delete-region (region-beginning) (region-end)))
            (insert str)
            (org-try-structure-completion)
            (when mod (insert mod) (forward-line))
            (when text (insert text))))

        (define-key org-mode-map "<"
          (lambda () (interactive)
            (if (or (region-active-p) (looking-back "^"))
                (hydra-org-template/body)
              (self-insert-command 1))))
        ))
    ))

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
