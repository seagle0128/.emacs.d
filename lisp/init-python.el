;; init-python.el --- Initialize python configurations.
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
;;             Python configurations.
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

;; Python Mode
(use-package python
  :defer t
  :defines gud-pdb-command-name pdb-path
  :config
  (progn
    (add-hook 'python-mode-hook
              '(lambda ()
                 (define-key python-mode-map (kbd "RET") 'newline-and-indent)))

    (add-hook 'inferior-python-mode-hook
              '(lambda ()
                 (define-key inferior-python-mode-map "\C-c\C-z" 'kill-buffer-and-window)
                 (process-query-on-exit-flag (get-process "Python"))))

    ;; fix python indent compatibility issue
    (eval-after-load 'auto-indent-mode
      (setq python-indent-guess-indent-offset nil))

    ;; iPython
    (if (executable-find "ipython")
        (setq
         python-shell-interpreter "ipython"
         python-shell-interpreter-args "-i"
         python-shell-prompt-regexp "In \\[[0-9]+\\]: "
         python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
         python-shell-completion-setup-code
         "from IPython.core.completerlib import module_completion"
         python-shell-completion-string-code
         "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

    ;; Pdb setup, note the python version
    (setq pdb-path 'pdb
          gud-pdb-command-name (symbol-name pdb-path))
    (defadvice pdb (before gud-query-cmdline activate)
      "Provide a better default command line when called interactively."
      (interactive
       (list (gud-query-cmdline pdb-path
                                (file-name-nondirectory buffer-file-name)))))

    ;; Autopep8
    (use-package py-autopep8
      :defer t
      :init (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

    ;; Anaconda
    (use-package anaconda-mode
      :defer t
      :diminish anaconda-mode
      :init
      (add-hook 'python-mode-hook 'anaconda-mode)
      :config
      (eval-after-load 'company
        '(use-package company-anaconda
           :defer t
           :init
           (add-to-list 'company-backends 'company-anaconda)))
      )))

(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
