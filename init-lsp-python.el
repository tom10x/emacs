;; python
;;
;; Dependencies (pip using appropriate venv):
;;   pip install 'python-language-server[all]'
;;     [all] installs yapf rope pyflakes mccabe pycodestyle ...
;;   pip install flake8
;;
;; flake8 is configurable with e.g. a ".flake8" file in project root.
;; After changing config file, run M-x lsp-workspace-restart.
;;
;; See https://github.com/palantir/python-language-server
;; See https://gitlab.com/pycqa/flake8
(use-package python-mode
  :ensure nil ;; built-in
  :init
  ;; PEP8 recommends that Python code indentation be a multiple of four.
  ;; https://www.flake8rules.com/rules/E111.html
  (setq python-indent-offset 4)
  (defun my-python-mode-hook ()
    (progn
      (lsp)
      (setq flycheck-checker 'python-flake8)
      (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
      (add-hook 'before-save-hook #'lsp-format-buffer nil 'make-it-local)))
  :hook
  (python-mode . my-python-mode-hook))

;; pyvenv
(use-package pyvenv
  :ensure t
  :pin melpa-stable
  :config
  (setenv "WORKON_HOME" "~/.venv")
  (pyvenv-workon "p383"))
