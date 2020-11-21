(defvar my-pyvenv-dir "~/.venv" "Directory with virtual environments.")
(defvar my-pyvenv-workon "p383" "Virtual environment to use.")

(use-package python-mode
  :ensure nil
  :init
  ;; PEP8 recommends that Python code indentation be a multiple of four.
  ;; See https://www.flake8rules.com/rules/E111.html
  (setq python-indent-offset 4)
  (defun my-python-mode-hook ()
    (progn
      (lsp)
      (setq flycheck-checker 'python-flake8)
      (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
      (add-hook 'before-save-hook #'lsp-format-buffer nil 'make-it-local)))
  :hook
  (python-mode . my-python-mode-hook))

(use-package pyvenv
  :ensure t
  :pin melpa-stable
  :config
  (setenv "WORKON_HOME" my-pyvenv-dir)
  (pyvenv-workon my-pyvenv-workon))
