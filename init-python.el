(defvar
  my-pyvenv-dir
  "~/.venv"
  "Directory with virtual environments.")

(defvar
  my-pyvenv-workon
  "p383"
  "Virtual environment to use.")

(defvar
  my-python-indent-default
  4
  "Default indentation offset to use when Emacs can't guess.
PEP8 recommends that Python code indentation be a multiple of four.
See https://www.flake8rules.com/rules/E111.html")

(defvar
  my-python-indent-guess-verbose
  nil
  "Don't emit a warning when indentation guessing fails.
This rarely fails in my experience, so the warning is mostly just annoying.")

;; -----------------------------------------------

(let ((offset   my-python-indent-default)
      (verbose  my-python-indent-guess-verbose))
  (setq
   python-indent-offset offset
   python-indent-guess-indent-offset-verbose verbose))

(use-package pyvenv
  :ensure t
  :pin melpa-stable
  :config
  (setenv "WORKON_HOME" my-pyvenv-dir)
  (pyvenv-workon my-pyvenv-workon))
