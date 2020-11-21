(use-package ghub
  :ensure t
  :pin melpa-stable)

(use-package magit-popup
  :ensure t
  :pin melpa-stable)

(use-package git-commit
  :ensure t
  :pin melpa-stable)

(use-package magit
  :ensure t
  :pin melpa-stable
  :commands magit-dispatch
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch)))