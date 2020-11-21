(use-package json-reformat
  :ensure t
  :pin melpa-stable)

(use-package json-snatcher
  :ensure t
  :pin melpa-stable)

(use-package json-mode
  :ensure t
  :pin melpa-stable
  :init
  (defun my-json-mode-hook ()
    (progn
      (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
      (flycheck-mode +1)))
  :hook
  (json-mode . my-json-mode-hook))
