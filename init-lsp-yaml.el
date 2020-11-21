(use-package yaml-mode
  :ensure t
  :pin melpa-stable
  :init
  (defun my-yaml-mode-hook ()
    (progn
      (lsp)
      (add-hook 'before-save-hook #'lsp-format-buffer nil 'make-it-local)))
  :hook
  (yaml-mode . my-yaml-mode-hook))
