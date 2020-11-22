(use-package spinner
  :ensure t
  :pin gnu)

(use-package lsp-mode
  :ensure t
  :pin melpa-stable
  :commands lsp
  :init
  (setq
   lsp-completion-provider :capf
   lsp-idle-delay 0.500          ; better performance
   lsp-keymap-prefix "C-c k")
  (defun my-lsp-mode-hook ()
    (progn
      (setq-local read-process-output-max (* 1024 1024))
      (lsp-enable-which-key-integration)))
  :hook
  ((lsp-mode . my-lsp-mode-hook)))

(use-package lsp-ui
  :ensure t
  :pin melpa-stable
  :commands lsp-ui-mode
  :init
  (setq
   lsp-ui-doc-enable nil
   lsp-ui-sideline-enable nil)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions) ; M-.
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))  ; M-?
