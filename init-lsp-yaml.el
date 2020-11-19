;; YAML
;;
;; Dependencies
;;   npm i -g yaml-language-server
;;
;; See https://emacs-lsp.github.io/lsp-mode/page/lsp-yaml/
;; See https://stable.melpa.org/#/yaml-mode
;; See https://www.emacswiki.org/emacs/YamlMode
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
