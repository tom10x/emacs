;; lsp-mode: client/library for the Language Server Protocol. Uses
;; flycheck for linting, and company-capf for code completion.
;;
(use-package lsp-mode
  :ensure t
  :pin melpa-stable

  ;; 2020-07-09: 7.0 released which most likely has the fix, so getting rid of local copy of main branch.
  ;; 2020-06-30: switching to current main branch
  ;; compile with:
  ;; (add-to-list 'load-path (expand-file-name "~/prg/lsp-mode-20200630"))
  ;; (byte-recompile-directory "~/prg/lsp-mode-20200630" 0)
  ;; :ensure nil
  ;; :load-path "~/prg/lsp-mode-20200630"

  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c k")
  (setq lsp-idle-delay 0.500) ;; better performance, see https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq lsp-completion-provider :capf) ;; in lsp-completion.el
  (defun my-lsp-mode-hook ()
    (progn
      (setq-local read-process-output-max (* 1024 1024)) ;; 1 mb
      (lsp-enable-which-key-integration)))
  :hook ((lsp-mode . my-lsp-mode-hook)))

(use-package lsp-ui
  :ensure t
  :pin melpa-stable
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions) ;; M-.
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))  ;; M-?
