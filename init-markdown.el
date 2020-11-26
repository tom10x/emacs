(defvar
  my-markdown-command
  "pandoc"
  "Command to run markdown.")

(use-package markdown-mode
  :ensure t
  :pin melpa-stable
  :init
  (setq
   markdown-command my-markdown-command
   markdown-enable-prefix-prompts nil
   markdown-use-pandoc-style-yaml-metadata t)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))
