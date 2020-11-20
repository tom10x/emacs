;; Markdown

(defvar my-markdown-command "pandoc" "My command to run markdown.")

;; To use all of the features of markdown-mode, you'll need to
;; install the Emacs package itself and also have a local Markdown
;; processor installed (e.g., Markdown.pl, MultiMarkdown, or Pandoc).
;;
;; see https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :ensure t
  :pin melpa-stable
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command my-markdown-command)
  (setq markdown-enable-prefix-prompts nil)
  (setq markdown-use-pandoc-style-yaml-metadata t))
