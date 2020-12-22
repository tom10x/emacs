(use-package doom-themes
  :ensure t
  :pin melpa-stable
  :config
  (load-theme 'doom-dracula t)
  (set-face-attribute 'org-level-1 nil :height 1.0)
  (doom-themes-visual-bell-config)         ; flash modeline on error
  (doom-themes-org-config))                ; tweak org-mode