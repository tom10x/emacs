(use-package doom-themes
  :ensure t
  :pin melpa-stable
  :after (ivy)
  :config
  (load-theme 'doom-dracula t)
  (set-face-attribute 'org-level-1 nil :height 1.0)
  (doom-themes-visual-bell-config)         ; flash modeline on error
  (doom-themes-org-config)                 ; tweak org-mode
  (custom-set-faces                        ; tweak ivy for theme
   '(ivy-current-match
     ((((class color) (background light))
       :background "#ffff44" :foreground "#000000")
      (((class color) (background dark))
       :background "#ffff44" :foreground "#000000")))))

(use-package doom-modeline
  :ensure t
  :pin melpa-stable
  :init
  (setq
   doom-modeline-minor-modes t        ; show Projectile info
   doom-modeline-env-version nil      ; save space
   doom-modeline-buffer-encoding nil) ; save space
  :hook
  (after-init . doom-modeline-mode))
