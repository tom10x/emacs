(use-package doom-themes
  :ensure t

  :pin melpa-stable

  :after (ivy)

  :config

  (load-theme 'doom-dracula t)
  (set-face-attribute 'org-level-1 nil :height 1.0)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; tweak ivy for dracula
  (custom-set-faces
   '(ivy-current-match
     ((((class color) (background light))
       :background "#ffff44" :foreground "#000000")
      (((class color) (background dark))
       :background "#ffff44" :foreground "#000000"))))

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t

  :pin melpa-stable

  :init
  (setq doom-modeline-minor-modes t) ;; otherwise Projectile info won't show
  (setq doom-modeline-env-version nil) ;; saves some space
  (setq doom-modeline-buffer-encoding nil)

  :hook (after-init . doom-modeline-mode))
