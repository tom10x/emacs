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
