(use-package projectile
  :ensure t
  :pin melpa-stable
  :init
  (setq
   projectile-switch-project-action #'projectile-find-file
   projectile-mode-line-prefix " P"
   projectile-completion-system 'ivy
   projectile-sort-order 'recently-active)
  :config
  (projectile-register-project-type 'npm
                                    '("package.json")
                                    :compile "npm run build&"
                                    :test "npm run test&"
                                    :run "npm run start&"
                                    :test-suffix ".spec")
  (projectile-mode +1))
