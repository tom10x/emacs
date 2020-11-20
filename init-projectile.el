;; projectile
;;
;; Keybindings after C-c p
;; p     Display a list of known projects you can switch to.
;; f     Display a list of all files in the project. With a prefix argument it will clear the cache first.
;; l     Display a list of all files in a directory (that’s not necessarily a project)
;; s g   Run grep on the files in the project.
;; o     Runs multi-occur on all project buffers currently open.
;; r     Runs interactive query-replace on all files in the projects.
;; e     Shows a list of recently visited project files.
;; !     Runs shell-command in the root directory of the project.
;; C     Runs a standard configure command for your type of project.
;; c     Runs a standard compilation command for your type of project.
;; P     Runs a standard test command for your type of project.
;;
;; see https://docs.projectile.mx/en/latest/
(use-package projectile
  :ensure t
  :pin melpa-stable
  :init
  (setq projectile-sort-order 'recently-active)
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line-prefix " P")
  (setq projectile-switch-project-action #'projectile-find-file)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-register-project-type 'npm '("package.json")
                                    :compile "npm run build&"
                                    :test "npm run test&"
                                    :run "npm run start&"
                                    :test-suffix ".spec")
  (projectile-mode +1))
