(let ((dir "~/repo/emacs/"))
  (load (concat dir "init-core"))
  ;; ... load more packages here ...
  )

(set-frame-font "Fira Code-12")
(setq-default line-spacing 4)

(add-to-list 'org-refile-targets '("fun.org" :maxlevel . 2) 'append)

(defun my-after-init-hook-setup ()
  (progn
    (org-agenda nil " ")
    (toggle-frame-maximized)))
(add-hook 'after-init-hook #'my-after-init-hook-setup)