;;; init.el --- My init file
;;; Commentary:
;; My Emacs config init file.

;;; Code:

(let ((dir "~/public/repo/emacs/"))
  (org-babel-load-file (concat dir "config.org"))
  (load (concat dir "calendar-sv"))
  (load (concat dir "org"))
  (load (concat dir "python"))
  (load (concat dir "doom-theme-dracula"))
  (load (concat dir "doom-modeline"))
  (load (concat dir "server"))
  (load (concat dir "keybindings")))

(set-frame-font "Fira Code-12")
(setq-default line-spacing 4)

(add-to-list 'org-refile-targets '("fun.org" :maxlevel . 2) 'append)

(setq org-link-abbrev-alist
      '(("gmap"      . "https://maps.google.com/maps?q=%s")
        ("gmail"     . "https://mail.google.com/mail/u/0/#inbox/%h")))

(defun my-after-init-hook-setup ()
  (progn
    (org-agenda nil " ")
    (toggle-frame-maximized)))
(add-hook 'after-init-hook #'my-after-init-hook-setup)

(provide 'init)

;;; init.el ends here