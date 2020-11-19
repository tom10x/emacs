(use-package server
  :ensure nil ;; built-in
  :commands ;; create autoloads
  server-running-p
  :functions ;; silence missing function warning
  my-server-start
  :init
  (when (equal window-system 'w32) (setq server-use-tcp t))
  (defun my-server-start ()
    "Start my Emacs server."
    (progn
      (unless (server-running-p)
        (server-start)
        ;; suppresses: Buffer `blah' still has clients; kill it?
        (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))))
  (defun my-server-restart ()
    "Restart my Emacs server."
    (interactive)
    (progn
      (server-force-delete)
      (my-server-start)))
  (add-hook 'after-init-hook #'server-start)
  ;; make emacsclient bring emacs to front
  (add-hook 'server-switch-hook #'raise-frame))