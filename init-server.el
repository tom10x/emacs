(use-package server
  :ensure nil
  :commands
  server-running-p
  :functions
  my-server-start
  :init
  (defun my-server-start ()
    "Start Emacs server."
    (progn
      (unless (server-running-p)
        (server-start)
        (remove-hook                            ; Kill buffer without asking, even if
         'kill-buffer-query-functions           ; there are connected clients.
         'server-kill-buffer-query-function))))
  (defun my-server-restart ()
    "Restart Emacs server."
    (interactive)
    (progn
      (server-force-delete)
      (my-server-start)))
  :config
  (add-hook 'server-switch-hook #'raise-frame)   ; bring Emacs to front when running emacsclient cmd
  (add-hook 'after-init-hook #'my-server-start)) ; run server after init
