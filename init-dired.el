(use-package dired
  :ensure nil ;; built-in

  :functions
  dired-get-filename
  dired-find-file

  :init
  (setq ls-lisp-use-insert-directory-program nil) ;; use ls-lisp.el instead of cmd line ls
  (setq ls-lisp-dirs-first t)
  ;; fix time column
  (setq ls-lisp-use-localized-time-format t)
  (setq ls-lisp-format-time-list
        '("%Y-%m-%d %H:%M"
          "%Y-%m-%d %H:%M"))
  (setq dired-listing-switches "-Alh")
  (setq dired-dwim-target t)
  (setq dired-recursive-copies (quote always)) ; “always” means no asking
  (setq dired-recursive-deletes (quote top)) ; “top” means ask once

  (defun my-dired-open-files-in-external-app ()
    "Open the file at point in Dired using an external app."
    (if (string-equal system-type "gnu/linux")
        (my-dired-open-files-in-external-app-gnu/linux)
      (dired-find-file) ))

  ;; source: https://emacs.stackexchange.com/questions/21796/dired-alternative-to-openwith-how-to-open-file-per-extension
  (defun my-dired-open-files-in-external-app-gnu/linux ()
    "Open the file at point in Dired on a GNU/Linux system. Open in external app if the
file has a special extension (.e.g pdf)."
    (interactive)
    (let* ((item (dired-get-filename))
           (itemext (downcase (concat "" (file-name-extension item)))))
      (if (member itemext '("pdf" "svg" "jpg" "jpeg" "gif" "png"))
          (let ((process-connection-type nil))
            (start-process "" nil "xdg-open" item))
        (dired-find-file))))

  ;; auto-refresh dir if dir change on disk
  (add-hook 'dired-mode-hook #'auto-revert-mode)

  :bind
  (:map dired-mode-map
        ("RET" . my-dired-open-files-in-external-app))

(use-package dired-x
  :ensure nil ;; built-in

  :init
  (when (string-equal "gnu/linux" system-type)
    (setq dired-guess-shell-alist-user
          '(("\\.pdf\\'" "xdg-open")))))
