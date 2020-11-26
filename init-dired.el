(use-package dired
  :ensure nil
  :functions
  dired-get-filename
  dired-find-file
  my-dired-open-files-in-external-app-gnu/linux
  :init
  (setq
   ls-lisp-use-insert-directory-program nil  ; use ls-lisp.el instead of cmd line ls
   ls-lisp-dirs-first t
   ls-lisp-use-localized-time-format t
   dired-listing-switches "-Alh"
   dired-dwim-target t
   dired-recursive-copies (quote always)     ; always=don't ask
   dired-recursive-deletes (quote top))      ; top=ask once
  (setq ls-lisp-format-time-list
        '("%Y-%m-%d %H:%M"
          "%Y-%m-%d %H:%M"))
  :config
  (defun my-dired-open-files-in-external-app-gnu/linux ()
    "Open file on a GNU/Linux system."
    (interactive)
    (let* ((item (dired-get-filename))
           (itemext (downcase (concat "" (file-name-extension item)))))
      (if (member itemext '("pdf" "svg" "jpg" "jpeg" "gif" "png"))
          (let ((process-connection-type nil))
            (start-process "" nil "xdg-open" item))
        (dired-find-file))))
  (defun my-dired-open-files-in-external-app ()
    "Open the file at point in Dired in an OS appropriate way.
Open in external app if the file has a special extension.
Source: https://emacs.stackexchange.com/questions/21796/dired-alternative-to-openwith-how-to-open-file-per-extension"
    (if (string-equal system-type "gnu/linux")
        (my-dired-open-files-in-external-app-gnu/linux)
      (dired-find-file) ))
  (add-hook 'dired-mode-hook #'auto-revert-mode)
  :bind
  (:map dired-mode-map
        ("RET" . my-dired-open-files-in-external-app)))

(use-package dired-x
  :ensure nil
  :init
  (when (string-equal "gnu/linux" system-type)
    (setq dired-guess-shell-alist-user
          '(("\\.pdf\\'" "xdg-open")))))
