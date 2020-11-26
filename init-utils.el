(defvar
  my-secrets-file
  (concat user-emacs-directory "secrets.el.gpg")
  "Secret file.")

;; ------------------------------------------------

(defun my-recompile-elpa-and-local-repos ()
  "Recompile ELPA. It's sometimes necessary to recompile a package.
Remove the offending .elc files with e.g. `find *.elc -print0 | xargs -0 rm -f'
Then call this function."
  (interactive)
  (let ((elpa-dir (expand-file-name (concat user-emacs-directory "elpa")))
        (local-repo-dir (expand-file-name (concat user-emacs-directory "elisp")))
        (flag 0)) ;; 0=DO compile .el file without corresponding .elc file
    (progn
    (byte-recompile-directory elpa-dir flag)
    (byte-recompile-directory local-repo-dir flag))))

(defun my-recompile-init-dir ()
  "Recompile changed .el files in my init directory."
  (interactive)
  (let ((compile-even-if-elc-does-not-exist-arg 0))
    (byte-recompile-directory
     my-init-dir
     compile-even-if-elc-does-not-exist-arg)))

(defun my-copy-filename ()
  "Copy current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "File name copied: %s" filename))))

(defun my-split-window-horizontally ()
  "Split window horizontally and follow."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun my-split-window-vertically ()
  "Split window vertically and follow."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun my-kill-this-buffer ()
  "Kill current buffer without asking for confirmation.
Credit to http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/"
  (interactive)
  (kill-buffer (current-buffer)))

(defun my-load-secrets ()
  "Load secrets file."
  (interactive)
  (load my-secrets-file))
