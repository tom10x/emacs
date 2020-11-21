(defvar
  my-backup-directory-alist
  `((".*" . , (concat user-emacs-directory "backup/")))
    "File name patterns and where to backup those files.")

(defvar
  my-auto-save-file-name-transforms
  `((".*" , (concat user-emacs-directory "auto-save/") t))
  "File name patterns and where to auto-save those files.")

(defvar
  my-auto-save-list-file-prefix
  (concat user-emacs-directory "auto-save/")
  "Prefix that we use to determine directory of session files.")

(setq
 backup-by-copying-when-mismatch t ; try to preserve owner and group
 delete-old-versions t             ; delete excess backup versions silently
 version-control t                 ; make numeric backup versions unconditionally
 vc-make-backup-files t            ; do backup files under version control
 create-lockfiles nil              ; no lockfiles, I'm the only user
 kept-new-versions 16
 kept-old-versions 5
 backup-directory-alist my-backup-directory-alist
 auto-save-list-file-prefix my-auto-save-list-file-prefix
 auto-save-file-name-transforms my-auto-save-file-name-transforms)