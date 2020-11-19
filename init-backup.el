;; ===== backups, auto-saves, sessions, lockfiles =====

;; - backup files (~file)
;;   Emacs creates a backup file the first time a buffer is saved, and ONLY the first time
;; - auto-save files (#file#)
;; - session files (<pid>-<hostname>~)
;;   Emacs records information about interrupted sessions in files named
;;   <pid>-<hostname>~ in the directory specified by auto-save-list-file-prefix
;; - special "symbolic link" file on windows (.#file)
;;   created on first modification of buffer visiting a file
;;   created in the same directory as the file
;;   file is removed when changes are saved
;;
;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Recover.html

(setq
 backup-directory-alist `((".*" . , (concat user-emacs-directory "backup/")))

 ;; create backup-file by renaming (faster) unless it would change the file's owner or group, then do copy instead
 backup-by-copying-when-mismatch t

 ;; delete excess backup versions silently
 delete-old-versions t

 ;; keep this many new backup files
 kept-new-versions 16

 ;; keep this many old backup files
 kept-old-versions 5

 ;; make numeric backup versions unconditionally
 version-control t

 ;; dir to store session files in
 auto-save-list-file-prefix (concat user-emacs-directory "auto-save/")

 ;; dir to store auto-save files in
 auto-save-file-name-transforms `((".*" , (concat user-emacs-directory "auto-save/") t))

 ;; backup files even if they are covered by version control
 vc-make-backup-files t

 ;; no lockfiles, I'm the only Emacs user of my files anyway
 create-lockfiles nil)