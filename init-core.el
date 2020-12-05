(defvar
  my-init-dir (expand-file-name "~/public/repo/emacs/")
  "Directory with this file and `config.org'.")

(defvar
  my/indent-width 2
  "Default number of spaces to indent each line.")

(defvar
  my-package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")
    ("melpa" . "https://melpa.org/packages/"))
  "Package archives.")

(defvar
  my-package-archive-priorities
  '(("melpa" . -1)
    ("org"   . -1))
  "Package archive priorities.")

(defvar
  my-system-time-locale
  "C"
  "Controls e.g. org-mode weekday names. `C' means: use English weekdays.")

(defvar
  my-spelling-default-dictionary
  "en_US"
  "The default dictionary to use, default is en_US = American English.")

(defvar
  my-spelling-binary
  "hunspell"
  "Binary to use, specify full path if its not on PATH.")

(defvar
  my-projectile-project-search-path
  '("~/projects/")
  "List of folders projectile will scan for projects.")

(defvar
  my-markdown-command
  "pandoc"
  "Command to run markdown.")

(let ((env "LC_COLLATE")
      (collate "sv_SE.UTF-8"))
  (unless (string-equal collate (getenv env))
    (error (format "The environment variable %s is not set correctly.
On GNU/Linux, add this to the ~/.profile file:

  export %s=%s" env env collate))))

(org-babel-load-file (concat my-init-dir "config.org"))