(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")))
(setq package-archive-priorities
      '(("melpa" . -1)
        ("org"   . -1)))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(when (string-equal system-type "gnu/linux")
  (require 'iso-transl))

;; Encoding
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8)
(set-charset-priority 'unicode)
(set-language-environment "UTF-8")

(set-frame-font "Fira Code-12")  ; nicer font
(setq-default line-spacing 4)    ; font may require line space tweaking

(load "path/to/init-core.el")
