(defvar
  my-init-dir (expand-file-name "~/public/repo/emacs/")
  "Directory with my init*.el files.")

(defvar
  my/indent-width 2
  "Default number of spaces to indent each line.")

(defvar
  my-custom-file
  (concat user-emacs-directory "custom.el")
  "Custom file.")

(defvar
  my-abbrev-file
  (concat user-emacs-directory "abbrev_defs.el")
  "Abbreviation file.")

(defvar
  my-savehist-file
  (concat user-emacs-directory "savehist")
  "Minibuffer history save file.")

(defvar
  my-recentf-save-file
  (concat user-emacs-directory "recentf")
  "Recent list save file.")

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
  my-collate
  "sv_SE.UTF-8"
  "Collate to use when sorting, for example.")

;; -----------------------------------------------

(let ((env "LC_COLLATE"))
  (unless (string-equal my-collate (getenv env))
    (error (format "The environment variable %s is not set correctly.
On GNU/Linux, add this to your ~/.profile file:

  export %s=%s" env env my-collate))))

(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-charset-priority 'unicode)
(set-language-environment "UTF-8")

(setq
 gc-cons-threshold (* 100 1024 1024)      ; boost garbage collection threshold
 custom-file my-custom-file               ; separate file and don't load it
 tls-checktrust t
 gnutls-verify-error t
 epg-pinentry-mode 'loopback              ; allow query of passphrases in minibuffer
 abbrev-file-name my-abbrev-file
 save-abbrevs 'silent
 backward-delete-char-untabify-method nil ; backward delete on tab simply deletes the tab
 shift-select-mode nil                    ; seems weird, don't want it
 echo-keystrokes 0.3                      ; echo unfinished commands "immediately"
 ring-bell-function 'ignore               ; don't ring the bell
 enable-recursive-minibuffers t           ; allow interactive commands in minibuffer
 x-stretch-cursor t                       ; stretch cursor over char, nice for tabs
 delete-by-moving-to-trash t              ; use system trash when applicable
 large-file-warning-threshold 100000000   ; boost big file warning threshold
 initial-scratch-message ""
 inhibit-startup-message t
 frame-title-format "%b (%f)"             ; show buffername and filename in frame title ...
 icon-title-format "%b (%f)"              ; ... and in iconified frame title
 confirm-kill-emacs 'y-or-n-p             ; prevent accidental exit
 confirm-kill-processes nil               ; no comfirmation on killing processes
 require-final-newline nil                ; no final newline
 mode-require-final-newline nil           ; no final newline, really
 sentence-end-double-space nil            ; no, just no
 mouse-highlight nil)                     ; no highlight as mouse hovers over link

(fset 'yes-or-no-p 'y-or-n-p)             ; y/n is good enough
(put 'upcase-region 'disabled nil)        ; allow upcase-region
(put 'narrow-to-region 'disabled nil)     ; allow narrow-to-region

(defalias 'list-buffers 'ibuffer)

(show-paren-mode 1)                       ; show matching parentheses
(delete-selection-mode 1)                 ; replace selection with next typed char
(auto-image-file-mode 1)                  ; show images on visit
(tooltip-mode 0)                          ; show help text in minibuffer, not as tooltip
(tool-bar-mode 0)                         ; no icon tool bar
(menu-bar-mode 0)                         ; no menu bar
(blink-cursor-mode 0)                     ; no blinking cursor
(column-number-mode 1)
(line-number-mode 1)
(size-indication-mode 0)                  ; no buffer size info in modeline
(save-place-mode 1)                       ; go to last place in file on visit
(global-visual-line-mode 1)

(setq-default abbrev-mode t)

(when window-system
  (global-hl-line-mode 1))                ; highlight current line

(add-hook
 'before-save-hook
 #'delete-trailing-whitespace)

(setq-default
 indent-tabs-mode nil                     ; no tabs for indentation, use spaces
 tab-width my/indent-width                ; distance between tab stops
 sh-basic-offset my/indent-width)
(setq tab-always-indent 'complete)        ; not all modes respect this variable

(setq
 savehist-file my-savehist-file
 history-length t                         ; no truncation of history
 history-delete-duplicates t
 savehist-save-minibuffer-history t)      ; save all recorded minibuffer histories
(savehist-mode 1)

(setq
 electric-pair-preserve-balance nil
 electric-pair-skip-self t
 electric-pair-inhibit-predicate 'ignore
 electric-pair-skip-whitespace nil)
(electric-pair-mode 1)

(setq
 scroll-conservatively 10000              ; >100 so redisplay will never recenter point
 scroll-step 1                            ; lines to try to scroll when point moves out
 scroll-margin 3                          ; lines of context at top and bottom
 scroll-preserve-screen-position t        ; point stays on scroll
 hscroll-step 1)                          ; columns to scroll
(scroll-bar-mode 0)                       ; no scroll bar

(setq
 window-divider-default-places 'right-only
 window-divider-default-right-width 3)
(window-divider-mode 1)

(setq
 auto-revert-verbose nil)
(global-auto-revert-mode t)               ; revert buffer if its file changes

;; -----------------------------------------------

(require 'package)
(setq
 package-archives my-package-archives
 package-archive-priorities my-package-archive-priorities)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; -----------------------------------------------

(use-package delight
  :ensure t
  :pin gnu
  :config
  (delight 'abbrev-mode nil t)
  (delight 'visual-line-mode nil t)
  (delight 'eldoc-mode nil "eldoc")
  (delight 'auto-revert-mode nil t))

(use-package dash
  :ensure t
  :pin melpa-stable)

(use-package dash-functional
  :ensure t
  :pin melpa-stable)

(use-package s
  :ensure t
  :pin melpa-stable)

(use-package ht
  :ensure t
  :pin melpa-stable)

(use-package f
  :ensure t
  :pin melpa-stable)

(use-package epl
  :ensure t
  :pin melpa-stable)

(use-package bind-key
  :ensure t
  :pin melpa-stable)

(use-package async
  :ensure t
  :pin melpa-stable)

(use-package treepy
  :ensure t
  :pin melpa-stable)

(use-package lv
  :ensure t
  :pin melpa-stable)

(use-package memoize
  :ensure t
  :pin melpa-stable)

(use-package shrink-path
  :ensure t
  :pin melpa-stable)

(use-package all-the-icons
  :ensure t
  :pin melpa-stable)

(use-package pkg-info
  :ensure t
  :pin melpa-stable)

(use-package with-editor
  :ensure t
  :pin melpa-stable)

(use-package yasnippet
  :ensure t
  :pin gnu
  :commands
  yas-reload-all
  :delight yas-minor-mode
  :mode
  ("\\.yasnippet\\'" . snippet-mode)
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package recentf
  :ensure nil
  :init
  (setq
   recentf-save-file my-recentf-save-file
   recentf-max-saved-items 1000
   recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1)
  (run-at-time
   nil
   600
   (lambda ()
     (let ((inhibit-message t))
       (recentf-save-list)))))

(use-package avy
  :ensure t
  :pin gnu)

(use-package amx
  :ensure t
  :pin melpa-stable)

(use-package ivy
  :ensure t
  :pin gnu
  :delight ivy-mode
  :init
  (setq
   ivy-count-format "(%d/%d) "
   ivy-height 20
   ivy-use-virtual-buffers t
   ivy-virtual-abbreviate 'full
   ivy-use-selectable-prompt t
   ivy-display-style 'fancy
   ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :pin gnu
  :delight counsel-mode
  :defer 1
  :config
  (counsel-mode 1))

(use-package swiper
  :ensure t
  :pin gnu)

(use-package ivy-rich
  :ensure t
  :pin melpa-stable
  :config
  (ivy-rich-mode 1))

(use-package helpful
  :ensure t
  :pin melpa-stable
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

(use-package ace-window
  :ensure t
  :pin melpa
  :init
  (setq
   aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
   aw-scope 'frame
   aw-background nil))

(use-package company
  :ensure t
  :pin melpa-stable
  :delight company-mode
  :init
  (setq
   company-idle-delay 0.5
   company-minimum-prefix-length 3
   company-dabbrev-downcase nil
   company-dabbrev-ignore-case 'ignore-case-in-candidate-selection
   company-tooltip-align-annotations t
   company-backends
   '((company-files company-keywords company-capf)
     (company-dabbrev-code company-etags)
     company-dabbrev))
  :config
  (global-company-mode 1)
  :bind
  (:map company-active-map
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)))

(use-package flycheck
  :ensure t
  :pin melpa)

(use-package which-key
  :ensure t
  :pin melpa-stable
  :delight which-key-mode
  :config
  (which-key-mode))

;; -----------------------------------------------

(use-package yaml-mode
  :ensure t
  :pin melpa-stable)
