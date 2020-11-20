;; Generic Emacs config.
;;
;; Supports Windows and Ubuntu, and probably other flavors as well.

;; Intended to be loaded from a local init.el file.

;; Customization:
(defvar my-init-dir (expand-file-name "~/public/repo/emacs/") "Directory with my init*.el files.")
(defvar my/indent-width 2 "Default number of spaces to indent each line.")

;; Make Custom write everything to separate file and do not load it.
;; If we wanted it loaded we could do: (load custom-file 'noerror)
(setq custom-file (concat user-emacs-directory "custom.el"))

;; default garbage collection threshold is very low
(setq gc-cons-threshold (* 100 1024 1024))

(setq
 tls-checktrust t
 gnutls-verify-error t)

;; Enable Emacs to use the minibuffer to query passphrases.
(setq epg-pinentry-mode 'loopback)

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

;; C-c w f
(defun my-copy-filename ()
  "Copy current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "File name copied: %s" filename))))
(global-set-key (kbd "C-c w f") 'my-copy-filename)

;; C-x 2
(defun my-split-window-horizontally ()
  "Split window horizontally and follow."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'my-split-window-horizontally)

;; C-x 3
(defun my-split-window-vertically ()
  "Split window vertically and follow."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'my-split-window-vertically)

;; C-x k
(defun my-kill-this-buffer ()
  "Kill current buffer without asking for confirmation.
Credit to http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/"
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'my-kill-this-buffer)

;; Abbreviations.
;; öö => ~
(setq
 abbrev-file-name (concat user-emacs-directory "abbrev_defs.el")
 save-abbrevs 'silent)
(setq-default abbrev-mode t) ;; always on

;; electric pair mode
(setq electric-pair-preserve-balance nil
      electric-pair-skip-self t
      electric-pair-inhibit-predicate 'ignore
      electric-pair-skip-whitespace nil)
(electric-pair-mode 1)

;; show matching parenthesis
(show-paren-mode 1)

;; always delete one char, even if tab
(setq backward-delete-char-untabify-method nil)

;; replace selection with next typed char
(delete-selection-mode 1)

;; no shift marking
(setq shift-select-mode nil)

;; delete extra whitespaces when saving
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; enable upcase-region C-x C-u
(put 'upcase-region 'disabled nil)

;; enable-narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; echo unfinished commands "immediately"
(setq echo-keystrokes 0.3)

;; no ring function
(setq ring-bell-function 'ignore)

;; dispaly image files upon visit
(auto-image-file-mode t)

;; display help text in minibuffer, not as tooltip
(tooltip-mode -1)

;; allow interactive commands from within the minibuffer
(setq enable-recursive-minibuffers t)

(defalias 'list-buffers 'ibuffer)

;; switch between buffers easily
(global-set-key (kbd "C-ö")  'mode-line-other-buffer)
(global-set-key (kbd "C-ä")  'ivy-switch-buffer)

;; no icon tool bar
(tool-bar-mode -1)

;; no menu bar
(menu-bar-mode -1)

;; no blinking cursor
(blink-cursor-mode 0)

;; stretch cursor the full width of char (e.g. tab)
(setq x-stretch-cursor t)

;; Set titlebar to show filename if available, buffer name otherwise, but...
;; ... after 30s minimized icon-title-format is used instead (on Windows anyway)
(setq
 frame-title-format "%b (%f)"
 icon-title-format "%b (%f)")

;; show column & line nbr in mode line
(column-number-mode 1)
(line-number-mode 1)

;; no buffer size display
(size-indication-mode -1)

;; automatically save place in each file.
(save-place-mode 1)

;; use system trash can when applicable
(setq delete-by-moving-to-trash t)

;; warn when opening files bigger than 100MB (default is 10 MB)
(setq large-file-warning-threshold 100000000)

(setq
 initial-scratch-message ""
 inhibit-startup-message t)

(setq
 scroll-conservatively 10000 ;; >100 so redisplay will never recenter point
 scroll-step 1               ;; lines to try to scroll when point moves out
 scroll-margin 3             ;; lines of context at top and bottom
 scroll-preserve-screen-position t ;; point stays on scroll
 hscroll-step 1)             ;; columns to scroll

(scroll-bar-mode -1)         ;; no scroll bar

;; Vertical window divider
(setq window-divider-default-right-width 3)
(setq window-divider-default-places 'right-only)
(window-divider-mode)

;; do not require "yes" or "no",  y/n is good enough
(fset 'yes-or-no-p 'y-or-n-p)

;; prevent accidental exit
(setq confirm-kill-emacs 'y-or-n-p)

;; no comfirmation on killing processes
(setq confirm-kill-processes nil)

;; no final newline
(setq require-final-newline nil)
(setq mode-require-final-newline nil)

;; auto-revert buffer if file or directory changes
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(global-auto-revert-mode t)

;; indent with spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; use this tab width unless otherwise defined
(setq-default tab-width my/indent-width)

;; TAB first indents, then complete the thing at point with function
;; completion-at-point. The completion method is determined by
;; variable completion-at-point-functions.
;;
;; NB! Not all modes respect this variable.
(setq tab-always-indent 'complete)

;; sh-mode
(setq sh-basic-offset my/indent-width)

;; double space does not end sentence in Sweden
(setq sentence-end-double-space nil) ;; so e.g. M-a and M-e works as expected

;; tramp needs some help identifying the remote prompt
(setq shell-prompt-pattern "^[^$]+$") ; works for prompts ending with a ~$~.
(when (string-equal system-type "windows-nt")
  (setq tramp-default-method "plink"))

;; no mouse highlight (in any mode)
(setq mouse-highlight nil)

;; highlight current line
(when window-system (global-hl-line-mode t))

;; F5-l
(global-set-key (kbd "<f5> l") 'toggle-truncate-lines)

;; ===== History =====

(setq
 savehist-file (concat user-emacs-directory "savehist")

 ;; no truncation
 history-length t

 ;; delete duplicates in history
 history-delete-duplicates t

 ;; save all recorded minibuffer histories
 savehist-save-minibuffer-history 1)

(savehist-mode 1)

(require 'use-package)

;; delight
;;
(use-package delight
  :ensure t
  :pin gnu
  :config
  (delight 'abbrev-mode nil t) ;; t = already loaded, use t or 'emacs with standard minor modes
  (delight 'eldoc-mode nil "eldoc")
  (delight 'auto-revert-mode nil t))

;; dash: A modern list library for Emacs
;;
;; melpa-stable version > gnu version
(use-package dash
  :ensure t
  :pin melpa-stable)

;; dash-functional: Collection of useful combinators for Emacs Lisp
;;
(use-package dash-functional
  :ensure t
  :pin melpa-stable)

;; s: The long lost Emacs string manipulation library.
;;
(use-package s
  :ensure t
  :pin melpa-stable)

;; ht: The missing hash table library for Emacs
;;
(use-package ht
  :ensure t
  :pin melpa-stable)

;; f: Modern API for working with files and directories
;;
(use-package f
  :ensure t
  :pin melpa-stable)

;; epl: Emacs Package Library
;;
(use-package epl
  :ensure t
  :pin melpa-stable)

;; bind-key: A simple way to manage personal keybindings
;;
(use-package bind-key
  :ensure t
  :pin melpa-stable)

;; async: Asynchronous processing in Emacs
;;
(use-package async
  :ensure t
  :pin melpa-stable)

;; treepy: Generic tree traversal tools
;;
(use-package treepy
  :ensure t
  :pin melpa-stable)

;; lv: Other echo area
;;
(use-package lv
  :ensure t
  :pin melpa-stable)

;; memoize: Memoization functions
;;
(use-package memoize
  :ensure t
  :pin melpa-stable)

(use-package emacs
  :ensure nil ;; very built-in
  :delight visual-line-mode ; remove "Wrap" from mode-line
  :config
  (global-visual-line-mode 1))

;; shrink-path: fish-style path
;;
(use-package shrink-path
  :ensure t
  :pin melpa-stable)

;; all-the-icons: A library for inserting Developer icons
;;
(use-package all-the-icons
  :ensure t
  :pin melpa-stable)

;; pkg-info: Information about packages
;;
(use-package pkg-info
  :ensure t
  :pin melpa-stable)

;; with-editor: Use the Emacsclient as $EDITOR
;;
(use-package with-editor
  :ensure t
  :pin melpa-stable)

;; spinner: Add spinners and progress-bars to the mode-line for ongoing operations
;;
;; Needed by lsp-mode
(use-package spinner
  :ensure t
  :pin gnu)

;; snippets go in ~/.emacs.d/snippets and take precedence
;; yas-describe-tables lists available snippets
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
  ;; NB! Can't hook into org-mode here, must do that inside the "use-package org" block below
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package recentf
  :ensure nil ;; built-in
  :init
  (setq
   recentf-save-file (concat user-emacs-directory "recentf")
   recentf-max-saved-items 1000
   recentf-auto-cleanup 'never)
  (run-at-time nil 600
               (lambda ()
                 (let ((inhibit-message t))
                   (recentf-save-list))))
  :config
  (recentf-mode t))

;; avy is used by ivy if installed
(use-package avy
  :ensure t
  :pin gnu
  :bind
  (("M-s" . avy-goto-char-timer)))

;; amx - easy access to recently and most frequently used commands
;;
;; used by ivy if installed
(use-package amx
  :ensure t
  :pin melpa-stable)

;; ivy -- generic completion frontend (selection from list)
;;        https://github.com/abo-abo/swiper
;;        http://oremacs.com/swiper/
;;        C-o m toggles the current regexp builder.
;;
;; (ivy-mode +1) sets completing-read-function to ivy-completing-read.

(use-package ivy
  :ensure t
  :pin gnu
  :delight ivy-mode
  :init
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 20)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-use-selectable-prompt t) ;; C-p and enter to accept input as-is
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  (setq ivy-display-style 'fancy)
  :config
  (ivy-mode +1))

;; counsel -- collection of Ivy-enhanced versions of common Emacs commands
(use-package counsel
  :ensure t
  :pin gnu
  :delight counsel-mode
  :defer 1
  :config
  (counsel-mode +1)
  :bind
  (("C-c i u" . counsel-unicode-char)
   ("M-x" . counsel-M-x)
   ("C-t" . counsel-company)))

;; swiper -- isearch with an overview (incremental search)
;;
;; notable minibuffer key bindings
;;   M-i ivy-insert-current insert current candidate into the minibuffer
;;   C-' ivy-avy select candidate with ivy
;;
;; see https://github.com/abo-abo/swiper
(use-package swiper
  :ensure t
  :pin gnu
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)))

;; ivy-rich -- rich info for ivy searches
;;
;; see https://github.com/Yevgnen/ivy-rich
(use-package ivy-rich
  :ensure t
  :pin melpa-stable
  :config (ivy-rich-mode 1))

;; helpful -- nicer emacs help
;;
;; see https://github.com/Wilfred/helpful
(use-package helpful
  :ensure t
  :pin melpa-stable
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

;; ace-window
;;
;; Using melpa because current version is 0.9.0 released 2015, with 97 commits since then.
;;
;; see https://github.com/abo-abo/ace-window
(use-package ace-window
  :ensure t
  :pin melpa
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame)
  (setq aw-background nil)
  :bind
  (("M-o" . ace-window)))

;; company -- text completion framework
;;
;; `company-begin-backend' can be used to start a specific backend
;; M-n / M-p cycles through options
;; M-(digit) completes one of the first 10 candidates.
;;
;; Stops at the first backend with a result.
;;
;; backends built-in:
;;   company-bbdb          - BBDB (contact management utility) in message-mode (editing mail and news)
;;   company-capf        +++ completion-at-point-functions, supports any major mode with a proper completion function
;;   company-clang         - Clang: c-mode c++-mode objc-mode
;;   company-cmake         - Cmake: cmake-mode
;;   company-dabbrev       + dynamic abbrev, not mode-specific, key bindings `M-/' and `C-M-/'
;;   company-dabbrev-code  + dynamic abbrev for code symbols: prog-mode batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode lua-mode python-mode (and derivatives)
;;   company-eclim         - Eclim (binary) access to Eclipse Java IDE features. Recommendation is to use emacs-eclim instead.
;;   company-etags         + access etags (names => definitions): prog-mode c-mode objc-mode c++-mode java-mode jde-mode pascal-mode perl-mode python-mode
;;   company-files         + file names, not mode-specific
;;   company-gtags         - GNU Global, I currently don't have it installed
;;   company-keywords      + programming language keywords, supports many modes (see source code)
;;   company-oddmuse       - oddmuse-mode (edit Oddmuse wikis)
;;   company-semantic      ? semantic-mode (enables convenience functions for some programming languages, global minor mode), probably "-"
;;   company-xcode         - Xcode stuff
(use-package company
  :ensure t
  :pin melpa-stable ;; in gnu, but company-web is not
  :delight company-mode
  :init
  (setq company-idle-delay 0.5)
  (setq company-minimum-prefix-length 3)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case 'ignore-case-in-candidate-selection)
  (setq company-tooltip-align-annotations t)

  ;; Adjust company-backends as needed for the different modes
  (setq company-backends '((company-files company-keywords company-capf) (company-dabbrev-code company-etags) company-dabbrev))

  :config
  (global-company-mode +1)
  :bind
  (:map company-active-map
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)))

;; flycheck provides syntax checking via external external programs or
;; services. Multiple checkers can be run in sequence, see
;; `flycheck-add-next-checker'.
;;
;; Standard tool for linting.
;;
;; Not using global-flycheck-mode, prefer to activate it when needed.
;;
;; melpa, because latest version is 31, which was released in 2017,
;; and there's been 570+ commits since then.
;;
;; C-c ! v   verify setup
;; C-c ! ?   describe checker
;; C-c ! l   show error list
;; M-g n     go to next error
;; M-g p     go to previous error
;;
;; See https://www.flycheck.org
;; https://github.com/flycheck/flycheck
(use-package flycheck
  :ensure t
  :pin melpa)

(use-package which-key
  :ensure t
  :pin melpa-stable ;; melpa-stable version > gnu
  :delight which-key-mode
  :config
  (which-key-mode))
