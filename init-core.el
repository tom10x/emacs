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

;; ---------------------------------------------

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
 sentence-end-double-space nil            ; not in Sweden
 mouse-highlight nil)                     ; no highlight as mouse hovers over link

(fset 'yes-or-no-p 'y-or-n-p)             ; y/n is good enough
(put 'upcase-region 'disabled nil)        ; allow upcase-region
(put 'narrow-to-region 'disabled nil)     ; allow narrow-to-region

(defalias 'list-buffers 'ibuffer)

(show-paren-mode 1)                    ; show matching parentheses
(delete-selection-mode 1)              ; replace selection with next typed char
(auto-image-file-mode 1)               ; show images on visit
(tooltip-mode 0)                       ; show help text in minibuffer, not as tooltip
(tool-bar-mode 0)                      ; no icon tool bar
(menu-bar-mode 0)                      ; no menu bar
(blink-cursor-mode 0)                  ; no blinking cursor
(column-number-mode 1)
(line-number-mode 1)
(size-indication-mode 0)               ; no buffer size info in modeline
(save-place-mode 1)                    ; go to last place in file on visit

(setq-default abbrev-mode t)

(when window-system
  (global-hl-line-mode 1))             ; highlight current line

(add-hook
 'before-save-hook
 #'delete-trailing-whitespace)

(setq-default
 indent-tabs-mode nil                  ; no tabs for indentation, use spaces
 tab-width my/indent-width            ; distance between tab stops
 sh-basic-offset my/indent-width)
(setq tab-always-indent 'complete)     ; not all modes respect this variable

(setq
 savehist-file my-savehist-file
 history-length t                      ; no truncation of history
 history-delete-duplicates t
 savehist-save-minibuffer-history t)   ; save all recorded minibuffer histories
(savehist-mode 1)

(setq
 electric-pair-preserve-balance nil
 electric-pair-skip-self t
 electric-pair-inhibit-predicate 'ignore
 electric-pair-skip-whitespace nil)
(electric-pair-mode 1)

(setq
 scroll-conservatively 10000        ; >100 so redisplay will never recenter point
 scroll-step 1                      ; lines to try to scroll when point moves out
 scroll-margin 3                    ; lines of context at top and bottom
 scroll-preserve-screen-position t  ; point stays on scroll
 hscroll-step 1)                    ; columns to scroll
(scroll-bar-mode 0)                 ; no scroll bar

(setq
 window-divider-default-places 'right-only
 window-divider-default-right-width 3)
(window-divider-mode 1)

(setq
 auto-revert-verbose nil)
(global-auto-revert-mode t)         ; revert buffer if its file changes

(global-visual-line-mode 1)

(require 'use-package)

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
  :pin gnu
  :bind
  (("M-s" . avy-goto-char-timer)))

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
  (counsel-mode 1)
  :bind
  (("C-c i u" . counsel-unicode-char)
   ("M-x" . counsel-M-x)
   ("C-t" . counsel-company)))

(use-package swiper
  :ensure t
  :pin gnu
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)))

(use-package ivy-rich
  :ensure t
  :pin melpa-stable
  :config (ivy-rich-mode 1))

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
   aw-background nil)
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

(global-set-key (kbd "C-c w f") 'my-copy-filename)
(global-set-key (kbd "C-x 2") 'my-split-window-horizontally)
(global-set-key (kbd "C-x 3") 'my-split-window-vertically)
(global-set-key (kbd "C-x k") 'my-kill-this-buffer)
(global-set-key (kbd "C-ö")  'mode-line-other-buffer)
(global-set-key (kbd "C-ä")  'ivy-switch-buffer)
(global-set-key (kbd "<f5> l") 'toggle-truncate-lines)