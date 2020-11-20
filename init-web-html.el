;; Needed by company-web
(use-package web-completion-data
  :ensure t
  :pin melpa-stable)

;; company-web
;;
;; Provides company backend company-web-html for web-mode
(use-package company-web
  :ensure t
  :pin melpa-stable)

;; web-mode
;; web-completion-data: Shared completion data for ac-html and company-web
;;
;; http://web-mode.org/
;; there is an issue setting web-mode-engines-alist in .dir-locals.el - web mode will not pick it up.
;; this .dir-locals.el content sets web-mode-engines-alist correctly but web-mode ignores it
;; ((web-mode
;;   . ((web-mode-engines-alist
;;       . '(("go" . "\\.html\\'"))))))
;; putting this in my init.el (this file) has the same effect, variable is set but web mode ignores it
;; (dir-locals-set-class-variables
;;  'hugo-project
;;  '((nil . ((web-mode-engines-alist
;;             . '(("go" . "\\.html\\'")))))))
;; (dir-locals-set-directory-class
;;  "/home/username/projects/aaa/" 'hugo-project)
;;
;; 20201113 pin to melpa instead of melpa-stable, hopefully fixes some formatting issues.
(use-package web-mode
  :ensure t
  :pin melpa
  :defines
  (web-mode-buffer-indent web-mode-buffer-fontify)
  :init
  (setq-default web-mode-markup-indent-offset my/indent-width)
  (setq-default web-mode-css-indent-offset my/indent-width)
  (setq-default web-mode-code-indent-offset my/indent-width)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-auto-expanding t) ;; e.g. d/ becomes <div>|</div>
  (setq web-mode-enable-auto-pairing nil) ;; i'm using electric-pair-mode

  ;; Problematic to define it globally like this, but .dir-locals.el and
  ;; similar solutiolns does not work with web-mode, see above.
  (setq web-mode-engines-alist
      '(("go"    . "\\.html\\'")))

  (defun my-web-mode-before-save-hook ()
    (progn
      (web-mode-buffer-indent)
      (web-mode-buffer-fontify)))

  (defun my-web-mode-hook ()
    (progn
      (set (make-local-variable 'company-backends) '((company-web-html company-files company-capf company-keywords) (company-dabbrev-code company-etags) company-dabbrev))
      (add-hook 'before-save-hook #'my-web-mode-before-save-hook nil 'make-it-local)))
  :hook
  (web-mode . my-web-mode-hook)
  :mode ("\\.html\\'"))
