(use-package company-web
  :ensure t
  :pin melpa-stable)

(use-package web-mode
  :ensure t
  :pin melpa
  :defines
  web-mode-buffer-indent
  web-mode-buffer-fontify
  :init
  (setq
   web-mode-code-indent-offset my/indent-width
   web-mode-markup-indent-offset my/indent-width
   web-mode-enable-current-column-highlight t
   web-mode-enable-auto-expanding t           ; e.g. d/ becomes <div>|</div>
   web-mode-enable-auto-pairing nil)          ; using electric-pair-mode instead
  (setq web-mode-engines-alist
      '(("go"    . "\\.html\\'")))            ; must define it globally, unfortunately
  (defun my-web-mode-before-save-hook ()
    (progn
      (web-mode-buffer-indent)
      (web-mode-buffer-fontify)))
  (defun my-web-mode-hook ()
    (progn
      (set
       (make-local-variable 'company-backends)
       '((company-web-html company-files company-capf company-keywords)
         (company-dabbrev-code company-etags)
         company-dabbrev))
      (add-hook
       'before-save-hook
       #'my-web-mode-before-save-hook
       nil
       'make-it-local)))
  :hook
  (web-mode . my-web-mode-hook)
  :mode ("\\.html\\'"))
