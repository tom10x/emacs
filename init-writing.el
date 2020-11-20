(use-package olivetti
  :ensure t
  :pin melpa-stable ;; obsolete in gnu
  :delight olivetti-mode)

(define-derived-mode my-write-mode org-mode "my-write"
  "Major mode for writing."
  (progn
    (setq-local org-startup-indented nil)
    (company-mode -1)
    (local-set-key (kbd "\"") (lambda () (interactive) (insert "“”")))
    (olivetti-mode t)))
