;; ghub: Minuscule client libraries for Git forge APIs.
;;
;; Required by: magit
(use-package ghub
  :ensure t
  :pin melpa-stable)

;; magit-popup: Define prefix-infix-suffix command combos
;;
;; Required by: magit
(use-package magit-popup
  :ensure t
  :pin melpa-stable)

;; git-commit: Edit Git commit messages
;;
;; Required by: magit-2.90.1
(use-package git-commit
  :ensure t
  :pin melpa-stable)

;; magit
;;
;; Most Magit commands are commonly invoked from the status buffer. It
;; can be considered the primary interface for interacting with Git
;; using Magit.
;;
;; p   prevous section
;; n   next section
;; q   quit
;; TAB fold/unfold sections
;; s   stage change at point from the working tree to the index, the change remains in the working tree
;; u   unstage change at point, remove change from the index, the change remains in the working tree
;; k   discard unstaged change at point, remove from index (if staged change) and working tree
;; v   reverse
;; C-n next line inside hunk
;; C-p next/previous line inside hunk
;; C-. select part of hunk with C-. and C-n/C-p to stage/unstage part of hunk
;; C-. select multiple files with C-. and n/p to stage/unstage multiple files at once
;; c   show commit commands
;;       C-c C-c create commit
;; P   show push commands
;;     p push
;; h   list transients
(use-package magit
  :ensure t
  :pin melpa-stable
  :commands
  magit-dispatch
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch)))