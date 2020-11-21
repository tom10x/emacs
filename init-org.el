(defvar my-org-dir
  (expand-file-name "~/org/")
  "Directory with .org files.")

(defvar my-org-archive-dir
  (concat my-org-dir "org_archive/")
  "Directory where archive files are kept.")

(defvar my-org-archive-loc
  (concat my-org-archive-dir "%s_archive::")
  "Location where subtrees should be archived.
Location consists of two parts, separated by a double-colon.
The first part is the file, where %s is replaced with the current file name.
The second part is the headline, where an empty one means append to eof.")

(defvar my-org-agenda-text-search-extra-files
  (append
   (file-expand-wildcards
    (concat my-org-dir "*.org"))
   (file-expand-wildcards
    (concat my-org-archive-dir "*.org_archive")))
"List of extra files to search by text search commands.")

(defvar my-org-agenda-files
  `(,(expand-file-name (concat my-org-dir "todo.org"))
    ,(expand-file-name (concat my-org-dir "calendar.org")))
  "Files used to build agenda.")

(defvar my-org-babel-safe-languages
  '("gnuplot" "plantuml")
  "Code blocks in these languages may be evaluated without confirmation.")

(use-package org
  :ensure t
  :pin gnu
  :commands my-org-mode-hook-ispell
  :delight org-indent-mode
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("C-c [" . undefined)
         ("C-c ]" . undefined)
         ("C-c ;" . undefined))
  :init
  (setq
   org-directory my-org-dir                  ; path to .org files
   org-archive-location my-org-archive-loc   ; location to archive subtrees
   org-startup-folded t
   org-catch-invisible-edits 'show-and-error
   org-blank-before-new-entry nil
   org-log-done (quote time)
   org-log-into-drawer "LOGBOOK"
   org-outline-path-complete-in-steps nil
   org-deadline-warning-days 7
   org-ellipsis "…"
   org-tags-column 0                         ; place tags directly after headline
   org-hide-leading-stars t                  ; hide all but one star in headline
   org-hide-emphasis-markers t               ; hide markers like "*" and "="
   org-image-actual-width '(700)             ; set this width using ImageMagick, if available
   org-clone-delete-id t)

  (setq
   org-id-link-to-org-use-id
   'create-if-interactive-and-no-custom-id)

  (setq
   org-link-frame-setup
   '((file . find-file)))                    ; open files in same buffer

  (setq
   org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d!)")
     (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))

  (setq
   org-todo-state-tags-triggers
   '(("CANCELLED" ("WAITING") ("CANCELLED" . t))
     ("WAITING" ("CANCELLED") ("WAITING" . t))
     ("TODO" ("WAITING") ("CANCELLED"))
     ("DONE" ("WAITING") ("CANCELLED"))))

  (setq
   org-use-speed-commands t
   org-speed-commands-user
   '(("d" org-todo "DONE")
     ("c" org-todo "CANCELLED")
     ("s" call-interactively 'org-schedule)
     (";" call-interactively 'org-set-property)
     ("$" call-interactively 'org-archive-subtree)))

  (setq
   org-export-initial-scope 'subtree
   org-export-with-drawers '(not "LOGBOOK" "CLOCK")
   org-export-use-babel nil                         ; no code block eval during export
   org-html-postamble nil)                          ; no postamble in html export

  (setq
   org-refile-use-outline-path 'file                ; full outline path to targets
   org-refile-targets
   '(("todo.org" :maxlevel . 2)
     ("calendar.org" :maxlevel . 2)
     ("future.org" :maxlevel . 2)
     ("wiki.org" :maxlevel . 2)
     (nil :maxlevel . 1)))                          ; nil=current buffer

  (setq
   org-agenda-files my-org-agenda-files
   org-agenda-sticky t                              ; do not rebuild on every visit
   org-agenda-span 3                                ; three day agenda
   org-agenda-start-on-weekday nil                  ; start on current day
   org-agenda-skip-scheduled-if-done t              ; don't show scheduled DONE items
   org-agenda-skip-deadline-if-done t               ; don't show deadline for DONE items
   org-agenda-window-setup 'current-window          ; open agenda in current window
   org-agenda-search-view-always-boolean t          ; all search terms must match
   org-agenda-show-future-repeats nil               ; show only first of repeated entries
   org-agenda-text-search-extra-files my-org-agenda-text-search-extra-files
   org-agenda-exporter-settings '((htmlize-output-type 'css)))

  (setq
   org-agenda-fontify-priorities nil
   org-agenda-breadcrumbs-separator ""
   org-agenda-dim-blocked-tasks nil                 ; no dimming improves performance
   org-agenda-block-separator (string-to-char " ")  ; becomes a line of " " chars
   org-agenda-prefix-format
   '((agenda  . " %i %?-12t% s")                    ; leading space is important
     (todo  . " %i %-12:c")
     (tags  . " %i ")
     (search . " %i %-12:c")))

  (setq
   org-agenda-custom-commands
   '((" " "Home"
      ((agenda
        ""
        ((org-agenda-todo-keyword-format "")
         (org-agenda-remove-times-when-in-prefix nil)
         (org-agenda-scheduled-leaders '("" ""))
         (org-agenda-remove-tags t)
         (org-agenda-time-grid
          (quote ((daily today require-timed)
                  (0700 1200 1700)
                  "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))
         (org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")
         (org-agenda-sorting-strategy
          '(time-up timestamp-up priority-down category-keep))))
       (tags-todo
        "-pinned"
        ((org-agenda-overriding-header "")
         (org-tags-match-list-sublevels nil)
         (org-agenda-todo-keyword-format "")
         (org-agenda-tags-todo-honor-ignore-options t)
         (org-agenda-todo-ignore-scheduled t)
         (org-agenda-todo-ignore-deadlines t)))
       (tags
        "pinned"
        ((org-agenda-overriding-header "Pinned")
         (org-agenda-show-inherited-tags t)
         (org-agenda-sorting-strategy '(alpha-up))
         (org-agenda-hide-tags-regexp "pinned")
         (org-tags-match-list-sublevels nil)))))))

  (setq
   org-agenda-category-icon-alist
   `(("Birthday" ,(list (all-the-icons-material "cake" :height 1.2  :face 'all-the-icons-lred)))
     ("Health" ,(list (all-the-icons-material "directions_run" :height 1.2)))
     ("GameDev" ,(list (all-the-icons-material "games" :height 1.2)))
     ("Holiday" ,(list (all-the-icons-material "star" :height 1.2 :face 'all-the-icons-lred)))
     ("Appointment" ,(list (all-the-icons-material "today" :height 1.2  :face 'all-the-icons-lred)))
     ("Repeating" ,(list (all-the-icons-material "repeat" :height 1.2)))
     ("Emacs" ,(list (all-the-icons-fileicon "emacs" :height 1.2)))
     (".*" ,(list (all-the-icons-material "chevron_right" :height 1.2)))))

  (add-hook
   'org-agenda-mode-hook
   (lambda()
     (setq-local mode-name "Agenda")
     (visual-line-mode -1)
     (toggle-truncate-lines 1)))

  (org-babel-do-load-languages       ; languages enabled for evaluation (C-c C-c)
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (org . t)
     (gnuplot . t)
     (plantuml . t)))

  (setq
   org-babel-default-header-args:plantuml
   '((:results  . "file")
     (:exports  . "results")
     (:noweb    . "yes")
     (:cmdline  . "-charset UTF-8")))

  (setq
   org-babel-default-header-args:gnuplot
   '((:results  . "file")
     (:exports  . "results")
     (:noweb    . "yes")
     (:prologue . "reset")))

  (setq
   org-babel-default-header-args:zsh
   '((:results  . "output verbatim replace")))

  (setq
   org-confirm-babel-evaluate
   (lambda (lang body)
     (not (member lang my-org-babel-safe-languages))))

  (setq
   org-src-preserve-indentation t
   org-src-window-setup 'current-window  ; use current window for C-c ' editing
   org-src-fontify-natively t)           ; syntax highlighting in code blocks

  (add-to-list 'org-src-lang-modes '("plantuml" . fundamental))

  (setq
   org-capture-templates
   `(("t" "Todo" entry
      (file "todo.org") ; relative to org-directory
      "* TODO %?\n\n")
     ("a" "Appointment" entry
      (file+headline "calendar.org" "Appointment")
      "* %?\n<%(org-read-date)>\nCREATED: %U")
     ("j" "Journal" entry
      (file "journal.org")
      "* %u %?\n%U\n\n")
     ("d" "Done" entry
      (file "todo.org")
      "* DONE %?\n\n")))

  (defun my-org-mode-hook-ispell ()
    "Configure ispell for org-mode.
Important to skip certain regions, e.g. begin_src ... end_src"
    (make-local-variable 'ispell-skip-region-alist)
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
    (add-to-list 'ispell-skip-region-alist '("\\[fn:.+:" . "\\]")) ; footnotes
    (add-to-list 'ispell-skip-region-alist '("~" "~"))
    (add-to-list 'ispell-skip-region-alist '("=" "="))
    (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
    (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE" . "^#\\+END_EXAMPLE")))
  (add-hook 'org-mode-hook #'my-org-mode-hook-ispell)

  (add-hook 'org-mode-hook 'org-indent-mode)

  (add-hook 'org-mode-hook #'yas-minor-mode)

  :config
  (org-id-update-id-locations
   (file-expand-wildcards (concat my-org-dir "*.org") t)))

(defun my-org-add-ids-to-headlines-in-buffer ()
  "Add ID properties to all headlines in the current buffer which do not already have one."
  (interactive)
  (progn
    (org-map-entries 'org-id-get-create)
    "Done."))
