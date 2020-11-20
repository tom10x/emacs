;; org-mode
;;
(use-package org
  :ensure t
  :pin gnu

  :delight org-indent-mode

  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("C-c [" . undefined)
         ("C-c ]" . undefined)
         ("C-c ;" . undefined))

  ;; INIT: executed before module is loaded
  ;; restrict to code that would work even if loading of module would fail
  :init
  (setq org-startup-folded t)

  (setq org-directory (expand-file-name "~/org/"))

  ;; safety
  (setq org-catch-invisible-edits 'show-and-error)

  ;; editing
  (setq org-blank-before-new-entry nil)
  ;; (setq org-insert-heading-respect-content t) ; insert new headings AFTER current subtree
  (setq org-clone-delete-id t)
  (setq org-archive-location (concat org-directory "org_archive/%s_archive::"))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d!)")
          (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))
  (setq org-todo-state-tags-triggers ; e.g: (state ("remove-tag") ("add-tag" t))
         '(("CANCELLED" ("WAITING") ("CANCELLED" . t))
          ("WAITING" ("CANCELLED") ("WAITING" . t))
          ("TODO" ("WAITING") ("CANCELLED"))
          ("DONE" ("WAITING") ("CANCELLED"))))
  (setq org-log-done (quote time)) ; record time when task moves to the DONE state
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-use-speed-commands t)
  (setq org-speed-commands-user
        '(("d" org-todo "DONE")
          ("c" org-todo "CANCELLED")
          ("s" call-interactively 'org-schedule)
          (";" call-interactively 'org-set-property)
          ("$" call-interactively 'org-archive-subtree)))

  ;; completion
  (setq org-outline-path-complete-in-steps nil) ;; nil best when used with completion pkg

  ;; scheduling
  (setq org-deadline-warning-days 7) ; show deadlines in agenda X days before due date

  ;; export
  ;; see https://orgmode.org/manual/Export-settings.html
  ;; see https://orgmode.org/manual/The-export-dispatcher.html
  (setq org-export-initial-scope 'subtree)
  (setq org-export-with-drawers '(not "LOGBOOK" "CLOCK"))
  (setq org-export-use-babel nil) ;; do not evaluate code blocks during export
  (setq org-html-postamble nil) ; no post-amble

  ;; appearance
  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers t)
  (setq org-image-actual-width '(700))
  (setq org-ellipsis "…")
  (setq org-tags-column 0)

  (defun my-org-add-ids-to-headlines-in-buffer ()
    "Add ID properties to all headlines in the current buffer which do not already have one."
    (interactive)
    (progn
      (org-map-entries 'org-id-get-create)
      "Done."))

  ;; refile
  (setq org-refile-targets
        '(("todo.org" :maxlevel . 2)
          ("future.org" :maxlevel . 2)
          ("wiki.org" :maxlevel . 2)
          ("calendar.org" :maxlevel . 2)
          (nil :maxlevel . 1))) ; nil=headings in current buffer
  (setq org-refile-use-outline-path 'file) ; full outline path for refile targets
  (setq org-refile-allow-creating-parent-nodes (quote confirm)) ; refile creates parent tasks on confirmation

  ;; org link follow setup (default except file uses find-file not find-file-other-window)
  (setq org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                               (vm-imap . vm-visit-imap-folder-other-frame)
                               (gnus . org-gnus-no-new-news)
                               (file . find-file)
                               (wl . wl-other-frame)))

  ;; agenda
  (setq org-agenda-sticky t) ; do not rebuild agenda every time
  (setq org-agenda-span 3)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-block-separator (string-to-char " "))
  (setq org-agenda-dim-blocked-tasks nil) ; don't dim blocked tasks (performance)
  (setq org-agenda-window-setup 'current-window) ; open agenda in current window
  (setq org-agenda-todo-ignore-with-date nil) ; Keep tasks w/ dates on global todo list
  (setq org-agenda-todo-ignore-deadlines nil) ; Keep tasks w/ deadlines on global todo list
  (setq org-agenda-todo-ignore-scheduled nil) ; Keep tasks w/ scheduled dates on global todo list
  (setq org-agenda-todo-ignore-timestamp nil) ; Keep tasks w/ timestamps on global todo lists
  (setq org-agenda-tags-todo-honor-ignore-options t) ; honor above 4 vars
  (setq org-agenda-search-view-always-boolean t) ; t=org mode no longer requires + to do boolean searches
  (setq org-agenda-files '())
  (dolist (fname (list "todo.org" "calendar.org"))
    (let* ((f (concat org-directory fname))) (if (file-exists-p f) (add-to-list 'org-agenda-files f t))))
  ;; extra files to include in text searches (C-c a s)
  (setq org-agenda-text-search-extra-files
        (append (file-expand-wildcards (concat org-directory "*.org"))
                (file-expand-wildcards (concat org-directory "org_archive/*.org_archive"))))
  (setq org-agenda-exporter-settings
        '((htmlize-output-type 'css)))
  (setq org-agenda-prefix-format
        '((agenda  . " %i %?-12t% s") ;; the leading space is important for some reason
          (todo  . " %i %-12:c")
          (tags  . " %i ")
          (search . " %i %-12:c")))
  (add-hook 'org-agenda-mode-hook
            (lambda()
              ;; mode-name: Pretty name of current buffer's major mode
              (setq mode-name "Agenda")
              (visual-line-mode -1)     ;; disable word-wrapping
              (toggle-truncate-lines 1))) ;; no line break
  (setq org-agenda-show-future-repeats nil)
  (setq org-agenda-prefer-last-repeat t)
  (setq org-agenda-breadcrumbs-separator "")
  (setq org-agenda-fontify-priorities nil)
  (setq org-agenda-custom-commands
        '(
          ;; Default agenda
          ;;
          ;; 2020-10-08 " " works again in org-version 9.4!
          ;; https://code.orgmode.org/bzg/org-mode/commit/2508dfa6
          (" " "Home"
           (
            (agenda ""
                    ((org-agenda-todo-keyword-format "")
                     (org-agenda-remove-times-when-in-prefix nil)
                     (org-agenda-scheduled-leaders '("" ""))
                     (org-agenda-remove-tags t)
                     (org-agenda-time-grid (quote ((daily today require-timed)
                                                   (0700 1200 1700)
                                                   "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))
                     (org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")
                     (org-agenda-sorting-strategy '(time-up timestamp-up priority-down category-keep))))

            (tags-todo "-pinned"
                       ((org-agenda-overriding-header "")
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-todo-keyword-format "")
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-todo-ignore-deadlines t)))

            (tags "pinned"
                  ((org-agenda-overriding-header "Pinned")
                   (org-agenda-show-inherited-tags t)
                   (org-agenda-sorting-strategy '(alpha-up))
                   (org-agenda-hide-tags-regexp "pinned")
                   (org-tags-match-list-sublevels nil)))
            )) ;; " " "Home" ends here
          )) ;; org-agenda-custom-commands ends here
  (setq org-agenda-category-icon-alist
        `(("Birthday" ,(list (all-the-icons-material "cake" :height 1.2  :face 'all-the-icons-lred)))
          ("Health" ,(list (all-the-icons-material "directions_run" :height 1.2)))
          ("GameDev" ,(list (all-the-icons-material "games" :height 1.2)))
          ("Holiday" ,(list (all-the-icons-material "star" :height 1.2 :face 'all-the-icons-lred)))
          ("Appointment" ,(list (all-the-icons-material "today" :height 1.2  :face 'all-the-icons-lred)))
          ("Repeating" ,(list (all-the-icons-material "repeat" :height 1.2)))
          ("Emacs" ,(list (all-the-icons-fileicon "emacs" :height 1.2)))
          (".*" ,(list (all-the-icons-material "chevron_right" :height 1.2)))))

  ;; show images after code block execution
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images)
  ;; languages enabled for evaluation (C-c C-c)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (ditaa . t)
     (dot . t)
     (shell . t)
     (latex . t)
     (R . t)
     (org . t)
     (gnuplot . t)
     (plantuml . t)))
  ;; don't ask for confirmation for these languages, they're safe
  (defun my-org-confirm-babel-evaluate (lang body)
    "Check if language LANG should be disabled, ignore BODY."
    (not (member lang '("dot" "ditaa" "gnuplot" "R" "plantuml"))))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  ;; Edit PlantUML code blocks in fundamental mode
  (add-to-list 'org-src-lang-modes '("plantuml" . fundamental))
  ;; syntax highlighting in buffer src blocks
  (setq org-src-fontify-natively t)
  ;; Use current window for C-c ' source editing
  (setq org-src-window-setup 'current-window)
  ;; Preserve leading whitespace characters on export.
  ;;   If this variable is nil then after editing with M-x
  ;; org-edit-src-code, the minimum (across-lines) number of leading
  ;; whitespace characters are removed from all lines, and the code
  ;; block is uniformly indented according to the value of
  ;; `org-edit-src-content-indentation'.
  (setq org-src-preserve-indentation t)
  ;; Defaults of src code block header
  ;;   :results "file" (already default) = result is saved to a file
  ;;   :exports "results" (already default) = export result, not code
  ;;   :noweb "yes" = allow noweb reference syntax
  ;;   :cmdline "-charset UTF-8" = otherwise Swedish chars aren't rendered properly
  ;;   :prologue "reset" = prepend this to the code block prior to execution
  ;;     (e.g. we always want reset in all gnuplot code blocks anyway)
  (setq org-babel-default-header-args:plantuml '((:results . "file") (:exports . "results") (:noweb . "yes") (:cmdline . "-charset UTF-8")))
  (setq org-babel-default-header-args:gnuplot '((:results . "file") (:exports . "results") (:noweb . "yes") (:prologue . "reset")))
  (setq org-babel-default-header-args:R '((:results . "output graphics") (:exports . "results") (:noweb . "yes")))
  (setq org-babel-default-header-args:zsh '((:results . "output verbatim replace")))

  (setq org-capture-templates
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

  ;; spelling
  ;;   Skip spell checking for some org-mode regions, e.g. BEGIN_SRC ... END_SRC
  (defun my-org-ispell ()
    "Configure `ispell-skip-region-alist' for `org-mode'."
    (make-local-variable 'ispell-skip-region-alist)
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
    (add-to-list 'ispell-skip-region-alist '("\\[fn:.+:" . "\\]")) ; footnotes
    (add-to-list 'ispell-skip-region-alist '("~" "~"))
    (add-to-list 'ispell-skip-region-alist '("=" "="))
    (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
    (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE" . "^#\\+END_EXAMPLE")))
  (add-hook 'org-mode-hook #'my-org-ispell)

  (add-hook 'org-mode-hook 'org-indent-mode)

  (add-hook 'org-mode-hook #'yas-minor-mode)

  :config
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-id-update-id-locations (file-expand-wildcards (concat org-directory "*.org") t))

  ) ;; org-mode ends here