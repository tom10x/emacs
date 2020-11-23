(defvar
  my-system-time-locale
  "C"
  "Controls e.g. org-mode weekday names. `C' means: use English weekdays.")

(setq system-time-locale my-system-time-locale)

(use-package calendar
  :ensure nil
  :init
  (setq
   calendar-mark-holidays-flag t
   calendar-week-start-day 1     ; 1=Monday
   calendar-date-style 'iso)
  (setq calendar-time-display-form '(24-hours ":" minutes))
  :config
  (defun sv-easter (year)
    "Calculate the date for Easter in YEAR."
    (let* ((century (1+ (/ year 100)))
           (shifted-epact (% (+ 14 (* 11 (% year 19))
                                (- (/ (* 3 century) 4))
                                (/ (+ 5 (* 8 century)) 25)
                                (* 30 century))
                             30))
           (adjusted-epact (if (or (= shifted-epact 0)
                                   (and (= shifted-epact 1)
                                        (< 10 (% year 19))))
                               (1+ shifted-epact)
                             shifted-epact))
           (paschal-moon (- (calendar-absolute-from-gregorian
                             (list 4 19 year))
                            adjusted-epact)))
      (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))
  (defun sv-days-from-easter ()
    "When used in a diary sexp, this function will calculate how many days
are between the current date (DATE) and Easter Sunday."
    (progn (eval-when-compile (defvar date))
           (- (calendar-absolute-from-gregorian date)
              (sv-easter (calendar-extract-year date)))))

  ;; Swedish holidays (no work, yay)
  (setq holiday-general-holidays
        '((holiday-fixed 1 1 "Nyårsdagen")
          (holiday-fixed 1 6 "Trettondedag jul")
          (holiday-filter-visible-calendar
           (mapcar
            (lambda (dag)
              (list (calendar-gregorian-from-absolute
                     (+ (sv-easter displayed-year) (car dag)))
                    (cadr dag)))
            '((  -2 "Långfredagen")
              (  -1 "Påskafton")
              (   0 "Påskdagen")
              (  +1 "Annandag påsk")
              ( +39 "Kristi himmelfärdsdag"))))
          (holiday-fixed 5 1 "Första maj")
          (let ((midsommar-d (calendar-dayname-on-or-before
                              6 (calendar-absolute-from-gregorian (list 6 26 displayed-year)))))
            (holiday-filter-visible-calendar
             (list
              (list
               (calendar-gregorian-from-absolute (1- midsommar-d))
               "Midsommarafton")
              (list
               (calendar-gregorian-from-absolute midsommar-d)
               "Midsommardagen"))))
          (holiday-fixed 6 6 "Sveriges nationaldag")
          (holiday-filter-visible-calendar
           (list
            (list
             (calendar-gregorian-from-absolute
              (calendar-dayname-on-or-before
               6 (calendar-absolute-from-gregorian
                  (list 11 6 displayed-year))))
             "Alla helgons dag")))
          (holiday-fixed 12 24 "Julafton")
          (holiday-fixed 12 25 "Juldagen")
          (holiday-fixed 12 26 "Annandag jul")
          (holiday-fixed 12 31 "Nyårsafton")))
  ;; no other holiday-*-holidays thanks
  (setq calendar-holidays holiday-general-holidays))

;; Using local vars to turn off this warning in sv-days-from-easter:
;;   global/dynamic var ‘date’ lacks a prefix
;; It uses `date' by design.

;; Local Variables:
;; byte-compile-warnings: (not lexical)
;; End: