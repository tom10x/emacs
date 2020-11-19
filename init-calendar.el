;; ===== Calendar & Date & Time (Swedish) =====

;; English weekdays in org mode dates and other places
(setq system-time-locale "C")

;; Pretty much copied from https://bigwalter.net/daniel/elisp/sv-kalender.el
(require 'calendar)
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
  (- (calendar-absolute-from-gregorian date)
     (sv-easter (calendar-extract-year date))))

;; Weeks start on Monday in Sweden
(setq calendar-week-start-day 1)

;; Dates in diary and params to diary- functions: year/month/day
(setq calendar-date-style 'iso)

;; Date format
(setq calendar-date-display-form
      '((if dayname
            (concat dayname ", "))
        day " " monthname " " year))

;; 24 hour clock without timezone
(setq calendar-time-display-form '(24-hours ":" minutes))

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
(setq calendar-holidays holiday-general-holidays)

;; distinguish calendar-holidays in Calendar
(setq calendar-mark-holidays-flag t)
