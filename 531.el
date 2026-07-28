;;; 531.el --- Generate 5/3/1 training plan and store training data.  -*- lexical-binding: t; -*-

(defvar 5/3/1-db-file "531.db"
  "Database file name.")

(defvar 5/3/1--main-lifts '("SQUAT" "BENCH PRESS" "DEADLIFT" "PRESS")
  "Main lifts in 5/3/1.")

(defvar 5/3/1--null-lift "NONE"
  "Value for main lift if there is not main lift.")

(defvar 5/3/1-weekday-to-lift
  (list
   5/3/1--null-lift
   (nth 0 5/3/1--main-lifts)
   (nth 1 5/3/1--main-lifts)
   5/3/1--null-lift
   (nth 2 5/3/1--main-lifts)
   (nth 3 5/3/1--main-lifts)
   5/3/1--null-lift
   5/3/1--null-lift)
  "Map from weekday to lift.")

(defvar original-5/3/1-template
  '(((0.65 . 5) (0.75 . 5) (0.85 . 5))
    ((0.70 . 3) (0.80 . 3) (0.90 . 3))
    ((0.75 . 5) (0.85 . 3) (0.95 . 1)))
  "The original 5/3/1 cycle template.")

(defvar 5/3/1-working-template original-5/3/1-template
  "The current working cycle template.")

;; Helper functions;
(defun 5/3/1--compute-training-max (weight rep &optional percent)
  "Compute your training max using WEIGHT and REP."
  (* (or percent 0.85) (+ weight (* weight rep 0.0333))))

(defun 5/3/1--get-lift-by-date ()
  "Return the main lift to do today."
  (let* ((weekday (string-to-number (format-time-string "%w")))
         (lift (nth weekday 5/3/1-weekday-to-lift)))
    (if (or (null lift)
            (equal lift 5/3/1--null-lift))
        (user-error "No main lift today!")
      lift)))

;; DB operators
(defvar 5/3/1--db nil
  "Connecting DB object.")

(defvar 5/3/1--training-max-table-name "TRAINING MAX"
  "Training max table name.")

(defvar 5/3/1--training-max-table-key-column "Lift"
  "Key column of the training max table.")

(defvar 5/3/1--training-max-table-value-column "Weight"
  "Value column of the training max table.")

(defun 5/3/1--init-db ()
  "Create DB file and tables if absent."
  (let ((path (expand-file-name 5/3/1-db-file user-emacs-directory)))
    (setq 5/3/1--db (sqlite-open path))
    (dolist (table 5/3/1--main-lifts)
      (sqlite-execute 5/3/1--db
                      (5/3/1--create-lift-table-command table)))
    (sqlite-execute 5/3/1--db
                    (5/3/1--create-training-max-table-command))))

(defun 5/3/1--get-db ()
  "Return the DB object."
  (unless 5/3/1--db
    (5/3/1--init-db))
  5/3/1--db)

(defun 5/3/1-get-training-max (lift)
  "Return the training max for LIFT."
  (let* ((db (5/3/1--get-db))
         (result (sqlite-select
                  db
                  (format "SELECT %s FROM \"%s\" WHERE %s = \"%s\""
                          5/3/1--training-max-table-value-column
                          5/3/1--training-max-table-name
                          5/3/1--training-max-table-key-column
                          lift))))
    (if result
        (caar result)
      (user-error "No training max record for %s!" lift))))

(defun 5/3/1-get-weeks-for-lift (lift)
  "Return the number of exercised weeks for LIFT."
  (let* ((db (5/3/1--get-db))
         (result (sqlite-select
                  db
                  (format "SELECT COUNT(rowid) FROM \"%s\"" lift))))
    (if result
        (caar result)
      (user-error "Database table %s corrupted!" lift))))

(defun 5/3/1--create-lift-table-command (lift)
  "Return the SQL command to create a table for LIFT."
  (format
   "CREATE TABLE IF NOT EXISTS \"%s\" (
Date DATE PRIMARY KEY,
Weight1 INT,
Reps1 INT,
Weight2 INT,
Reps2 INT,
Weight3 INT,
Reps3 INT)"
   lift))

(defun 5/3/1--create-training-max-table-command ()
  "Return the SQL command to create the training max table."
  (format
   "CREATE TABLE IF NOT EXISTS \"%s\" (
%s VARCHAR(64),
%s INT)"
   5/3/1--training-max-table-name
   5/3/1--training-max-table-key-column
   5/3/1--training-max-table-value-column))

;; User entry points

;;;###autoload
(defun 5/3/1-store-training-max (lift weight rep percent)
  "Store training max to DB for LIFT using WEIGHT, REP, and PERCENT."
  (interactive
   (list (completing-read "Lift: " 5/3/1--main-lifts)
         (read-number "Weight: ")
         (read-number "Reps: ")
         (read-number "Percent: ")))
  (let ((training-max (5/3/1--compute-training-max weight rep percent))
        (db (5/3/1--get-db)))
    (sqlite-execute db
                    (format "INSERT OR REPLACE INTO \"%s\" VALUES (?, ?)"
                            5/3/1--training-max-table-name)
                    (list lift (floor training-max)))))

;;;###autoload
(defun 5/3/1-generate-workout-plan ()
  "Generate the workout plan for today."
  (interactive)
  (let* ((lift (5/3/1--get-lift-by-date))
         (training-max (5/3/1-get-training-max lift))
         (week (5/3/1-get-weeks-for-lift lift))
         (template (nth (mod week (length 5/3/1-working-template))
                        5/3/1-working-template)))
    (insert (format "%s\n" lift))
    (dolist (set template)
      (insert (format "Weight: %d Reps: %d\n"
                      (* (car set) (floor training-max))
                      (cdr set))))))

;;;###autoload
(defun 5/3/1-store-workout-record (begin end)
  "Store the workout record in region."
  (interactive "r")
  (let* ((record (buffer-substring begin end))
         (lines (split-string record "\n" t "\n"))
         (lift (car lines))
         (records (cdr lines)))
    (if (member lift 5/3/1--main-lifts)
        (progn
          (let ((data '()))
            (dolist (record records)
              (if (string-match (rx "Weight: "
                                    (group (1+ digit))
                                    (1+ blank)
                                    "Reps: "
                                    (group (1+ digit)))
                                record)
                  (let ((weight (match-string 1 record))
                        (reps (match-string 2 record)))
                    (push weight data)
                    (push reps data))
                (user-error "Malformed record: %s!" record)))
            (setq data (nreverse data))
            (let ((db (5/3/1--get-db)))
              (sqlite-execute
               db
               (format
                "INSERT INTO \"%s\" (Date, Weight1, Reps1, Weight2, Reps2, Weight3, Reps3) VALUES (?, ?, ?, ?, ?, ?, ?)"
                lift)
               (cons (format-time-string "%Y-%m-%d")
                     data)))))
      (user-error "Invalid lift: %s!" lift))))

;;;###autoload
(defun 5/3/1-list-workout-history (lift &optional max)
  "List at most MAX workout history for LIFT."
  (interactive
   (list (completing-read "Lift: " 5/3/1--main-lifts)
         (read-number "Max: ")))
  (let* ((db (5/3/1--get-db))
         (result (sqlite-select db (format "SELECT * FROM \"%s\" LIMIT %d" lift max))))
    (dolist (row result)
      (insert (format "%s\n" row)))))

(provide '5/3/1)
;;; 531.el ends here
