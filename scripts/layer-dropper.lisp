;;;; tested with sbcl version 1.0.55.0.debian; requires the clsql package
;;;;
;;;; this code will generate a common lisp file that will replay the
;;;; onion adjustments that have occurred to ``*database*''

(proclaim '(optimize (debug 3)))

(defparameter *database* nil)

(defun verify-fields (fields)
  (and (string= (nth 0 fields) "_database")
       (string= (nth 1 fields) "_table")
       (string= (nth 2 fields) "_field")
       (string= (nth 3 fields) "_onion")
       (string= (nth 4 fields) "_level")
       (string= (nth 5 fields) "id")))

(defun generate-dropper (row c dbname)
  (assert (= 6 (length row)))
  `(clsql:query (apply #'format nil "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                                ,dbname ',(butlast (cdr row)))
                :database ,c))

(defmacro db-op (c db &rest body)
  `(let* ((,(car db) ,(cdr db))
          (,c (clsql:connect
                (list "127.0.0.1" ,(car db) "root" "letmein" 3307)
                :database-type :mysql
                :if-exists :new
                :make-default nil)))
     (unwind-protect
       (progn
         ,@body)
       (clsql:disconnect :database ,c))))

(defun pretty-now ()
  (multiple-value-bind (seconds minutes hours days months years)
        (decode-universal-time (get-universal-time))
    (format nil "~A/~A/~A ~A:~A:~A" months days years
                                    hours minutes seconds)))

(defun do-stuff (database)
  "build a lisp form for adjusting ``database''"
  (db-op c (db . database)
    (multiple-value-bind (onions fields)
        (clsql:query "SET @cryptdb='show'" :database c)
      (assert (or (and (null onions) (null fields))
                  (verify-fields fields)))
      ;;; remove RND and HOM onions
      (setf onions
            (remove-if #'(lambda (row)
                           (member (nth 4 row)
                                   '("RND" "HOM")
                                   :test #'string=))
                       onions))
      ;;; only use results pertaining to 'database'
      (setf onions
            (remove-if-not #'(lambda (row)
                               (string= (nth 0 row) database))
                           onions))
      ;;; build the output lisp forms
      `(defun adjust-main ()
         ,(format nil "onion adjuster for database: ~A, created: ~A"
                      database (pretty-now))
         ,(macroexpand
            `(db-op conn (db . ,database)
               ,@(mapcar #'(lambda (o) (generate-dropper o 'conn 'db))
                         onions)))
         nil))))

(defun main ()
  (with-open-file (stream "onions.lisp" :direction :output
                                        :if-exists :supersede)
    (pprint (do-stuff *database*) stream)))
