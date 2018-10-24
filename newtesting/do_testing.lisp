;; NOTES
;; > the testing interface does not support mysql identifiers
;;   (tables, fields, etc) with leading or trailing spaces
;;   + in queries like, ``SELECT a + b FROM t'' mysql returns the field as
;;     'a + b ' (note the trailing space); this behavior is not replicated by
;;     cryptdb hence the tests use a generic string trim operation on
;;     all fields
;; TODO
;; > 'USE' the default database for each test group

(defpackage :cryptdb-testing
  (:use :cl :clsql)
  (:export :main
           :*filter-tests*
           :test-all-units
           :compare-results
           :db-connect
           :issue-query
           :execute-test-query
           :make-connection-state
           :connection-state-cryptdb
           :connection-state-plain
           :destroy-connections
           :read-file))
(in-package :cryptdb-testing)

(proclaim '(optimize (debug 3)))

(defparameter +default-tests-path+       (concatenate 'string
                                           (sb-unix::posix-getenv "EDBDIR")
                                           "/newtesting/tests.sexpr"))
(defparameter +default-kz-tests-path+    (concatenate 'string
                                           (sb-unix::posix-getenv "EDBDIR")
                                           "/newtesting/kz_tests.sexpr"))
(defparameter +default-ip+               "127.0.0.1")
(defparameter +default-username+         "root")
(defparameter +default-password+         "letmein")
(defparameter +default-database+         "cryptdbtest")
(defparameter +default-control-database+ "cryptdbtest_control")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;     database helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct connection-state
  cryptdb
  plain)

(defmethod destroy-connections ((connections connection-state))
  (clsql:disconnect :database (connection-state-cryptdb connections))
  (setf (connection-state-cryptdb connections) nil)
  (clsql:disconnect :database (connection-state-plain connections))
  (setf (connection-state-plain connections) nil))

(defun init-use (connections)
  (must-succeed-query (format nil "USE ~A" +default-database+)
                      (connection-state-cryptdb connections))
  (must-succeed-query (format nil "USE ~A" +default-control-database+)
                      (connection-state-plain connections)))

(defmethod revive-connections ((connections connection-state))
  (setf (connection-state-cryptdb connections)
        (clsql:reconnect :database (connection-state-cryptdb connections)))
  (setf (connection-state-plain connections)
        (clsql:reconnect :database (connection-state-plain connections)))
  (init-use connections))

(defun db-connect (db port &optional (ip       +default-ip+)
                                     (username +default-username+)
                                     (password +default-password+))
  (clsql:connect
    `(,ip ,db ,username ,password ,port)
    :database-type :mysql
    :if-exists     :new
    :make-default  nil))

(defmethod connection-alive? (c)
  (query-result-status (issue-query "show databases" c)))

(defstruct query-result
  status
  fields
  rows)

(defun issue-query (query database)
  (handler-case
        (multiple-value-bind (rows fields) (clsql:query query :database database)
          (make-query-result
            :status t
            :fields (mapcar #'(lambda (f) (string-trim '(#\Space) f)) fields)
            :rows   rows))
    (clsql:sql-database-error (e)
      (declare (ignore e))
      (make-query-result
        :status nil
        :fields nil
        :rows   nil))))

(defun must-succeed-query (query database)
  (assert (query-result-status (issue-query query database))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        misc helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; by _death from #lisp
(defun traverse (function structure path)
  (if (atom structure)
      (funcall function (reverse (cons structure path)))
      (dolist (x (cdr structure))
        (traverse function x (cons (car structure) path)))))

;; by _death from #lisp
(defmacro do-structure (((&rest vars) structure-form) &body forms)
  (let ((list (gensym)))
    `(traverse (lambda (,list)
                 (destructuring-bind ,vars ,list
                   (declare (ignorable ,@vars))
                   ,@forms))
               ,structure-form
               '())))

(defun read-file (path)
  (with-open-file (stream path :direction :input)
    (let ((seq (make-string (file-length stream))))
      (read-sequence seq stream)
      seq)))

(defun str-assoc (key alist)
  (assoc key alist :test #'string-equal))

;;; returns (did-the-lookup-complete? completed-portion-of-lookup unmatched-keys)
(defun many-str-assoc (keys nested-alists)
  (let ((alist nested-alists)
        (keys-len (length keys)))
    (do ((i 0 (1+ i)))
        ((= keys-len i) (values t alist '()))
      (let* ((k (elt keys i))
             (found-alist (str-assoc k (if (zerop i) alist (cdr alist)))))
        (when (null found-alist)
          (return (values nil alist (subseq keys i))))
        (setf alist found-alist)))))

;;; apply fn to each node
(defun map-tree (fn tree)
  (cond ((null tree) '())
        ((atom (car tree))
         (cons (funcall fn (car tree)) (map-tree fn (cdr tree))))
        (t (cons (map-tree fn (car tree)) (map-tree fn (cdr tree))))))

(defun list-depth (list)
  (assert (listp list))
  (cond ((null list) 1)
        (t (apply
             #'max
             (mapcar
               #'(lambda (e)
                   (cond ((atom e) 1)
                         (t (1+ (list-depth e)))))
               list)))))

(defmethod all-true ((pair list))
  (every #'identity pair))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   handle test file input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-tests (path)
  (read-from-string (read-file path)))

;; FIXME: implement
(defun proper-onion-update? (onion-check)
  (assert (listp onion-check))
  t)

(defun build-update-1 (tag database table field onion seclevel)
  `(,tag (,database (,table (,field (,onion ,seclevel))))))

(defun assert-all-lists (list)
  (dolist (e list) (assert (listp e))))

(defun fix-onion-check-form (dirty-onion-check)
  (cond ((null dirty-onion-check) nil)
        ((atom dirty-onion-check) (list (list dirty-onion-check)))
        ((atom (car dirty-onion-check))
         (list dirty-onion-check))
        (t (mapcar #'(lambda (e)
                       (if (listp e) e (list e)))
                   dirty-onion-check))))

(defun fix-onion-check-semantics (dirty-onion-check db)
  (mapcar
    #'(lambda (check)
        (let ((tag (car check)))
          (ecase (car check)
            (:check (assert (= 1 (length check)))
                    check)
            ((:set :update)
             (cond ((every #'atom check)
                    (ecase (length check)
                      ;; (:update <table> <field> <onion> <seclevel>)
                      (5 (apply #'build-update-1 tag db (cdr check)))
                      ;; (:update <database> <table> <field> <onion> <seclevel>)
                      (6 (apply #'build-update-1 tag (cdr check)))))
                   ((proper-onion-update? check)
                    (ecase (list-depth (cdr check))
                      ;; (:update (<table> (<field> ...)) (<table> ...) ...)
                      (4 `(,tag (,db ,@(cdr check))))
                      ;; (:update (<database> (<table> ...)) (<database> ...) ...)
                      (5 check)))))
            (:all-max
              (when (every #'atom check)
                (ecase (length check)
                  ;; (:all-max <table>)
                  (2 `(:all-max ,db ,(cadr check)))
                  ;; (:all-max <database> <table>)
                  (3 check))))
            ((:exists :does-not-exist)
              (when (every #'atom check)
                (ecase (length check)
                  ;; (:exists <table>)
                  (2 `(,tag ,db ,(cadr check)))
                  ;; (:exists <table> <field>)
                  (3 `(,tag ,db ,@(cdr check)))
                  ;; (:exists <database> <table> <field>)
                  (4 check)))))))
      dirty-onion-check))

;;; take possibly shorthand onion-checks and produce full length checks
;;; > return nil if `dirty-onion-checks` are invalid
(defun fixup-onion-checks (dirty-onion-checks default-database)
  (let* ((db (if (eq t default-database) +default-database+ default-database))
         (fixed (fix-onion-check-semantics
                  (fix-onion-check-form dirty-onion-checks)
                  db)))
    ;; convert symbols to strings except for the initial directive
    (mapcar #'(lambda (f)
                (cons (car f) (map-tree #'string (cdr f))))
            fixed)))

(defun fixup-execution-target (dirty-execution-target)
  (case dirty-execution-target
    ((nil) :both)
    ((:cryptdb :control :both) dirty-execution-target)))

(defun fixup-testing-strategy (dirty-testing-strategy)
  (when (null dirty-testing-strategy)
    (return-from fixup-testing-strategy #'ts-compare))
  (let ((ts-form
          (intern
            (concatenate 'string "TS-"
                                 (string-upcase dirty-testing-strategy)))))
    (cond ((fboundp ts-form) (symbol-function ts-form))
          (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  manage local onion state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ((database-name
;;;    (table-name (field-name (onion-name security-level) (onion-name security-level))
;;;                (field-name (...))))
;;;  (database-name
;;;    (table-name (...))
;;;  ...))
(defstruct onion-state
  databases)

(defmethod deep-copy-onion-state ((onions onion-state))
  (make-onion-state :databases (copy-list (onion-state-databases onions))))

(defmethod low-level-lookup-seclevel ((onions onion-state)
                                      database table field onion)
  (multiple-value-bind (success lookup unmatched-keys)
        (many-str-assoc `(,database ,table ,field ,onion)
                        (onion-state-databases onions))
    (assert (and success (null unmatched-keys)))
    (cdr lookup)))

(defmethod lookup-seclevel ((onions onion-state) database table field onion)
  (car (low-level-lookup-seclevel onions database table field onion)))

(defun (setf lookup-seclevel) (seclevel onions database table field onion)
  (setf (car (low-level-lookup-seclevel onions database table field onion))
        seclevel))

(defun nest-unmatched-keys (unmatched-keys seclevel)
  (assert (not (null unmatched-keys)))
  (cond ((null (cdr unmatched-keys)) `(,(car unmatched-keys) ,seclevel))
        (t `(,(car unmatched-keys) ,(nest-unmatched-keys
             (cdr unmatched-keys) seclevel)))))

(defmethod add-onion! ((onions onion-state) database table field onion seclevel)
  ;; don't try to look anything up if we don't have any onion data
  (when (null (onion-state-databases onions))
    (setf (onion-state-databases onions)
          `((,database (,table (,field (,onion ,seclevel))))))
    (return-from add-onion! t))
  (multiple-value-bind (success lookup unmatched-keys)
        (many-str-assoc `(,database ,table ,field ,onion)
                        (onion-state-databases onions))
    ;; a total match should only occur when we max back to back tables
    ;; > ie, consecutive CREATE TABLE queries
    (when success
      (assert (null unmatched-keys))
      (assert (string-equal seclevel (cadr lookup)))
      (return-from add-onion! t))
    (assert (not (null lookup)))
    (setf (cdr lookup)
          (append (cdr lookup)
                  `(,(nest-unmatched-keys unmatched-keys seclevel))))))

(defun max-level? (onion seclevel)
  (cond ((member onion '("oEq" "oOrder" "oPLAIN") :test #'string-equal)
         (string-equal "RND" seclevel))
        ((string-equal "oADD" onion) (string-equal "HOM" seclevel))))

;;; change our local copy of the onion state
(defmethod update-onion-state! ((onions onion-state) onion-check)
  (assert (eq :update (car onion-check)))
  (dolist (checks (cdr onion-check))
    (do-structure ((database table field onion seclevel) checks)
      (setf (lookup-seclevel onions database table field onion) seclevel))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    onion check handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-cryptdb-show (connections)
  (let ((results (issue-query "SET @cryptdb='show'"
                              (connection-state-cryptdb connections))))
    (assert (query-result-status results))
    (assert (equal '("_database" "_table" "_field" "_onion" "_level" "id")
                   (query-result-fields results)))
    (query-result-rows results)))

;; should we fail when presented with onion data for a database not in our
;; local cache?
(defparameter *break-errant-database* nil)

(defgeneric handle-check (connections onions type onion-check)
  (:method (connections (onions onion-state) (type (eql :all-max)) onion-check)
    (let ((max-database (cadr onion-check))
          (max-table    (caddr onion-check))
          (results      (get-cryptdb-show connections))
          (output       t))
      (dolist (row results output)
        (destructuring-bind (database table field onion seclevel id) row
          (declare (ignore id))
          (when (and (string-equal max-database database)
                     (string-equal max-table table))
            (when (not (max-level? onion seclevel))
              (setf output nil))
            ;; update our local copy of onion state
            ;; > we cannot use (setf lookup-seclevel) because the
            ;;   local onion state may be nil
            (add-onion! onions database table field onion seclevel))))))
  (:method (connections (onions onion-state) (type (eql :set)) onion-check)
    (dolist (checks (cdr onion-check) t)
      (do-structure ((database table field onion seclevel) checks)
        (unless (add-onion! onions database table field onion seclevel)
          (break)
          (return nil)))))
  (:method (connections (onions onion-state) (type (eql :update)) onion-check)
    (update-onion-state! onions onion-check)
    (handle-check connections onions :check onion-check))
  (:method (connections (onions onion-state) (type (eql :check)) onion-check)
    (let ((results (get-cryptdb-show connections))
          (output  t))
      (dolist (row results output)
        (destructuring-bind (database table field onion seclevel id) row
          (declare (ignore id))
          (cond ((not (str-assoc database (onion-state-databases onions)))
                 (assert (not *break-errant-database*)))
                (t
                  (unless (string-equal seclevel
                             (lookup-seclevel
                               onions database table field onion))
                    ; (break)
                    ;; setting a flag instead of shortcircuiting causes us to
                    ;; continue updating seclevel's after failure
                    (setf output nil))
                  (setf (lookup-seclevel onions database table field onion)
                        seclevel)))))))
  (:method (connections (onions onion-state) (type (eql :exists)) onion-check)
    (let ((cryptdb (connection-state-cryptdb connections)))
      (ecase (length onion-check)
        ;; table
        (3 (destructuring-bind (tag db table) onion-check
             (declare (ignore tag))
             (query-result-status
               (issue-query
                 (string-downcase (format nil "SELECT * FROM `~A`.`~A`" db table))
                 cryptdb))))
        ;; field
        (4 (destructuring-bind (tag db table field) onion-check
             (declare (ignore tag))
             (query-result-status
               (issue-query
                 (string-downcase
                   (format nil "SELECT `~A` FROM `~A`.`~A`" field db table))
                 cryptdb)))))))
  (:method (connections
            (onions onion-state)
            (type (eql :does-not-exist))
            onion-check)
    (not (handle-check connections onions :exists onion-check))))

;;; take the per query onion checks and use them to update our onion state;
;;; then compare this new onion state to the state reported by cryptdb for
;;; each testing onion
(defmethod handle-onion-checks ((connections connection-state)
                                (onions onion-state)
                                (onion-checks list))
  (all-true
    (mapcar #'(lambda (c) (handle-check connections onions (car c) c))
            onion-checks)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      testing strategies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric ts-must-succeed (cryptdb-results plain-results)
  (:method ((cryptdb-results query-result) plain-results)
    (query-result-status cryptdb-results)))

(defgeneric ts-must-fail (cryptdb-results plain-results)
  (:method ((cryptdb-results query-result) plain-results)
    (not (query-result-status cryptdb-results))))

(defgeneric ts-ignore (cryptdb-results plain-results)
  (:method (cryptdb-results plain-results)
    t))

(defgeneric ts-compare (cryptdb-results plain-results)
  (:method ((cryptdb-results query-result) (plain-results query-result))
    (compare-results cryptdb-results plain-results)))

(defgeneric ts-ignore-fields (cryptdb-results plain-results)
  (:method ((cryptdb-results query-result) (plain-results query-result))
    (setf (query-result-fields cryptdb-results) nil
          (query-result-fields plain-results)   nil)
    (compare-results cryptdb-results plain-results)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;         run tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct group-score
  (wins 0)
  (fails 0))

(defgeneric update-score (value)
  (:method ((value (eql t)))
    (declare (special *score*))
    (incf (group-score-wins *score*)))
  (:method ((value (eql nil)))
    (declare (special *score*))
    (break)
    (incf (group-score-fails *score*))))

(defun fast-compare (results-a results-b)
  ; (break)
  (equal results-a results-b))

(defun slow-compare (results-aye results-bee)
  (when (not (= (length results-aye) (length results-bee)))
    (return-from slow-compare nil))
  ; (break)
  (every #'(lambda (a)
             (= (count a results-aye :test #'equal)
                (count a results-bee :test #'equal)))
         results-aye))

(defmethod compare-results ((results-a query-result) (results-b query-result))
  (cond ((and (not (query-result-status results-a))
              (not (query-result-status results-b)))
         t)
        ((or (not (query-result-status results-a))
             (not (query-result-status results-b)))
         ; (break)
         nil)
        ((not (equal (query-result-fields results-a)
                     (query-result-fields results-b)))
         ; (break)
         nil)
        (t ;; cryptdb returns all results as strings while the normal
           ;; database uses numbers and such
           (let ((rows-a (all-strings (query-result-rows results-a)))
                 (rows-b (all-strings (query-result-rows results-b))))
             (or (fast-compare rows-a rows-b)
                 (slow-compare rows-a rows-b))))))

;; HACK: cryptdb returns integers for SUM(...) of integers while vanilla
;;       mysql returns floating points with no decimals
(defun float-to-string (float)
  (assert (typep float 'float))
  (let ((rational (rationalize float)))
    (if (= 1 (denominator rational))
        (write-to-string (numerator rational))
        (format t "~A" float))))

(defun all-strings (results)
  (mapcar #'(lambda (row)
              (mapcar #'(lambda (e)
                          (etypecase e
                            (string  e)
                            (integer (write-to-string e))
                            (null    "NULL")
                            (float   (float-to-string e))))
                      row))
          results))

(defun valid-group-default? (group-default)
  (or (equal t group-default) (equal nil group-default) (stringp group-default)))

(defmethod execute-test-query ((connections connection-state)
                               query
                               execution-target)
  (let ((cryptdb (connection-state-cryptdb connections))
        (control (connection-state-plain connections)))
    (ecase execution-target
      (:cryptdb (issue-query query cryptdb))
      (:control (values nil (issue-query query control)))
      (:both    (values (issue-query query cryptdb)
                        (issue-query query control))))))

(defparameter *killed*    nil)
(defparameter *reconnect* nil)
(defparameter *reconnect-wait-time* 1)

(defun handle-query (query-encoding onions connections default)
  (let* ((query (car query-encoding))
         (onion-checks (fixup-onion-checks (cadr query-encoding) default))
         (execution-target (fixup-execution-target (caddr query-encoding)))
         (testing-strategy (fixup-testing-strategy (cadddr query-encoding))))
    (assert (eq (null (cadr query-encoding)) (null onion-checks)))
    (multiple-value-bind (cryptdb-results plain-results)
        (execute-test-query connections query execution-target)
      (setf *killed*
            (not (connection-alive? (connection-state-cryptdb connections))))
      (when (and *killed* *reconnect*)
        (sleep *reconnect-wait-time*)
        (revive-connections connections))
      (let ((onion-check-result
             (handle-onion-checks connections onions onion-checks)))
        (and onion-check-result
             (funcall testing-strategy cryptdb-results plain-results))))))

(defun run-test-group (connections test-group)
  (let* ((*score* (make-group-score))
         (onions (make-onion-state)))
    (declare (special *score*))
    (destructuring-bind (group-name group-default &rest test-list) test-group
      (declare (ignorable group-name))
      (assert (valid-group-default? group-default))
      (dolist (test-case test-list *score*)
        (update-score
          (handle-query test-case onions connections group-default))))))

;; for each assertion we must;
;; 0> do setup
;; 1> set the kill count
;; 2> issue the test query
;; 3> check assertions
;; 4> check comparison queries
;; 5> do teardown
;;
;; in order to accomplish this we have two bindings for tracking onion state
;; ! 'incremental-onions' tracks the state of our onions from one assertion
;;   to the next; only the _first_ setup and the _first_ test query affect
;;   this onion state, after that we incrememntally change it with assertions
;; ! 'onions' tracks the state of our onions during a given test run; it is
;;   reset after each teardown
(defun kz-run-test-group (connections test-group)
  (let* ((*break-errant-database* nil)
         (*score* (make-group-score))
         (incremental-onions nil))
    (declare (special *score*))
    (destructuring-bind
        (group-name group-default setup test-query compares asserts teardown)
        test-group
      (declare (ignorable group-name))
      (assert (valid-group-default? group-default))
      (dolist (assertion asserts *score*)
        (let ((onions (make-onion-state)))
          ;; do setup
          (dolist (q setup)
            (handle-query q onions connections group-default))
          (destructuring-bind (where count stones) assertion
            ;; set kill count
            (must-succeed-query
              (format nil "SET @cryptdb='killzone', @where='~A', @count='~A'"
                          where count)
              (connection-state-cryptdb connections))
            ;; issue test query
            (let ((*killed*    nil)
                  (*reconnect* t))
              (handle-query test-query onions connections group-default)
              (assert *killed*)
              ;; now throw stones (onion checks)
              (let ((stones (fixup-onion-checks stones group-default)))
                ;; should only happen the first time (or 2..?)
                (when (or (null incremental-onions)
                          (null (onion-state-databases incremental-onions)))
                  (setf incremental-onions (deep-copy-onion-state onions)))
                (update-score
                  (and (handle-onion-checks connections incremental-onions stones)
                       (all-true
                          (mapcar
                            #'(lambda (c)
                                (handle-query c onions connections group-default))
                            compares)))))))
          ;; do tear down
          (dolist (q teardown)
            (handle-query q onions connections group-default)))))))

(defun run-tests (all-test-groups group-tester)
  (let* ((connections
           (make-connection-state :cryptdb (db-connect nil 3307)
                                  :plain   (db-connect nil 3306)))
         (cryptdb (connection-state-cryptdb connections))
         (plain (connection-state-plain connections))
         (results '()))
    ;; remove artefacts
    (issue-query
      (format nil "DROP DATABASE ~A" +default-database+) cryptdb)
    (issue-query
      (format nil "DROP DATABASE ~A" +default-control-database+) plain)
    ;; create default database
    (must-succeed-query
      (format nil "CREATE DATABASE ~A" +default-database+) cryptdb)
    (must-succeed-query
      (format nil "CREATE DATABASE ~A" +default-control-database+) plain)
    ;; set proper default database
    (init-use connections)
    ;; begin testing
    (dolist (test-group all-test-groups results)
      (let ((score (funcall group-tester connections test-group)))
        (format t "~A:~25T~A failures~%"
                  (car test-group)
                  (group-score-fails score))
        (push score results)))
    (destroy-connections connections)
    (reverse results)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   commence cryptdb testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; give *filter-tests* the names of test groups to be run
;; > no names indicates that all tests should be run
(defparameter *filter-tests* '())

(defun main (&optional (test-type 'functional))
  (let* ((tpair
           (ecase test-type
             (functional (cons #'run-test-group +default-tests-path+))
             (killzone   (cons #'kz-run-test-group +default-kz-tests-path+))))
         (tests (load-tests (cdr tpair))))
    (run-tests (remove-if-not #'(lambda (name)
                                  (or (null *filter-tests*)
                                      (member name *filter-tests*
                                              :test #'string-equal)))
                              tests
                              :key #'car)
               (car tpair))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        unit tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro must-signal (&rest body)
  `(handler-case
     (progn
       ,@body
       nil)
     (error ()
        (progn
          t))))

(defun test-fixup-onion-checks ()
  ;; a fully specifed check should be returned as is
  (and (let ((line
               '((:update
                   ("database0"
                    ("table0" ("field0" ("onion0" "seclevel0")
                                        ("onion1" "seclevel1"))
                              ("field1" ("onion2" "seclevel2"))))))))
         (equal line (fixup-onion-checks line nil)))
       ;; :check doesn't take parameters
       (must-signal (fixup-onion-checks '(:check 1 2 3) nil))
       ;; a single check does not require nesting
       (equal '((:update ("database" ("table" ("field" ("onion" "seclevel"))))))
              (fixup-onion-checks
                '(:update "database" "table" "field" "onion" "seclevel") nil))
       (must-signal (fixup-onion-checks '(:update ()) nil))
       (must-signal (fixup-onion-checks :update nil))
       (equal '((:check))
              (fixup-onion-checks '(:check) nil))
       (equal '((:check))
              (fixup-onion-checks :check nil))
       (must-signal (fixup-onion-checks :all-max nil))
       (equal '((:all-max "db" "table"))
              (fixup-onion-checks '(:all-max "db" "table") nil))
       ;; use hardcoded default database
       (equal `((:all-max ,+default-database+ "table"))
              (fixup-onion-checks '(:all-max "table") t))
       (equal `((:update (,+default-database+
                           ("table" ("field" ("onion" "seclevel"))))))
              (fixup-onion-checks
                '(:update "table" "field" "onion" "seclevel") t))
       ;; use default database from test group
       (equal '((:all-max "some-default" "table"))
              (fixup-onion-checks '(:all-max "table") "some-default"))
       (equal '((:update ("a-default" ("table" ("field" ("onion" "seclevel"))))))
              (fixup-onion-checks
                '(:update "table" "field" "onion" "seclevel") "a-default"))))

(defun test-fixup-onion-checks-multiple-update-default-bug ()
  (let ((line '(:update ("t" ("f"  ("o"  "l"))
                             ("f2" ("o2" "l2"))))))
    (equal `((:update (,+default-database+ ("t" ("f"  ("o"  "l"))
                                                ("f2" ("o2" "l2"))))))
           (fixup-onion-checks line t))))

(defun test-fixup-onion-checks-set ()
  (and (let ((line '((:set ("d" ("t" ("f" ("o" "l"))))))))
         (and (equal line (fixup-onion-checks line nil))
              (equal line
                     (fixup-onion-checks '(:set ("t" ("f" ("o" "l")))) "d"))))))

(defun test-fixup-onion-check-atoms-in-list ()
  (equal '((:check) (:check))
         (print (fixup-onion-checks '((:check) :check) nil))))

(defun test-fixup-execution-target ()
  (and (eq :both (fixup-execution-target :both))
       (eq :both (fixup-execution-target nil))
       (eq :control (fixup-execution-target :control))
       (eq :cryptdb (fixup-execution-target :cryptdb))
       (eq nil (fixup-execution-target 'whatever))))

(defun test-fixup-testing-strategy ()
  (and (eq #'ts-compare (fixup-testing-strategy :compare))
       (eq #'ts-must-succeed (fixup-testing-strategy :must-succeed))
       (eq #'ts-must-fail (fixup-testing-strategy :must-fail))
       (eq #'ts-ignore (fixup-testing-strategy :ignore))
       (eq #'ts-compare (fixup-testing-strategy nil))
       (null (fixup-testing-strategy 'whatever))))

(defun test-lookup-seclevel ()
  (let ((onions
          (make-onion-state
            :databases '(("database"
                            ("table0" ("field0" ("onion0" "level")))
                            ("table1" ("field1" ("onion1" "anotherlevel")
                                                ("onion2" "moremore"))
                                      ("field2" ("onion3" "again"))))))))
    (and (string= "level"
                  (lookup-seclevel onions "database" "table0" "field0" "onion0"))
         (string= "anotherlevel"
                  (lookup-seclevel onions "database" "table1" "field1" "onion1"))
         (string= "moremore"
                  (lookup-seclevel onions "database" "table1" "field1" "onion2"))
         (string=
           "again"
           (lookup-seclevel onions "database" "table1" "field2" "onion3")))))

(defun test-update-onion-state! ()
  (let ((onions
          (make-onion-state
            :databases '(("database"
                            ("table0" ("field0" ("onion0" "level")))
                            ("table1" ("field1" ("onion1" "anotherlevel")
                                                ("onion2" "moremore"))
                                      ("field2" ("onion3" "again"))))))))
    (update-onion-state!
      onions '(:update ("database" ("table1" ("field1" ("onion1" "newlevel"))))))
    (string= "newlevel"
             (lookup-seclevel onions "database" "table1" "field1" "onion1"))))


(defun test-many-str-assoc ()
  (and (multiple-value-bind (success lookup unmatched-keys)
            (many-str-assoc '(a b c) '((a (b (x y)
                                             (c d)))))
         (and success
              (equal '(c d) lookup)
              (null unmatched-keys)))
       (let ((test (copy-list
                     '((a (b (c d)
                             (e f))
                          (c (c d)))))))
         (multiple-value-bind (success lookup unmatched-keys)
            (many-str-assoc '(a c c) test)
           (and success
                (equal '(c d) lookup)
                (null unmatched-keys)
                (multiple-value-bind (success-2 lookup-2 unmatched-keys-2)
                    (many-str-assoc '(a c c) test)
                  (declare (ignore unmatched-keys-2))
                  (assert success-2)
                  (setf (cdr lookup-2) '(g))
                  (equal '((a (b (c d)
                                 (e f))
                              (c (c g))))
                         test)))))
       (multiple-value-bind (success lookup unmatched-keys)
            (many-str-assoc '(x y z) '((x (y)
                                          (z))))
         (and (not success)
              (equal '(y) lookup)
              (equal '(z) unmatched-keys)))
       (multiple-value-bind (success lookup unmatched-keys)
            (many-str-assoc '(x y z)  '((x (y (q)
                                              (p)
                                              (r)))))
         (and (not success)
              (equal '(y (q)
                         (p)
                         (r))
                     lookup)
              (equal '(z) unmatched-keys)))
       (multiple-value-bind (success lookup unmatched-keys)
            (many-str-assoc '(a b c) '((x (y))))
         (and (not success)
              (equal '((x (y))) lookup)
              (equal '(a b c) unmatched-keys)))))

(defun test-add-onion! ()
  (let ((onions (make-onion-state)))
    (add-onion! onions "db0" "t0" "f0" "o0" "l0")
    (and (equal '(("db0" ("t0" ("f0" ("o0" "l0")))))
                (onion-state-databases onions))
         (string= "l0" (lookup-seclevel onions "db0" "t0" "f0" "o0"))
         (progn
           (add-onion! onions "db0" "t0" "f1" "o1" "l1")
           (and (string= "l0" (lookup-seclevel onions "db0" "t0" "f0" "o0"))
                (string= "l1" (lookup-seclevel onions "db0" "t0" "f1" "o1"))))
         (progn
           (add-onion! onions "db0" "t1" "f2" "o1" "l2")
           (and (string= "l0" (lookup-seclevel onions "db0" "t0" "f0" "o0"))
                (string= "l1" (lookup-seclevel onions "db0" "t0" "f1" "o1"))
                (string= "l2" (lookup-seclevel onions "db0" "t1" "f2" "o1")))))))

(defun test-map-tree ()
  (and (equal '((2 3) (4 ((5))) 6)
              (map-tree #'(lambda (n) (1+ n)) '((1 2) (3 ((4))) 5)))
       (equal '()
              (map-tree #'(lambda (n) (declare (ignore n)) (assert nil)) '()))))

(defun test-slow-compare ()
  (and (slow-compare '((a b c)) '((a b c)))
       (not (slow-compare '((a b c)) '((a b c) (a b c))))
       (not (slow-compare '((a b)) '((a b c))))
       (slow-compare '((a b c) (d e f)) '((d e f) (a b c)))
       (slow-compare '((a b c) (d e f) (g h i)) '((d e f) (a b c) (g h i)))
       (not (slow-compare '((a b c) (d e f) (g h i)) '((d e f) (a b c))))))

(defun test-all-strings ()
  (and (equal '(("1" "2" "3" "4.00"))
              (all-strings '((1 2.0 3.000 "4.00"))))))

(defun test-list-depth ()
  (and (= 1 (list-depth '()))
       (= 1 (list-depth '(2 3 4)))
       (= 2 (list-depth '(1 (2) (4) 7)))
       (= 3 (list-depth '(1 (2) 3 4 (5 (6) 7))))))

(defun test-all-units ()
  (and (test-fixup-onion-checks)
       (test-fixup-onion-checks-multiple-update-default-bug)
       (test-fixup-onion-checks-set)
       (test-fixup-execution-target)
       (test-fixup-testing-strategy)
       (test-lookup-seclevel)
       (test-update-onion-state!)
       (test-many-str-assoc)
       (test-add-onion!)
       (test-map-tree)
       (test-slow-compare)
       (test-all-strings)
       (test-list-depth)))

