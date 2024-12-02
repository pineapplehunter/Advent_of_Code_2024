(require :asdf)
(asdf:load-system "str")

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defparameter file-contents (get-file "day1-input.txt"))
;(setf file-contents
;      '(
;      "3   4"
;      "4   3"
;      "2   5"
;      "1   3"
;      "3   9"
;      "3   3"))

(defun filter-empty-string (lst)
       (remove-if #'(lambda (x) (string= x "")) lst))

(defun map-int (lst)
       (mapcar #'parse-integer lst))

(defparameter parsed
      (loop for x in file-contents
      	    collect (map-int
		(filter-empty-string
			(str:split " " x)))))

(defparameter firsts (mapcar #'first parsed))
(defparameter seconds (mapcar #'second parsed))

(defparameter table-f (make-hash-table))
(defparameter table-s (make-hash-table))

(defun inc-table (table val)
    (if (getHash val table)
      	(setf (getHash val table) (1+ (getHash val table)))
	(setf (getHash val table) 1)))

(loop for x in parsed
      do (let
      	   ((f (first x))
            (s (second x)))
	    (inc-table table-f f)
	    (inc-table table-s s)
	   ))

(defun hash-table-to-list (hash-table)
  "Convert a hash table to a list of key-value pairs."
  (let (result)
    (maphash (lambda (key value)
               (push (cons key value) result))  ; Push each key-value pair as a cons
             hash-table)
    result))  ; Return the list of pairs (reverse to maintain insertion order)

(defun sum (lst) (reduce #'+ lst))

(defparameter v (mapcar (lambda (x)
	(let ((k (car x))
	      (v (cdr x)))
	     (*	(* k v)
		(if (gethash k table-s)
		    (gethash k table-s)
		    0))))
	(hash-table-to-list table-f)))

(print (sum v))