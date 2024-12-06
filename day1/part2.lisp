(require :asdf)
(asdf:load-system "str")

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defparameter file-contents (get-file "../inputs/day1/day1-input.txt"))

;;(setf file-contents
;;      '(
;;      "3   4"
;;      "4   3"
;;      "2   5"
;;      "1   3"
;;      "3   9"
;;      "3   3"))

(defun filter-empty-string (lst)
  (remove-if #'(lambda (x) (string= x "")) lst))

(defun map-int (lst)
  (mapcar #'parse-integer lst))

(defparameter parsed
  (loop for x in file-contents
        collect (map-int
                 (filter-empty-string
                  (str:split " " x)))))

(defparameter left (mapcar #'first parsed))
(defparameter right (mapcar #'second parsed))

(defparameter left-table (make-hash-table))
(defparameter right-table (make-hash-table))

(defun inc-table (table val)
  (if (getHash val table)
      (incf (getHash val table) 1)
      (setf (getHash val table) 1)))

(loop for x in parsed
      do (inc-table left-table (first x))
         (inc-table right-table (second x)))

(defun hash-table-to-list (hash-table)
  "Convert a hash table to a list of key-value pairs."
  (let (result)
    (maphash (lambda (key value)
               (push (cons key value) result))  ; Push each key-value pair as a cons
             hash-table)
    result))  ; Return the list of pairs (reverse to maintain insertion order)

(defun sum (lst) (reduce #'+ lst))

(defparameter v
  (mapcar (lambda (x)
            (let* ((k (car x))
                   (v (cdr x))
                   (s (gethash k right-table)))
              (*	(* k v) (if s s 0))))
          (hash-table-to-list left-table)))

(print (sum v))
