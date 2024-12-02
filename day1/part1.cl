(require "asdf")
(asdf:load-system "str")

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defparameter file-contents (get-file "day1-input.txt"))

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

(defparameter left-sorted (sort left #'<))
(defparameter right-sorted (sort right #'<))

(defparameter diffs
	      (mapcar
		(lambda (a b) (abs (- a b)))
		left-sorted
		right-sorted))

(defun sum (lst) (reduce #'+ lst))

(print (sum diffs))