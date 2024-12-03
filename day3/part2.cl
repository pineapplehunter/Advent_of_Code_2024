(require :asdf)
(asdf:load-system "cl-ppcre")
(asdf:load-system "str")

(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
              collect line)))

(defparameter inputs (get-file "input"))

(defparameter pat "mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)")
(defparameter matching
  (reduce #'append
	  (mapcar (lambda (x)
		    (cl-ppcre:all-matches-as-strings pat x))
		  inputs)))

(defparameter parsed
  (mapcar (lambda (x) (if (string= "do()" x)
			  '(:do)
			  (if (string= "don't()" x)
			      '(:dont)
			      (cons :mul (mapcar #'parse-integer
						 (cl-ppcre:all-matches-as-strings "\\d+" x))))))
	  matching))

(defparameter result
  (let ((flag t)
	(sum 0))
    (loop for x in parsed
	  do (cond
	       ((eq :do (first x)) (setf flag t))
	       ((eq :dont (first x)) (setf flag nil))
	       (flag (incf sum (* (second x) (nth 2 x)))))
	  finally (return sum))))

(print result)
