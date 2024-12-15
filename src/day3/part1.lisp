(require :asdf)
(asdf:load-system :cl-ppcre)
(asdf:load-system :str)

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun inputs ()
  (get-file "../inputs/day3/input"))

(defparameter pat "mul\\(\\d+,\\d+\\)")

(defun matching (line)
  (cl-ppcre:all-matches-as-strings pat line))

(defun mul-values (pat)
  (mapcar #'parse-integer
          (cl-ppcre:all-matches-as-strings "\\d+" pat)))

(defun main ()
  (format t "ans: ~A~%"
          (loop for line in (inputs)
                sum (loop for m in (matching line)
                          sum (destructuring-bind (a b) (mul-values m)
                                (* a b))))))

(main)
