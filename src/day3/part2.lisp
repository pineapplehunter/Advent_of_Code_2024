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

(defparameter pat "mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)")

(defun matching (line)
  (cl-ppcre:all-matches-as-strings pat line))

(defun mul-values (pat)
  (mapcar #'parse-integer
          (cl-ppcre:all-matches-as-strings "\\d+" pat)))

(defun main ()
  (format t "ans: ~A~%"
          (let ((parsed (loop for line in (inputs)
                              nconc (matching line)))
                (sum 0)
                (flag t))
            (loop for x in parsed
                  do (cond
                       ((string= "do()" x) (setf flag t))
                       ((string= "don't()" x) (setf flag nil))
                       (flag (destructuring-bind (a b) (mul-values x)
                               (incf sum (* a b))))))
            sum)))

(main)
