(require "asdf")
(asdf:load-system "str")

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (loop for s in (str:split " " line)
                        when (string/= "" s)
                          collect (parse-integer s)))))

(defun inputs () (get-file "../inputs/day1/day1-input.txt"))

(defun left (lst) (mapcar #'first lst))
(defun right (lst) (mapcar #'second lst))

(defun diffs (inputs)
  (mapcar (lambda (a b) (abs (- a b)))
          (sort (left inputs) #'<)
          (sort (right inputs) #'<)))

(defun sum (lst) (reduce #'+ lst))

(format t "ans: ~A~%" (sum diffs))
