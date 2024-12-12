(require "asdf")
(asdf:load-system "str")

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (mapcar #'parse-integer
                          (str:split-omit-nulls " " line)))))

(defun inputs ()
  (get-file "../inputs/day2/input"))

(defun windowed-iteration (lst)
  (loop for a in lst
        for b in (rest lst)
        collect `(,a ,b)))

(defun increasing (lst)
  (every (lambda (x) (destructuring-bind (a b) x (< a b)))
         (windowed-iteration lst)))

(defun decreasing (lst)
  (every (lambda (x) (destructuring-bind (a b) x (> a b)))
         (windowed-iteration lst)))

(defun diff-smaller-than-3 (lst)
  (every (lambda (x) (destructuring-bind (a b) x (<= (abs (- a b)) 3)))
         (windowed-iteration lst)))

(defun is-ok (lst)
  (and
   (or (increasing lst)
       (decreasing lst))
   (diff-smaller-than-3 lst)))

(defun main ()
  (format t "ans: ~A~%"
          (loop for x in (inputs)
                when (is-ok x)
                  sum 1)))

(main)
