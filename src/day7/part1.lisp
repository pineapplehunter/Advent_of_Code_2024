(require :asdf)
(asdf:load-system "str")

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

;; parse input file
(defparameter inputs
  (get-file "../inputs/day7/input"))

(defparameter parsed
  (loop for line in inputs
        collect (destructuring-bind
                    (f r) (str:split ":" line)
                  (list (parse-integer f)
                        (mapcar #'parse-integer (str:split-omit-nulls " " r))))))

(defun check-if-ok (acc lst ans)
  (if (null lst)
      (= acc ans)
      (or (check-if-ok (+ acc (first lst)) (rest lst) ans)
          (check-if-ok (* acc (first lst)) (rest lst) ans))))

(print (loop for input in parsed
             sum (destructuring-bind (ans lst) input
                   (if (check-if-ok 0 lst ans) ans 0))))
