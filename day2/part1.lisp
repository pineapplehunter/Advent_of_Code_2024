(require "asdf")
(asdf:load-system "str")

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun parse-file (filename)
  (mapcar (lambda (x)
            (let* ((splitted (str:split " " x))
                   (no-empty (remove-if (lambda (y) (string= y "")) splitted))
                   (numbers (mapcar #'parse-integer no-empty)))
              numbers))
          (get-file filename)))

(defparameter parsed (parse-file "../inputs/day2/input"))

(defun windowed-iteration (lst)
  (loop for i from 0 to (1- (length lst))
        for first = (nth i lst)
        for second = (nth (1+ i) lst)
        when second
          collect (list first second)))

(defun increasing (lst)
  (every (lambda (x)
           (let ((f (first x)) (s (second x))) (< f s)))
         (windowed-iteration lst)))

(defun decreasing (lst)
  (every (lambda (x)
           (let ((f (first x)) (s (second x))) (> f s)))
         (windowed-iteration lst)))

(defun diff-smaller-than-3 (lst)
  (every (lambda (x)
           (let ((f (first x)) (s (second x)))
             (<= (abs (- f s)) 3)))
         (windowed-iteration lst)))

(defun is-ok (lst)
  (and
   (or (increasing lst)
       (decreasing lst))
   (diff-smaller-than-3 lst)))


(defparameter ok-list (mapcar #'is-ok parsed))

(print (length (remove nil ok-list)))
