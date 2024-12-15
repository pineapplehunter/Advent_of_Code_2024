(load (posix-getenv "ASDF"))

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (map 'list (lambda (x) (parse-integer (string x))) line))))

;; parse input file
(defparameter inputs
  (get-file "../inputs/day10/input"))

(defparameter grid
  (make-array `(,(length (first inputs)) ,(length inputs))
              :initial-contents inputs))

(defun in-range (min max val)
  (when (and (<= min val)
             (< val max))
    val))

(defun unique (lst)
  (let ((out))
    (loop for x in lst
          when (not (loop for y in out
                          when (equal x y)
                            return t))
            do (push x out)
          finally (return out))))

(defun search-neighbor (x y val)
  ;;(format t "x:~A y:~A val:~A~%" x y val)
  (cond
    ((not (in-range 0 (array-dimension grid 0) y)) nil)
    ((not (in-range 0 (array-dimension grid 1) x)) nil)
    ((/= val (aref grid y x)) nil)
    ((= 9 val) (format t "is 9~%") `((,x ,y)))
    (t (format t "Searching from (~A ~A)~%" x y)
       (append (search-neighbor (1+ x) y (1+ val))
               (search-neighbor x (1+ y) (1+ val))
               (search-neighbor (1- x) y (1+ val))
               (search-neighbor x (1- y) (1+ val))))))

(defparameter final-count
  (let ((out (make-array (array-dimensions grid) :initial-element 0)))
    (loop for y from 0 below (array-dimension grid 0)
          do (loop for x from 0 below (array-dimension grid 1)
                   do (setf (aref out y x) (length (search-neighbor x y 0)))))
    out))

(print (loop for y from 0 below (array-dimension grid 0)
             sum (loop for x from 0 below (array-dimension grid 1)
                       sum (aref final-count y x))))
