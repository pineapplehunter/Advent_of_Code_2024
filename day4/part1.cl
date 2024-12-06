(require :asdf)
(asdf:load-system "str")

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defparameter inputs
  (get-file "../inputs/day4/input"))

(defparameter input-table
  (mapcar (lambda (x)
            (map 'list
                 (lambda (y)
                   (cond
                     ((eq #\X y) :x)
                     ((eq #\M y) :m)
                     ((eq #\A y) :a)
                     ((eq #\S y) :s)))
                 x))
          inputs))

(defparameter xsize (length (first input-table)))
(defparameter ysize (length input-table))

(defparameter target '(:x :m :a :s))

(defun checkpos (x y)
  (labels ((get-offset (ox oy)
             (let ((px (+ x ox))
                   (py (+ y oy)))
               (cond ((< px 0) :?)
                     ((< (1- xsize) px) :?)
                     ((< py 0) :?)
                     ((< (1- ysize) py) :?)
                     (t (nth px (nth py input-table))))))
           (offset-list (o1 o2 o3 o4)
             (list (get-offset (first o1) (second o1))
                   (get-offset (first o2) (second o2))
                   (get-offset (first o3) (second o3))
                   (get-offset (first o4) (second o4)))))
    (+
     (if (equal target (offset-list '(0 0) '( 0  1) '( 0  2) '( 0  3))) 1 0)
     (if (equal target (offset-list '(0 0) '( 0 -1) '( 0 -2) '( 0 -3))) 1 0)
     (if (equal target (offset-list '(0 0) '( 1  0) '( 2  0) '( 3  0))) 1 0)
     (if (equal target (offset-list '(0 0) '(-1  0) '(-2  0) '(-3  0))) 1 0)
     (if (equal target (offset-list '(0 0) '( 1  1) '( 2  2) '( 3  3))) 1 0)
     (if (equal target (offset-list '(0 0) '(-1  1) '(-2  2) '(-3  3))) 1 0)
     (if (equal target (offset-list '(0 0) '( 1 -1) '( 2 -2) '( 3 -3))) 1 0)
     (if (equal target (offset-list '(0 0) '(-1 -1) '(-2 -2) '(-3 -3))) 1 0))))

;;(loop for y from 0 to (1- ysize)
;;      do (print (loop for x from 0 to (1- xsize)
;;                     collect (checkpos x y))))

;;(loop for x in input-table do (print x))

(print (loop for y from 0 to (1- ysize)
             sum (loop for x from 0 to (1- xsize)
                       sum (checkpos x y))))
