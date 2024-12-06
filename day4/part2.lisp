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

(defparameter target '(:m :a :s))

(defun checkpos (x y)
  (labels ((get-offset (ox oy)
             (let ((px (+ x ox))
                   (py (+ y oy)))
               (cond ((< px 0) :?)
                     ((< (1- xsize) px) :?)
                     ((< py 0) :?)
                     ((< (1- ysize) py) :?)
                     (t (nth px (nth py input-table))))))
           (offset-list (o1 o2)
             (list (get-offset (first o1) (second o1))
                   (get-offset (first o2) (second o2)))))
    (if (and (eq :a (get-offset 0 0))
             (or
              (equal '(:m :s) (offset-list '(-1 -1) '(1 1)))
              (equal '(:m :s) (offset-list '(1 1) '(-1 -1))))
             (or
              (equal '(:m :s) (offset-list '(-1 1) '(1 -1)))
              (equal '(:m :s) (offset-list '(1 -1) '(-1 1)))))
        1 0)))

(loop for y from 0 to (1- ysize)
      do (print (loop for x from 0 to (1- xsize)
                      collect (checkpos x y))))

(loop for x in input-table do (print x))

(print (loop for y from 0 to (1- ysize)
             sum (loop for x from 0 to (1- xsize)
                       sum (checkpos x y))))
