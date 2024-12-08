(require :asdf)
(asdf:load-system "str")

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (map 'list
                       (lambda (c)
                         (cond
                           ((eq c #\.) " ")
                           (t (string c))))
                       line))))

;; parse input file
(defparameter inputs
  (get-file "../inputs/day8/input"))

(defun num-in-range (min max val)
  (when (<= min val)
    (when (< val max)
      val)))

(defparameter final-table
  (let* ((table (copy-tree inputs))
         (antinodes
           (loop for y in table
                 collect (loop for x in y
                               collect 0)))
         (xsize (length (first table)))
         (ysize (length table)))
    (labels ((loop-table (f)
               (loop for y from 0 to (1- (length table))
                     do (loop for x from 0 to (1- (length (first table)))
                              do (funcall f x y))))
             (multi-loop-table (f)
               (loop-table
                (lambda (x1 y1)
                  (loop-table
                   (lambda (x2 y2)
                     (funcall f x1 y1 x2 y2))))))
             (inc-pos (x y)
               (incf (nth x (nth y antinodes))))
             (get-pos (x y)
               (nth x (nth y table))))
      (multi-loop-table
       (lambda (x1 y1 x2 y2)
         (when (and (not (equal (list x1 y1) (list x2 y2)))
                    (string/= (get-pos x1 y1) " ")
                    (string= (get-pos x1 y1) (get-pos x2 y2)))
           (let ((xpos x1)
                 (ypos y1))
             (loop while (and xpos ypos)
                   do (inc-pos xpos ypos)
                      (setf xpos (num-in-range 0 xsize (+ xpos (- x2 x1))))
                      (setf ypos (num-in-range 0 ysize (+ ypos (- y2 y1))))))
           (let ((xpos x1)
                 (ypos y1))
             (loop while (and xpos ypos)
                   do (inc-pos xpos ypos)
                      (setf xpos (num-in-range 0 xsize (- xpos (- x2 x1))))
                      (setf ypos (num-in-range 0 ysize (- ypos (- y2 y1))))))
           ))))
    antinodes))

(print (loop for n in (reduce #'append final-table)
             when (/= 0 n)
               sum 1))
