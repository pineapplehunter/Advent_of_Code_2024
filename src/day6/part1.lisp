(require :asdf)
(asdf:load-system "str")

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (map 'list
                       (lambda (c)
                         (cond
                           ((eq c #\.) :_)
                           ((eq c #\#) :X)
                           ((eq c #\^) :^)
                           (t (print "no matching"))))
                       line))))

;; parse input file
(defparameter inputs
  (get-file "../inputs/day6/input"))

(defun rot (board)
  (reverse
   (loop for i from 0 to (1- (length (first board)))
         collect (mapcar (lambda (x) (nth i x))
                         board))))

(defun get-pos (board)
  (block outer
    (loop for y from 0 to (length board)
          do (loop for x from 0 to (length (first board))
                   when (eq :^ (nth x (nth y board)))
                     return (return-from outer (list x y))
                   finally (return nil)))))

(defun next-pos (pos)
  (destructuring-bind (x y) pos
    (if (< 0 y)
        (list x (1- y))
        nil)))

(defun rot-pos (pos board)
  (destructuring-bind (x y) pos
    (list y (- (length board) x 1))))

(defun at-pos (pos board)
  (nth (first pos) (nth (second pos) board)))


(defparameter final-board
  (let ((board (copy-tree inputs))
        (pos (get-pos inputs)))
    (loop while pos
          do (progn
               ;;(print pos)
               ;;(loop for x in board do (print x))
               ;;(print "")
               (setf (nth (first pos) (nth (second pos) board)) :*)
               (cond
                 ((and (next-pos pos)
                       (eq :X (at-pos (next-pos pos) board)))
                  ;;(print "rotating")
                  (setf pos (rot-pos pos board))
                  (setf board (rot board)))
                 (t (setf pos (next-pos pos))))))
    board))

(print (loop for x in (reduce #'append final-board) sum (if (eq x :*) 1 0)))
