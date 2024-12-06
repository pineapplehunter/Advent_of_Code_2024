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

(defun will-loop-with-extra-obsticle (extra-pos)
  (let ((board (copy-tree inputs))
        (pos (get-pos inputs))
        (will-loop nil))
    (if (equal extra-pos pos)
        nil
        (progn
          (setf (nth (first extra-pos) (nth (second extra-pos) board)) :X)
          (loop while (and pos (not will-loop))
                do (progn
                     (cond
                       ((and (next-pos pos)
                             (eq :X (at-pos (next-pos pos) board)))
                        ;;(print "rotating")
                        (loop while (and (next-pos pos)
                                         (eq :X (at-pos (next-pos pos) board)))
                              do (progn
                                   (setf pos (rot-pos pos board))
                                   (setf board (rot board))))
                        (if (eq :@ (at-pos pos board)) (setf will-loop t))
                        (setf (nth (first pos) (nth (second pos) board)) :@))
                       (t (if (not (eq :@ (at-pos pos board)))
                              (setf (nth (first pos) (nth (second pos) board)) :*))
                          (setf pos (next-pos pos))))
                     ;;(print pos)
                     ;;(loop for x in board do (print x))
                     ))
          will-loop))))

(defun trace-route (extra-pos)
  (let ((board (copy-tree inputs))
        (pos (get-pos inputs))
        (will-loop nil))
    (if (equal extra-pos pos)
        nil
        (progn
          (setf (nth (first extra-pos) (nth (second extra-pos) board)) :X)
          (loop while (and pos (not will-loop))
                do (progn
                     (cond
                       ((and (next-pos pos)
                             (eq :X (at-pos (next-pos pos) board)))
                        (print "rotating")
                        (loop while (and (next-pos pos)
                                         (eq :X (at-pos (next-pos pos) board)))
                              do (progn
                                   (setf pos (rot-pos pos board))
                                   (setf board (rot board))))
                        (if (eq :@ (at-pos pos board)) (setf will-loop t))
                        (setf (nth (first pos) (nth (second pos) board)) :@))
                       (t (if (not (eq :@ (at-pos pos board)))
                              (setf (nth (first pos) (nth (second pos) board)) :*))
                          (setf pos (next-pos pos))))
                     ;;(print pos)
                     ;;(loop for x in board do (print x))
                     ))
          (loop for x in board do (print x))))))

(defparameter final-board
  (let ((board (copy-tree inputs))
        (pos (get-pos inputs))
        (rot-count 0))
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
                  (setf board (rot board))
                  (incf rot-count))
                 (t (setf pos (next-pos pos))))))
    (loop while (not (= (mod rot-count 4) 0))
          do (progn
               ;;(print "adjust")
               (setf board (rot board))
               (incf rot-count)))
    board))

(defparameter obsticle-ok-list
  (loop for y from 0 to (1- (length inputs))
        collect (loop for x from 0 to (1- (length (first inputs)))
                      collect (progn
                                (print (list x y))
                                (if (eq :* (at-pos (list x y) final-board))
                                    (will-loop-with-extra-obsticle (list x y))
                                    nil)))))

;;(loop for x in obsticle-ok-list
;;      do (print (mapcar (lambda (y)
;;                          (if y :O :_))
;;                        x)))

(print (loop for x in (reduce #'append obsticle-ok-list) sum (if x 1 0)))
