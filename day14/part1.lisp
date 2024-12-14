(load (posix-getenv "ASDF"))
(asdf:load-system :str)
(asdf:load-system :cl-ppcre)

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (mapcar #'parse-integer 
                          (cl-ppcre:all-matches-as-strings "-?\\d+" line)))))

(defparameter fieldx 101)
(defparameter fieldy 103)

(defun inputs ()
  (get-file #p"../inputs/day14/input"))

(defun positions (inputs)
  (loop for (x y nil nil) in inputs
        collect `(,x ,y)))

(defun velocities (inputs)
  (loop for (nil nil x y) in inputs
        collect `(,x ,y)))

(defun generate-field-n (inputs n)
  (let ((positions (positions inputs))
        (velocities (velocities inputs))
        (field (make-array `(,fieldy ,fieldx)
                           :initial-element 0)))
    (loop for pos in positions
          for vel in velocities
          do (progn
               (loop for i from 0 below n
                     do (setf pos (destructuring-bind ((x y) (vx vy)) `(,pos ,vel)
                                    `(,(mod (+ x vx) fieldx) ,(mod (+ y vy) fieldy)))))
               (destructuring-bind (x y) pos
                 (incf (aref field y x)))))
    field))

(defun quads (inputs n)
  (let ((field (generate-field-n inputs n))
        (quadx (/ (1- fieldx) 2))
        (quady (/ (1- fieldy) 2)))
    (loop for ox in `(0 ,(1+ quadx))
          nconc (loop for oy in `(0 ,(1+ quady))
                        collect (loop for x from 0 below quadx
                                      sum (loop for y from 0 below quady
                                                sum (aref field (+ y oy) (+ x ox))))))))


(defun main ()
  (format t "ans: ~A~%"
  (let ((quads (quads (inputs) 100)))
    (reduce #'* quads))))

(main)
