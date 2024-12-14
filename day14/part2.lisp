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

(defun gen-n (n &optional (out t))
  (progn
       (format out "~%n: ~A~%" n)
       (let ((field (generate-field-n (inputs) n)))
         (loop for y from 0 below fieldy
               do (format out "~A~%"
                          (apply #'concatenate 'string
                                 (loop for x from 0 below fieldx
                                       collect (if (= 0 (aref field y x)) " " "#"))))))))

(defun main ()
  (with-open-file (str "output.txt"
                       :direction :output
                       :if-exists :overwrite)
  (loop for n from 5000 to 10000
        do (gen-n n str))))

;;(main)
