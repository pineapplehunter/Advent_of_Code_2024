(load (posix-getenv "ASDF"))
(asdf:load-system :str)
(asdf:load-system :cl-ppcre)

(defun get-file (filename)
  (with-open-file (stream filename)
    `(,(loop for line = (read-line stream nil)
             while (string/= "" line)
             collect (map 'list (lambda (x) (string x)) line))
      ,(let ((inst ""))
         (loop for line = (read-line stream nil)
               while line
               do (setf inst (concatenate 'string inst line))
               finally (return inst))))))

(defun inputs ()
  (get-file #p"../../inputs/day15/input"))

(defun field-array (field-raw)
  (make-array `(,(length (first field-raw)) ,(length field-raw))
              :initial-contents field-raw))

(defun offset-direction (direction)
  (cond
    ((string= ">" direction) '( 1  0))
    ((string= "<" direction) '(-1  0))
    ((string= "^" direction) '( 0 -1))
    ((string= "v" direction) '( 0  1))))

(defun add-pos (pos1 pos2)
  (destructuring-bind ((x1 y1) (x2 y2)) `(,pos1 ,pos2)
    `(,(+ x1 x2) ,(+ y1 y2))))

(defun move (field x y direction)
  (let ((this (aref field y x)))
    (cond
      ((string= this "#") nil)
      ((string= this ".") t)
      (t (destructuring-bind (next-x next-y)
             (add-pos `(,x ,y) (offset-direction direction))
           (when (move field next-x next-y direction)
             (setf (aref field next-y next-x) this)
             (setf (aref field y x) ".")))))))


(defmacro table-loop (table syms op &body body)
  `(loop for ,(second syms) from 0 below (array-dimension ,table 0)
         ,op (loop for ,(first syms) from 0 below (array-dimension ,table 1)
                   ,op ,@body)))

(defun move-robot (field direction)
  (let ((robot-x) (robot-y))
    (table-loop field (x y)
        do (when (string= "@" (aref field y x))
             (setf robot-x x)
             (setf robot-y y)))
    (move field robot-x robot-y direction)))

(defun move-robot-inst (field inst)
  (loop for d across inst
        do (move-robot field (string d))
        finally (return field)))

(defun calc-score (field)
  (let ((score 0))
    (table-loop field (x y)
        do (when (string= "O" (aref field y x))
             (incf score (+ x (* 100 y)))))
    score))

(defun main ()
  (format t "ans: ~A~%"
          (destructuring-bind (field-raw inst) (inputs)
            (let ((field (field-array field-raw)))
              (loop for d across inst
                    do (move-robot field (string d)))
              (calc-score field)))))

(main)
