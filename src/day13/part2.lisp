(load (posix-getenv "ASDF"))
(asdf:load-system :str)
(asdf:load-system :cl-ppcre)

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for A = (read-line stream nil)
          for B = (read-line stream nil)
          for C = (read-line stream nil)
          for _ = (read-line stream nil)
          while (and A B C)
          collect (mapcar (lambda (x)
                            (mapcar #'parse-integer 
                                    (cl-ppcre:all-matches-as-strings "\\d+" x)))
                          `(,A ,B ,C)))))

(defun inputs ()
  (get-file #p"../inputs/day13/input"))

(defun mat-inv (mat)
  (destructuring-bind ((a b) (c d)) mat
    (let ((det (- (* a d) (* b c))))
      `((,(/ d det) ,(/ (- b) det))
        (,(/ (- c) det) ,(/ a det))))))

(defun mat-mul (mata matb)
  (destructuring-bind (((a b) (c d)) ((e f) (g h))) `(,mata ,matb)
    `((,(+ (* a e) (* b g)) ,(+ (* a f) (* b h)))
      (,(+ (* c e) (* d g)) ,(+ (* c f) (* d h))))))

(defun mat-transpose (mat)
  (destructuring-bind ((a b) (c d)) mat
    `((,a ,c)
      (,b ,d))))

(defun main ()
  (format t "ans: ~A~%"
          (loop for x in (inputs)
                for out = (destructuring-bind (a b (o1 o2)) x
                            (mat-transpose
                             (mat-mul (mat-inv (mat-transpose `(,a ,b)))
                                      (mat-transpose
                                       `((,(+ o1 10000000000000) ,(+ o2 10000000000000))
                                         (0                      0))))))
                for a = (first (first out))
                for b = (second (first out))
                when (and (integerp a)
                          (integerp b))
                  sum (+ (* 3 a) b))))

(main)
