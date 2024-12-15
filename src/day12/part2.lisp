(load (posix-getenv "ASDF"))
(asdf:load-system :str)

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (map 'list (lambda (x) (intern (string x))) line))))

(defun inputs ()
  (get-file #p"../inputs/day12/input"))

(defun guarden (input)
  (make-array `(,(length (first input)) ,(length input))
              :initial-contents input))

(defun in-range (min max val)
  (when (and (<= min val) (< val max)) val))


(defmacro table-loop (table syms op &body body)
  `(loop for ,(second syms) from 0 below (array-dimension ,table 0)
         ,op (loop for ,(first syms) from 0 below (array-dimension ,table 1)
                   ,op ,@body)))

(defun get-region-at (x y sym guarden checked)
  (when (and (in-range 0 (array-dimension guarden 0) y)
             (in-range 0 (array-dimension guarden 1) x)
             (null (aref checked y x))
             (eq sym (aref guarden y x)))
    (setf (aref checked y x) t)
    (flet ((f (offsetx offsety)
             (get-region-at (+ x offsetx) (+ y offsety) sym guarden checked)))
      (nconc `((,x ,y))
             (f 1 0)
             (f 0 1)
             (f -1 0)
             (f 0 -1)))))

(defun get-all-regions (guarden)
  (let ((checked (make-array (array-dimensions guarden)
                             :initial-element nil))
        (regions))
    (table-loop guarden (x y)
        do (let ((r (get-region-at x y (aref guarden y x) guarden checked)))
             (when r (push r regions))))
    (reverse regions)))

(defun perimeter-at (x y guarden)
  (loop for offsetx in '(1 -1 0 0)
        for offsety in '(0 0 1 -1)
        when (let ((checkx (in-range 0 (array-dimension guarden 1)
                                     (+ x offsetx)))
                   (checky (in-range 0 (array-dimension guarden 0)
                                     (+ y offsety))))
               (not (and checkx
                         checky
                         (eq (aref guarden y x)
                             (aref guarden checky checkx)))))
          collect `(,(+ x offsetx) ,(+ y offsety))))

(defun unique (lst &optional (test #'equal))
  (let ((out))
    (loop for x in lst
          when (loop for y in out
                     when (funcall test x y)
                       return nil
                     finally (return t))
            do (push x out)
          finally (return out))))

(defun perimeter-of-region (region guarden)
  (reduce #'nconc
          (loop for p in region
                when (perimeter-at (first p) (second p) guarden)
                  collect (perimeter-at (first p) (second p) guarden))))

(defun draw-region (region)
  (let ((xmin 9999) (xmax 0) (ymin 9999) (ymax 0) (canvas))
    (loop for r in region
          do (destructuring-bind (x y) r
               (progn
                 (when (< x xmin) (setf xmin x))
                 (when (< xmax x) (setf xmax x))
                 (when (< y ymin) (setf ymin y))
                 (when (< ymax y) (setf ymax y)))))
    (setf canvas (make-array `(,(- (+ 3 ymax) ymin) ,(- (+ 3 xmax) xmin)) :initial-element 0))
    (loop for r in region
          do (destructuring-bind (x y) r
               (setf (aref canvas (1+ (- y ymin)) (1+ (- x xmin))) 1)))
    canvas))

(defun draw-diff-x (canvas)
  (let ((out (make-array `(,(array-dimension canvas 0) ,(1- (array-dimension canvas 1)))
                         :initial-element 0)))
    (table-loop out (x y)
        do (setf (aref out y x) (- (aref canvas y x) (aref canvas y (1+ x)))))
    out))

(defun draw-diff-y (canvas)
  (let ((out (make-array `(,(1- (array-dimension canvas 0)) ,(array-dimension canvas 1))
                         :initial-element 0)))
    (table-loop out (x y)
        do (setf (aref out y x) (- (aref canvas y x) (aref canvas (1+ y) x))))
    out))

(defun count-edge (canvas)
  (table-loop canvas (x y) sum (abs (aref canvas y x))))

(defun main ()
  (format t "ans: ~A~%"
          (let ((guarden (guarden (inputs))))
            (loop for region in (get-all-regions guarden)
                  for area = (length region)
                  for perim = (count-edge (draw-diff-y (draw-diff-x (draw-region region))))
                  sum (progn
                        (format t "~A * ~A~%" area perim)
                        (* area perim))))))

(time (main))
