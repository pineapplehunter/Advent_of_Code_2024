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
  (make-array (list (length (first input))
                    (length input))
              :initial-contents input))

(defun in-range (min max val)
  (when (and (<= min val)
             (< val max))
    val))

(defun get-region-at (x y sym guarden checked)
  (when (and (in-range 0 (array-dimension guarden 0) y)
             (in-range 0 (array-dimension guarden 1) x)
             (null (aref checked y x))
             (eq sym (aref guarden y x)))
    (setf (aref checked y x) t)
    (append `((,x ,y))
            (get-region-at (1+ x) y sym guarden checked)
            (get-region-at x (1+ y) sym guarden checked)
            (get-region-at (1- x) y sym guarden checked)
            (get-region-at x (1- y) sym guarden checked))))

(defun get-all-regions (guarden)
  (let ((checked (make-array (array-dimensions guarden)
                             :initial-element nil))
        (regions))
    (loop for y from 0 below (array-dimension guarden 0)
          do (loop for x from 0 below (array-dimension guarden 1)
                   do (let ((r (get-region-at x y (aref guarden y x) guarden checked)))
                        (when r (push r regions)))))
    regions))

(defun perimeter-at (x y guarden)
  (loop for offsetx in '(1 -1 0 0)
        for offsety in '(0 0 1 -1)
        sum (let ((checkx (in-range 0 (array-dimension guarden 1)
                                    (+ x offsetx)))
                  (checky (in-range 0 (array-dimension guarden 0)
                                    (+ y offsety))))
              (if (and checkx
                       checky
                       (eq (aref guarden y x)
                           (aref guarden checky checkx)))
                  0 1))))

(defun perimeter-of-region (region guarden)
  (loop for p in region
        sum (perimeter-at (first p) (second p) guarden)))

(defun main ()
  (print
   (let ((guarden (guarden (inputs))))
     (loop for region in (get-all-regions guarden)
           for perimeter = (perimeter-of-region region guarden)
           sum (* (length region) perimeter)))))

(main)
