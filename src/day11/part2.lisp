(load (posix-getenv "ASDF"))
(asdf:load-system :str)

(defun get-file (filename)
  (with-open-file (stream filename)
    (let ((line (read-line stream nil)))
      (loop for s in (str:split " " line)
            when (string/= "" s)
              collect (parse-integer s)))))

(defparameter inputs (get-file #p"../inputs/day11/input"))

(defparameter cache (make-hash-table :test 'equal))

(defun blink-n-stone (stone n)
  (if (= n 0)
      1
      (progn
        (when (null (gethash `(,stone ,n) cache))
          (setf (gethash `(,stone ,n) cache)
                (let ((xstr (format nil "~A" stone)))
                  (cond
                    ((= stone 0) (blink-n-stone 1 (1- n)))
                    ((= (mod (length xstr) 2) 0)
                     (let ((str1 (subseq xstr 0 (/ (length xstr) 2)))
                           (str2 (subseq xstr (/ (length xstr) 2) (length xstr))))
                       (+ (blink-n-stone (parse-integer str1) (1- n))
                          (blink-n-stone (parse-integer str2) (1- n)))))
                    (t (blink-n-stone (* 2024 stone) (1- n)))))))
        (gethash `(,stone ,n) cache))))

(defun blink-n-list (lst n)
  (loop for stone in lst
        sum (progn (format t "checking stone ~A~%" stone)
                   (blink-n-stone stone n))))

(print (blink-n-list inputs 75))
