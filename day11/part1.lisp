(load (posix-getenv "ASDF"))
(asdf:load-system :str)

(defun get-file (filename)
  (with-open-file (stream filename)
    (let ((line (read-line stream nil)))
      (loop for s in (str:split " " line)
            when (string/= "" s)
              collect (parse-integer s)))))

(defparameter inputs (get-file #p"../inputs/day11/input"))

(defun blink (lst)
  (reduce
   #'append
   (loop for x in lst
         for xstr = (format nil "~A" x)
         collect
         (cond
           ((= x 0) '(1))
           ((= (mod (length xstr) 2) 0)
            (list (parse-integer (subseq xstr 0 (/ (length xstr) 2)))
                  (parse-integer (subseq xstr (/ (length xstr) 2) (length xstr)))))
           (t `(,(* 2024 x)))))))

(defun blink-n (lst n)
  (let ((out (copy-tree lst)))
    (loop for i from 0 below n
          do (format t "blink ~A~%" i)
             (setf out (blink out)))
    out))

(print (length (blink-n inputs 25)))
