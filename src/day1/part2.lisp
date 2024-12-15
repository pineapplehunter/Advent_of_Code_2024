(require :asdf)
(asdf:load-system "str")

(defun get-file (filename)
  "Get file input by filename"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (mapcar #'parse-integer (str:split-omit-nulls " " line)))))

(defun inputs ()
  "Parse inputs"
  (get-file "../inputs/day1/day1-input.txt"))

(defmacro inc-or-init (table value)
  "Increment value in hashmap or init to 1"
  (let ((access `(gethash ,value ,table)))
    `(if (null ,access) (setf ,access 1) (incf ,access))))

(defun main ()
  (let (;; initialize hash-tables
        (left-table (make-hash-table))
        (right-table (make-hash-table))
        ;; initialize a value for resulting list
        (left-list))
    ;; count values in hash-table
    (loop for x in (inputs)
          do (destructuring-bind (l r) x
               (inc-or-init left-table l)
               (inc-or-init right-table r)))
    ;; convert the left-table to a list
    (maphash (lambda (k v) (push `(,k ,v) left-list)) left-table)
    ;; sum and print
    (format t "ans: ~A~%"
            (loop for x in left-list
                  sum (let* ((k (first x))
                            (v (second x))
                            (s (gethash k right-table)))
                        (* (* k v) (if s s 0)))))))

(main)
