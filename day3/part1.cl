(require :asdf)
(asdf:load-system "cl-ppcre")
(asdf:load-system "str")

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defparameter inputs (get-file "../inputs/day3/input"))

(defparameter pat "mul\\(\\d+,\\d+\\)")
(defparameter matching (reduce #'append (mapcar (lambda (x) (cl-ppcre:all-matches-as-strings pat x)) inputs)))

(defparameter mvalues (mapcar (lambda (x) (mapcar #'parse-integer
                                                  (cl-ppcre:all-matches-as-strings "\\d+" x)))
                              matching))

(print (reduce #'+ (mapcar (lambda (x) (* (first x) (second x))) mvalues)))
