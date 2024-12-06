(require :asdf)
(asdf:load-system "str")

(defun get-file (filename)
  (with-open-file (stream filename)
    (list
     (loop for line = (read-line stream nil)
           while (not (string= line ""))
           collect (mapcar #'parse-integer (str:split "|" line)))
     (loop for line = (read-line stream nil)
           while line
           collect (mapcar #'parse-integer (str:split "," line))))))

;; parse input file
(defparameter inputs
  (get-file "../inputs/day5/input"))

;; split input
(defparameter patterns (first inputs))
(defparameter pages (second inputs))

(defun correct-order (lst)
  (let ((f (first lst))
        (r (rest lst)))
    (if (null r)
        t
        (and (loop for x in r
                   when (loop for p in patterns
                              when (equal p (list x f))
                                return t
                              finally (return nil))
                     return nil
                   finally (return t))
             (correct-order r)))))

(defparameter correct-pages
  (loop for p in pages
        when (correct-order p) 
          collect p))

(print (loop for p in correct-pages
             sum (nth (/ (1- (length p)) 2) p)))
