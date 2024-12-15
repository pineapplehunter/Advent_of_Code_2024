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

(defparameter incorrect-pages
  (loop for p in pages
        when (not (correct-order p))
          collect p))

(defparameter sorted-pages
  (loop for page in incorrect-pages
        collect (sort page
                      (lambda (a b)
                        (loop for p in patterns
                              when (equal p (list a b))
                                return t
                              finally (return nil))))))

(print (loop for p in sorted-pages
             sum (nth (/ (1- (length p)) 2) p)))
