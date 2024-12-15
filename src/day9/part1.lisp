(load (posix-getenv "ASDF"))
(defpackage :day9
  (:use :cl :asdf))
(in-package :day9)

(asdf:load-system :str)

(defun get-file (filename)
  (with-open-file (stream filename)
    (let ((line (read-line stream nil)))
      (map 'list (lambda (x) (parse-integer (string x))) line))))

;; parse input file
(defparameter inputs
  (get-file "../inputs/day9/input"))

(defun expand-free (lst id)
  (when lst
    (append (make-sequence 'list (car lst))
            (expand-file (cdr lst) id))))

(defun expand-file (lst id)
  (when lst
    (append (fill (make-sequence 'list (car lst)) id)
            (expand-free (cdr lst) (1+ id)))))

(defun compress (input-list)
  (let* ((lst (make-array (length input-list) :initial-contents input-list))
        (hptr 0)
        (tptr (1- (length lst))))
    (loop while (< hptr tptr)
          do (progn
               (loop while (aref lst hptr)
                     do (incf hptr))
               (loop while (not (aref lst tptr))
                     do (incf tptr -1))
               (when (< hptr tptr)
                 (setf (aref lst hptr) (aref lst tptr))
                 (setf (aref lst tptr) nil)))
          finally (return (loop for i below (array-dimension lst 0)
                                collect (aref lst i))))))

(print (loop for val in (compress (expand-file inputs 0))
             for idx from 0
             when val
             sum (* val idx)))
