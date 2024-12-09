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
  (let ((lst (copy-tree input-list))
        (index 0))
    (loop while (nthcdr index lst)
          do (progn
               ;;(print lst)
               (when (not (nth index lst))
                 (loop while (not (first (last lst)))
                       do (setf lst (butlast lst)))
                 (setf (nth index lst) (first (last lst)))
                 (setf lst (butlast lst)))
               (incf index)))
    lst))

(print (loop for val in (compress (expand-file inputs 0))
             for idx from 0
             sum (* val idx)))
