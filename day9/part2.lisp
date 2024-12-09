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

(defun refactor-list (input-list)
  (let ((lst '())
        (is-file t)
        (index 0))
    (loop for x in input-list
          do (progn
               (if is-file
                   (progn (push `(,index ,x) lst)
                          (incf index))
                   (push `(:free ,x) lst))
               (setf is-file (not is-file))))
    (reverse lst)))

(defun is-free (item) (eq :free (first item)))
(defun get-size (item) (second item))

(defun compress (input-list)
  (let ((lst (copy-tree input-list))
        (max-id (first (first (last input-list)))))
    (loop for id in (reverse (loop for i from 0 to max-id collect i))
          do (let* ((id-idx (loop for x in lst
                                  for idx from 0
                                  when (and (not (is-free x))
                                            (= (first x) id))
                                    return idx))
                    (idx-val (nth id-idx lst))
                    (first-free-idx (loop for x in lst
                                          for idx from 0
                                          when (and (is-free x)
                                                    (<= (get-size idx-val)
                                                        (get-size x)))
                                            return idx)))
               (when (and first-free-idx
                          (< first-free-idx id-idx))
                 (print id)
                 ;; replace element with free
                 (setf lst (append (subseq lst 0 id-idx)
                                   (list `(:free ,(second idx-val)))
                                   (nthcdr (1+ id-idx) lst)))
                 ;; merge free
                 (let ((idx 0))
                   (loop while (nth (1+ idx) lst)
                         do (if (and (is-free (nth idx lst))
                                     (is-free (nth (1+ idx) lst)))
                                (progn
                                  (setf (second (nth idx lst))
                                        (+ (get-size (nth idx lst))
                                           (get-size (nth (1+ idx) lst))))
                                  (setf lst (append (subseq lst 0 (1+ idx))
                                                    (nthcdr (+ 2 idx) lst)))))
                            (incf idx)))
                 
                 ;; add the new element
                 (setf lst (append (subseq lst 0 first-free-idx)
                                   (if (= 0 (- (get-size (nth first-free-idx lst)) (get-size idx-val)))
                                       (list idx-val)
                                       (list idx-val
                                             `(:free ,(- (second (nth first-free-idx lst)) (second idx-val)))))
                                   (nthcdr (1+ first-free-idx) lst))))))
    lst))

(print (let ((idx 0))
         (loop for val in (compress (refactor-list inputs))
               sum (if (is-free val)
                       (progn
                         (incf idx (second val))
                         0)
                       (loop for _ from 1 to (second val)
                             sum (progn (incf idx)
                                        (* (first val) (1- idx))))))))
