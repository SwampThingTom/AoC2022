#!/usr/bin/env sbcl --script

;; Supply Stacks
;; https://adventofcode.com/2022/day/5

(defstruct move count from-stack to-stack)

(defun parse-stacks-line (stacks line)
    (defun add-item (item stack)
        (setf stack (cons item stack)))

    (let ((num-stacks (floor (/ (length line) 4))))
        (format t "Number of stacks: ~D~%" (+ num-stacks 1))
        (loop for s from 0 to num-stacks
            do (let ((c (+ (* s 4) 1)))
                (when (not (equal (char line c) #\SPACE)) 
                    (add-item (char line c) (aref stacks s)))))))

(defun read-file (filename)
    "Read the file named FILENAME and return a list of lines."
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
              while line
              collect line)))

(defun solve (stacks moves)
    "Applies each move in moves to the stacks."

    (defun move-one-item (from to)
        "Moves a single item from one stack to another."
        ;; (print stacks)
        (let ((item (car (aref stacks from))))
            ;; (format t "Moving item ~a from stack ~D to stack ~D " item from to)
            (setf (aref stacks to) (cons item (aref stacks to)))
            (setf (aref stacks from) (cdr (aref stacks from)))))

    (defun do-move (move)
        "Applies the given move to the stacks."
        (dotimes (i (move-count move))
            (move-one-item (move-from-stack move) (move-to-stack move))))

    (loop for move in moves do (do-move move))
    (loop for stack across stacks collect (car stack)))

(defvar stacks)
(defvar moves)

(setf stacks (make-array 3
    :initial-contents
    '(() () ())))

(print (parse-stacks-line stacks "    [D]    "))
(terpri)
(print stacks)

;; (setf stacks (make-array 3 
;;    :initial-contents 
;;    '((n z)
;;      (d c m)
;;      (p))))




;; (setf moves 
;;     (list (make-move :count 1 :from-stack 1 :to-stack 0)
;;           (make-move :count 3 :from-stack 0 :to-stack 2)
;;           (make-move :count 2 :from-stack 1 :to-stack 0)
;;           (make-move :count 1 :from-stack 0 :to-stack 1)))

;; (format t "Part 1: ~{~A~}~%" (solve stacks moves))
