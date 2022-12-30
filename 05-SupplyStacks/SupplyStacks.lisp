#!/usr/bin/env sbcl --script

;; Supply Stacks
;; https://adventofcode.com/2022/day/5

(defstruct move count from-stack to-stack)

(defun read-file (filename)
    "Read the file named FILENAME and return a list of lines."
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
              while line
              collect line)))

(defun parse-move (line)
    "Parse line string into a move structure."
    (let ((words (sb-unicode:words line)))
        ; subtract 1 from stack numbers for 0-based indexing
        (make-move
            :count (parse-integer (nth 2 words))
            :from-stack (- (parse-integer (nth 6 words)) 1)
            :to-stack (- (parse-integer (nth 10 words)) 1))))

(defun solve-part-1 (stacks moves)
    "Apply each move in moves to the stacks using the part 1 rules."

    (defun move-one-item (from to)
        "Move a single item from one stack to another."
        (let ((item (car (aref stacks from))))
            (setf (aref stacks to) (cons item (aref stacks to)))
            (setf (aref stacks from) (cdr (aref stacks from)))))

    (defun do-move (move)
        "Apply the given move to the stacks."
        (dotimes (i (move-count move))
            (move-one-item (move-from-stack move) (move-to-stack move))))

    (loop for move in moves do (do-move move))
    (loop for stack across stacks collect (car stack)))

(defun solve-part-2 (stacks moves)
    "Apply each move in moves to the stacks using the part 2 rules."

    (defun move-items (from to n)
        "Move n items from one stack to another."
        (let ((items (subseq (aref stacks from) 0 n)))
            (setf (aref stacks to) (append items (aref stacks to)))
            (setf (aref stacks from) (subseq (aref stacks from) n))))

    (defun do-move (move)
        "Apply the given move to the stacks."
        (move-items (move-from-stack move) (move-to-stack move) (move-count move)))

    (loop for move in moves do (do-move move))
    (loop for stack across stacks collect (car stack)))

(defun initial-stacks ()
    "Hardcode the stacks from the input for now instead of parsing."
    (make-array 9
        :initial-contents
        '((#\t #\v #\j #\w #\n #\r #\m #\s)
          (#\v #\c #\p #\q #\j #\d #\w #\b)
          (#\p #\r #\d #\h #\f #\j #\b)
          (#\d #\n #\m #\b #\p #\r #\f)
          (#\b #\t #\p #\r #\v #\h)
          (#\t #\p #\b #\c)
          (#\l #\p #\r #\j #\b)
          (#\w #\b #\z #\t #\l #\s #\c #\n)
          (#\g #\s #\l))))
    
(let ((stacks (initial-stacks))
      (moves (mapcar 'parse-move (read-file "input-moves.txt"))))
    (format t "Part 1: ~{~A~}~%" (solve-part-1 stacks moves)))
    
(let ((stacks (initial-stacks))
      (moves (mapcar 'parse-move (read-file "input-moves.txt"))))
    (format t "Part 2: ~{~A~}~%" (solve-part-2 stacks moves)))
