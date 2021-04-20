#lang racket

#| Write a function to perform integer division without using either / or * operations.
   Find a fast way to do it. |#

;;; (divide a b) -> integer?
;;; Gives the result of (/ a b) without the remainder
(define (divide a b)
  (if (zero? b) 
      (error "division by zero")  
      (let ([sign (if (xor (< a 0) (< b 0)) -1 1)])
        (let loop ([a (abs a)] [b (abs b)] [counter 0])
            (if (<= a b)
                (add1 counter)
                (* sign (loop (- a b) b (add1 counter))))))))
