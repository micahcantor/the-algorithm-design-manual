#lang racket/base

;;; Exercise 4-31
;; Determine the maximum number in a sorted array that has been
;; circularly shifted some k elements to the right.

#| Part 1: k is known |#

(define (shifted-max vec k)
  (define pos (sub1 (modulo k (vector-length vec))))
  (vector-ref vec pos))

#| Part 2: k is unknown |#

;;; Look at the middle element
;;; If the element to its right is smaller,
;;; and the element to its left is equal or smaller,
;;; then it its the max

;;; Otherwise, if the element to the left is bigger
;;; then the element to the left is the max

;;; Otherwise, repeat in the right side of the array

(define (right vec p)
  (if (= p (sub1 (vector-length vec)))
      (vector-ref vec 0)
      (vector-ref vec (add1 p))))

(define (left vec p)
  (if (= p 0)
      (vector-ref vec (sub1 (vector-length vec)))
      (vector-ref vec (sub1 p))))

;;; Essentially peforms a one sided binary search
;;; on the vector as it looks for the maximum.
;;; Time complexity O(log(n))
(define (shifted-max-unknown vec)
  (let loop ([low 0] [high (vector-length vec)])
    (define mid-idx (quotient (+ low high) 2))
    (define mid-val (vector-ref vec mid-idx))
    (cond
      [(and (< (right vec mid-idx) mid-val)
            (<= (left vec mid-idx) mid-val))
       mid-idx]
      [(> (left vec mid-idx) mid-val)
       (if (zero? mid-idx) (vector-length vec) (sub1 mid-idx))]
      [else 
       (loop (add1 mid-idx) high)])))

#| Testing |#
(shifted-max-unknown (vector 32 45 5 15 20))
(shifted-max-unknown (vector 5 15 20 32 45))
(shifted-max-unknown (vector 20 32 45 5 15))