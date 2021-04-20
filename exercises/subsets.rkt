#lang racket/base
(require racket/vector)

#| Question 2.43: You are given a set S of n numbers. You must pick a subset S' of k numbers from S
   such that the probability of each element of S occurring in S' is equal (i.e each
   is selected with probability k/n). You may make only one pass over the numbers.
   What if n is unknown? |#

#| S = {1, 2, 3, 4, 5} , size n
   k = 3
   S' = subset of S of size k
|#

#| If S were stored in a mutable vector, and n were known, we could choose a random
   number from 0 to n-1, select the element in S at that index, remove it, then 
   repeat k times |#

(define (vector-remove v i)
  (vector-append (vector-take v i) ; subvector on [0, i)
                 (vector-drop v (add1 i)))) ; subvector on [i+1, length)

;;; (random-subset S k) -> list?
;;;   S : vector?
;;;   k : integer?
;;; Returns a random k-subset of S with distinct elements.
;;; Time Complexity: O(k)
(define (random-subset S k)
  (define n (vector-length S))
  (for/list ([in-range k]
             [i (in-range n 0 -1)])
    (define rand (random i))
    (define choice (vector-ref S rand))
    (set! S (vector-remove S rand))
    choice))

#| If n were unknown, we could use our one pass over S to count its length,
   then just use random-subset. |#

;;; Time Complexity: O(n + k)
(define (random-subset-unknown-n S k)
   (define n (for/sum ([s S]) 1))
   (for/list ([in-range k]
              [i (in-range n 0 -1)])
    (define rand (random i))
    (define choice (vector-ref S rand))
    (set! S (vector-remove S rand))
    choice)) 