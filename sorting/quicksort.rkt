#lang racket/base
(require racket/match)

;;; Partition lst in two, elements that are less than the head
;;; and those that are not. Then recursively sort each side,
;;; appending the result together.
(define (quicksort lst <?)
  (match lst
    [(list) null]
    [(cons head tail)
     (define-values (left right)
       (partition (λ (n) (<? n head)) 
                  tail))
     (append (quicksort left <?)
             (list head)
             (quicksort right <?))]))

;;; Partition lst into elements which pass pred? 
;;; and elements that don't
(define (partition pred? lst)
  (let loop ([yes null] [no null] [lst lst])
    (match lst
      [(list) (values yes no)]
      [(cons head tail)
       (if (pred? head)
           (loop (cons head yes) no (cdr lst))
           (loop yes (cons head no) (cdr lst)))])))

