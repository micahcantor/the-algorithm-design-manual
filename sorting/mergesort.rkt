#lang racket/base
(require racket/list)

;;; Lists of length 0 and 1 are already sorted, by definition.
;;; For other lists, sort each half, then merge them back together
(define (mergesort items <=?)
  (cond
    [(or (null? items) (null? (cdr items)))
      items]
    [else 
      (define-values (left right)
        (split-at items (quotient (length items) 2)))
      (merge (mergesort left <=?) (mergesort right <=?) <=?)]))

;;; If either list is empty, return the other list.
;;; Construct a list with the smaller head of the two sides
;;; and merge the rest recursively.
(define (merge left right <=?)
  (cond 
    [(null? left) right]
    [(null? right) left]
    [(<=? (car left) (car right))
      (cons (car left) (merge (cdr left) right <=?))]
    [else
      (cons (car right) (merge left (cdr right) <=?))]))