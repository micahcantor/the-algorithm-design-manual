#lang racket/base

#| A basic implementation of linked lists in racket |#

(struct ll (head next))

(define (cons val lst)
  (ll val lst))

(define (car lst)
  (ll-head lst))

(define (cdr lst)
  (ll-next lst))

(define (member? lst x)
  (cond [(null? lst) #f]
        [(equal? x (car lst)) #t]
        [else (member? (cdr lst) x)]))
