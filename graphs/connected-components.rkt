#lang racket/base
(require "bfs.rkt" racket/hash)

(define (connected-components g)
  (define vertices (hash-keys g))
  (define visited (make-hash (list (cons (car vertices) null))))
  (for/sum ([v vertices])
    (cond 
      [(hash-ref visited v #f)
        0]
      [else 
        (hash-union! visited (bfs g v) #:combine/key (λ (k v1 v2) v2))
        1])))

#| Testing |#
(define g (make-hash (list (cons 0 '(1))
                           (cons 1 '(2))
                           (cons 2 '(0))
                           (cons 3 '(4))
                           (cons 4 '(3)))))

(connected-components g)
