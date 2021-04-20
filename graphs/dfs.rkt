#lang racket/base

(define (dfs g root)
  (define visited (make-hash))
  (let loop ([vtx root])
    (println visited)
    (hash-set! visited vtx null)
    (for ([neighbor (hash-ref g vtx)])
      (unless (hash-ref visited neighbor #f)
        (loop neighbor)))))

#| Testing |#
(define g (make-hash (list (cons 0 '(1 2))
                           (cons 1 '(2))
                           (cons 2 '(0 3))
                           (cons 3 '(2)))))

(dfs g 2)
