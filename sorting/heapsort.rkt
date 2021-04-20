#lang racket/base

(require "../data-structures/heap.rkt")

(define (heapsort lst <?)
  (define heap (list->heap lst <?))
  (for/list ([l lst]) (heap-extract! heap <?)))

(define (vector-heapsort! vec <?)
  (define heap (vector->heap vec <?))
  (for ([i (in-range (vector-length vec))])
    (vector-set! vec i (heap-extract! heap <?))))