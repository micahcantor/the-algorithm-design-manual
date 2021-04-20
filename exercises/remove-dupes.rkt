#lang racket/base
(require racket/match)

;;; (rem-dupes lst) -> list?
;;; Complexity: O(nlog(n))
(define (remove-dupes lst [<? <])
  (define sorted (sort lst <?))
  (let loop ([filtered null] [prev #f] [lst sorted])
    (match lst
      [(list) (reverse filtered)]
      [(cons head tail)
       (if (equal? head prev)
           (loop filtered head tail)
           (loop (cons head filtered) head tail))])))