#lang racket/base
(require "../data-structures/stack.rkt")

#| 
  Balancing Parens
  A largely imperative solution
|#

(define (balanced? s)
  (define parens (make-stack))
  (define flag #t)
  (for ([char s]
        #:when (or (char=? char #\() (char=? char #\))))
    (if (char=? char #\()
        (push parens 'left)
        (if (stack-empty? parens)
            (set! flag #f)
            (pop parens))))
  (and (stack-empty? parens)
       flag))

(balanced? "(())()()") ;; -> #t

#| Takeaway:
   Imperative programming isn't always bad for a problem that feels procedural in nature.
   ALso, we implement a stack in this solution, which is a nice way of keeping track of the
   balance of 'opposing forces' such as balanced parens. |#