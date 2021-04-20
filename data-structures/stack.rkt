#lang racket/base

#| An implementation of the stack abstract data type in Racket |#
#| Operations: |#
;; push(s, v) [O(1)] : insert an element at the top of the stack
;; pop(q) [O(1)] : remove and return the element at the top of the stack

(struct stack (data) #:mutable)

(define (make-stack)
  (stack null))

(define (push stack value)
  (set-stack-data! stack (cons value (stack-data stack))))

(define (pop stack)
  (unless (null? (stack-data stack))
          (set-stack-data! stack (cdr (stack-data stack)))))

(define (stack-empty? stack)
  (null? (stack-data stack)))

(provide make-stack push pop stack-empty?)