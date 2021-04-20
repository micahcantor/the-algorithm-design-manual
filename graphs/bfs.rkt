#lang racket/base
(require data/queue)

(define (bfs g root)
  (define visited (make-hash (list (cons root null))))
  (define q (make-queue))
  (enqueue! q root)
  (while (not (queue-empty? q))
      (define vertex (dequeue! q))
      (for ([neighbor (hash-ref g vertex)])
        (unless (hash-ref visited neighbor #f)
          (hash-set! visited neighbor null)
          (enqueue! q neighbor))))
  visited)

(define-syntax-rule (while cond-expr body ...)
  (let loop ()
    (when cond-expr
      body ... (loop))))

(provide bfs)

#| Testing |#
(define g (make-hash (list (cons 0 '(1))
                           (cons 1 '(2))
                           (cons 2 '(0))
                           (cons 3 '(4))
                           (cons 4 '(3)))))

(bfs g 0)