#lang racket/base

#| Heap Struct |#

(struct heap (data n) #:mutable)

#| API |#

(define (heapsort lst <?)
  (define heap (list->heap lst <?))
  (for/list ([l lst]) (heap-extract! heap <?)))

(define (vector-heapsort! vec <?)
  (define heap (vector->heap vec <?))
  (for ([i (in-range (vector-length vec))])
    (vector-set! vec i (heap-extract! heap <?))))

(define (make-heap size)
  (heap (make-vector size) 0))

(define (list->heap lst <?)
  (define heap (make-heap (length lst)))
  (for-each (λ (e) (heap-insert! heap e <?))
            lst)
  heap)

(define (vector->heap vec <?)
  (define heap (make-heap (vector-length vec)))
  (for ([v vec])
    (heap-insert! heap v <?))
  heap)

(define (heap-insert! heap val <?)
  (vector-set! (heap-data heap) (heap-n heap) val)
  (bubble-up! heap (heap-n heap) <?)
  (set-heap-n! heap (add1 (heap-n heap))))

(define (heap-extract! heap <?)
  (define data (heap-data heap))
  (define n (sub1 (heap-n heap)))
  (define root (vector-ref data 0))
  (vector-set! data 0 (vector-ref data n))
  (bubble-down! heap 0 <?)
  (set-heap-n! heap (sub1 (heap-n heap)))
  root)

#| Helpers |#

(define (heap-parent p)
  (quotient (sub1 p) 2))

(define (heap-lc p)
  (add1 (* 2 p)))

(define (heap-rc p)
  (+ 2 (* 2 p)))

(define (heap-lesser? heap p k <?)
  (and (< p (heap-n heap))
       (<? (vector-ref (heap-data heap) p)
           (vector-ref (heap-data heap) k))))

(define (heap-swap! heap p k)
  (define data (heap-data heap))
  (define temp (vector-ref data p))
  (vector-set! data p (vector-ref data k))
  (vector-set! data k temp))

(define (bubble-up! heap p <?)
  (define parent (heap-parent p))
  (cond
    [(equal? parent #f) void]
    [(<? (vector-ref (heap-data heap) p)
         (vector-ref (heap-data heap) parent))
      (heap-swap! heap parent p)
      (bubble-up! heap parent <?)]))

(define (bubble-down! heap p <?)
  (define lc (heap-lc p))
  (define rc (heap-rc p))
  (define low p)
  (when (heap-lesser? heap lc low <?)
    (set! low lc))
  (when (heap-lesser? heap rc low <?)
    (set! low rc))
  (unless (= low p)
    (heap-swap! heap low p)
    (bubble-down! heap low <?)))

