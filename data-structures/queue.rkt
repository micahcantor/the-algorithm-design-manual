#lang racket/base

#| A queue is an abstract data structure that supports first-in, first-out (FIFO) data retrievial. |#

#| Operations: |#
;; Enqueue(q, v) [O(1)] : insert an element at the back of the queue
;; Dequeue(q) [O(1)] : return the element at the front of the queue

(struct queue (head tail) #:mutable)

(struct node (value [next #:mutable]))

(define (make-queue)
  (queue #f #f))

(define (queue-empty? q)
  (not (queue-head q)))

;;; Inserts an element at the back of the queue. If the queue is non-empty (i.e. has a head element)
;;; then set the `next` pointer of the tail of the list to the new node value. Otherwise, make the 
;;; queue head to be the new value. Then, set the tail of the list to the new node.
(define (enqueue! q v)
  (let ([n (node v #f)])
    (if (queue-head q)
        (set-node-next! (queue-tail q) n)
        (set-queue-head! q n))
    (set-queue-tail! q n)))

;;; Returns (and removes) the element at the front of the queue. If there is only one element in queue
;;; (i.e. the head equals the tail), then set both the head and the tail to the value #f, resetting it.
;;; Otherwise, set the head of the queue to the `next` pointer. Then return the value of the head.
(define (dequeue! q)
  (let ([old (queue-head q)])
    (unless old (error "cannot dequeue from empty queue"))
    (if (equal? old (queue-tail q))
        (begin (set-queue-head! q #f)
               (set-queue-tail! q #f))
        (set-queue-head! q (node-next old)))
    (node-value old)))

(provide make-queue queue-empty? enqueue! dequeue!)