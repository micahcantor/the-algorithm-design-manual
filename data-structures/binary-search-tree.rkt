#lang racket/base
(require racket/bool)
(require rackunit)

#| An implementation of BSTs in Racket with some useful functions |#

(struct bst (val left right) #:mutable #:transparent)

(define (leaf val)
  (bst val null null))

(define (bst-find bst query less-than?)
  (cond [(null? (bst-val bst))
          #f]
        [(equal? (bst-val bst) query)
          bst]
        [(less-than? query (bst-val bst))
          (bst-find (bst-left bst) query less-than?)]
        [else
          (bst-find (bst-right bst) query less-than?)]))

(define (bst-min bst)
  (cond [(null? (bst-val bst))
          #f]
        [(null? (bst-left bst))
          (bst-val bst)]
        [else (bst-min (bst-left bst))]))

(define (bst-max bst)
  (cond [(null? (bst-val bst))
          #f]
        [(null? (bst-right bst))
          (bst-val bst)]
        [else (bst-max (bst-right bst))]))

(define (count-children bst)
  (cond [(and (null? (bst-left bst))
              (null? (bst-right bst)))
          0]
        [(xor (null? (bst-left bst))
              (null? (bst-right bst)))
          1]
        [else 
          2]))

(define (bst-insert! bst val less-than?)
  (cond [(null? (bst-val bst))
          #f]
        [(and (less-than? val (bst-val bst))
              (null? (bst-left bst)))
          (set-bst-left! bst val)]
        [(and (not (less-than? val (bst-val bst)))
              (null? (bst-right bst)))
          (set-bst-right! bst val)]
        [(less-than? val (bst-val bst))
          (bst-insert! (bst-left bst) val less-than?)]
        [else (bst-insert! (bst-right bst) val less-than?)]))


(define (bst-delete! bst val less-than?)
  (define (delete!-helper bst doomed side less-than?)
    (let ([set-bst-side! (if (eq? side 'left) set-bst-left! set-bst-right!)]
          [children (count-children doomed)])
      (cond [(= 0 children)
              (set-bst-side! bst null)]
            [(= 1 children)
              (let ([gc (if (not (null? (bst-left bst)))
                            (bst-left bst)
                            (bst-right bst))])
                (set-bst-side! bst gc))]
            [(= 2 children)
              (let ([successor (bst-min (bst-right doomed))])
                (bst-delete! bst successor less-than?)
                (set-bst-val! doomed successor))])))
  (let ([left (bst-left bst)] [right (bst-right bst)])
    (cond
      [(null? (bst-val bst))
        #f]
      [(and (not (null? left))
            (equal? (bst-val left) val))
        (delete!-helper bst left 'left less-than?)]
      [(and (not (null? right))
            (equal? (bst-val right) val))
        (delete!-helper bst right 'right less-than?)]
      [(less-than? val (bst-val bst))
        (bst-delete! left val less-than?)]
      [else 
        (bst-delete! right val less-than?)])))

#| Testing |#
(define mt (bst 2 (leaf 1) (bst 7 (bst 4 (leaf 3) (bst 6 (leaf 5) null)) (leaf 8))))
(bst-delete! mt 4 <)
(check-equal? mt
              (bst 2 (leaf 1) (bst 7 (bst 5 (leaf 3) (leaf 6)) (leaf 8)))
              "book pg 81 example 2 node deletion")
