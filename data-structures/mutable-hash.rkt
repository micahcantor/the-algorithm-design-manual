#lang racket/base

(require math/number-theory)

#| An implentation of mutable hash maps with string keys in Racket |#

#| "Private" Functions |#

(struct hash (table [num #:auto])
  #:mutable
  #:auto-value 0)

; Returns the index for a given string key
(define (hash-func key size)
  (let ([initial 17] [mult 13])
    (modulo 
     (+ initial
        (for/sum ([char key]) (* mult (char->integer char))))
     size)))

; Resizes a hash table to avoid collisions.
(define (hash-resize! hash)
  (let* ([old-size (vector-length (hash-table hash))]
         [new-size (next-prime (* 2 old-size))]         
         [new-table (build-vector new-size (λ (n) null))])    
    (for* ([lst (hash-table hash)]           
           [pair lst]           
           #:unless (null? lst))       
      (let* ([idx (hash-func (car pair) new-size)]             
             [lookup (vector-ref new-table idx)])
        (vector-set! new-table idx (cons (cons (car pair) (cdr pair)) 
                                         lookup))))
    (set-hash-table! hash new-table)))

#| "Public" Functions |#

; Creates an empty hash table
(define (make-hash)
  (let ([size 3])
    (hash (build-vector size (λ (n) null)))))

; Returns the value associated with key in hash 
(define (hash-get hash key)
  (let* ([table (hash-table hash)]
         [idx (hash-func key (vector-length table))])
    (cdr (findf (λ (pair) (equal? key (car pair)))
                (vector-ref table idx)))))

; Sets the value in hash associated with key.
(define (hash-set! hash key value)
  (set-hash-num! hash (add1 (hash-num hash)))
  (let ([load-factor (/ (hash-num hash) (vector-length (hash-table hash)))])
    (when (> load-factor .8)
      (hash-resize! hash)))
  (let* ([table (hash-table hash)]
         [idx (hash-func key (vector-length table))]
         [lookup (vector-ref table idx)])
    (vector-set! table idx (cons (cons key value) lookup))))

#| Testing |#

(define my-table (make-hash))
(hash-table my-table) ; --> '#(() () ())
(vector-length (hash-table my-table)) ; --> 3 (initial size)
(hash-set! my-table "key1" "value1")
(hash-set! my-table "key2" "value2")
(hash-set! my-table "key3" "value3")
(hash-table my-table) ; --> '#(() (("key3" . "value3")) (("key2" . "value2")) (("key1" . "value1")) () () ())
(vector-length (hash-table my-table)) ; --> 7 (next prime after doubling 3)

(provide make-hash hash-get hash-set!)
