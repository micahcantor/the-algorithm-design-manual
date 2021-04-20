#lang racket/base
(require racket/list)
(require racket/match)
#| Generate all Ramanujan numbers where a, b, c, d < n |#

#| cubes: 1, 8, 27, 64 |#
#| pseudocode:
    generate all the 2-combinations up to length n
    filter these for pairs of combinations where all four elements are distinct 
    map to a list of pairs of pairs by cubing and summing each pair
    filter for pairs that are equal |#

(define (ramanujan n)
  (define combs (combinations (range n) 2)) ; '((1 2) (1 3) (2 3))
  (define pairs (combinations combs 2)) ; '(((0 1) (0 2)) ((0 1) (1 2)) ((0 2) (1 2)))
  (define distinct-pairs 
    (filter 
      (λ (pair)
        (match-let* ([(list c1 c2) pair]
                     [(list a b) c1]
                     [(list c d) c2])
          (not (check-duplicates (list a b c d)))))
      pairs))
   (define cubed/summed
     (map
       (λ (pair)
         (match-let* ([(list c1 c2) pair]
                     [(list a b) c1]
                     [(list c d) c2])
          (cons (+ (expt a 3) (expt b 3))
                (+ (expt c 3) (expt d 3))))) 
       distinct-pairs))
  (define equal-pairs
    (filter (λ (pair) (= (car pair) (cdr pair)))
            cubed/summed))
  (define solutions
    (map (λ (pair) (car pair))
         equal-pairs))
  solutions)

#| better implementation, with dictionaries |#

(define (better-ramanujan n)
  (define calculated (make-hash))
  (define solutions (make-hash))
  (for ([pair (in-combinations (range n) 2)])
    (define sum (+ (expt (car pair) 3) 
                   (expt (last pair) 3)))
    (when (hash-has-key? calculated sum)
          (hash-set! solutions sum pair))
    (hash-set! calculated sum pair))
  (hash-keys solutions))

#| Takeaway: When searching for pairs of elements, storing values calculated thus far
   in a dictionary or a vector can be extremely useful. In this case, we are searching
   for pairs of integer pairs (a,b) and (c,d). We calculate each sum and store it in our
   table. Then, all we have to do to check if we have found a pairing is see if the current
   sum has already been calculated.
   
   This is efficient since hash and vector lookups are constant operations.|#