#lang racket/base

#| A fast divide and conquer algorithm for exponentiation |#

;;; let f(n) = a^n
;;; note: n = (floor n/2) + (floor n/2)
;;; for n even, a^n = (a^n)^2
;;; for n odd, a^n = a(a^(floor n/2))^2
;;; time complexity: O(log(n))

(define (power a n)
  (if (zero? n)
      1
      (let ([x (power a (floor (/ n 2)))])
        (if (even? n)
            (* x x)
            (* a x x)))))

#| Takeaway: Look to decompose a problem into distinct cases.
   Here we separated into separate cases for odd and even n.
   Then we determined the correct result to apply for each case. |#