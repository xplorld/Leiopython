(define (zero? x)
  (= x 0))
(define (fac i)
  (if (zero? i) 1 (* (fac (- i 1)) i)))
(fac 100)