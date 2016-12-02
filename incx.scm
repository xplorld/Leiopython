
(define (zero? x)
  (= x 0))
(define (incx i)
  (let inc-int ((j i) (s 0))
  	(if (zero? j)
  	    s
  	    (inc-int (- j 1) (+ s j)))))
(incx 1000)