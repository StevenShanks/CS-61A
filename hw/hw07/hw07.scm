(define (cddr s)
  (cdr (cdr s)))

(define (cadr s)
  (car (cdr s))
)

(define (caddr s)
  (car (cdr (cdr s)))
)


(define (sign num)
  (cond ((> num 0) 1) ((= num 0) 0) (else -1))
)


(define (square x) (* x x))

(define (pow x y)
  (if (= y 1)
    x
    (if (even? y)
      (square (pow x (/ y 2)))
      (* x (square (pow x (/ (- y 1) 2))))
    ) 
  )
)


(define (unique s)
  (define (f x)
      (not (eq? x (car s)))
  )
  (if (null? s)
    s
    (cons (car s) (unique (filter f (cdr s))))
  )
)


(define (replicate x n)
  (define (replicate-tail n ok)
    (if (zero? n)
      ok
      (replicate-tail (- n 1) (append ok `(,x)))
    )
  )
  (replicate-tail n '()) 
)


(define (accumulate combiner start n term)
  (if (zero? n)
    start
    (accumulate combiner (combiner start (term n)) (- n 1) term)
  )
)


(define (accumulate-tail combiner start n term)
  (if (zero? n)
    start
    (accumulate combiner (combiner start (term n)) (- n 1) term)
  )
)


(define-macro (list-of map-expr for var in lst if filter-expr)
  `(map (lambda (,var) ,map-expr) (filter (lambda (,var) ,filter-expr) ,lst))
)

