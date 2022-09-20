
(define-macro (def func args body)
    `(define ,(cons func args) ,body))


(define (map-stream f s)
    (if (null? s)
    	nil
    	(cons-stream (f (car s)) (map-stream f (cdr-stream s)))))

(define all-three-multiples
  (cons-stream 3 (map-stream (lambda (x) (+ 3 x)) all-three-multiples))
)


(define (compose-all funcs)
  (define (helper funcs result)
    (if (null? funcs)
      result
      (helper (cdr funcs) (lambda (x) ((car funcs) (result x))))
    )
  )
  (helper funcs (lambda (x) x))
)


(define (partial-sums stream)
  (define (helper start stream)
    (if (null? stream) ()
    (cons-stream (+ (car stream) start) (helper (+ (car stream) start) (cdr-stream stream)))
    )
  )
  (helper 0 stream)
)

