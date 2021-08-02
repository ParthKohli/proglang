#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file


(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))


(define (list-nth-mod xs n)
  (cond
    [(< n 0) (error "list-nth-mod: negative number")]
    [(empty? xs) (error "list-nth-mod: empty list")]
    [#t (car (list-tail xs (remainder n (length xs))))]))


(define (stream-for-n-steps s n)
  (define se (s))
  (if (= n 0)
      null
      (cons (car se) (stream-for-n-steps (cdr se) (- n 1)))))


(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0) (- x) x)
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))


(define dan-then-dog
  (letrec ([next-string (lambda (s) (if (equal? s "dan.jpg") "dog.jpg" "dan.jpg"))]
           [f (lambda (x) (cons x (lambda () (f (next-string x)))))])
    (lambda () (f "dan.jpg"))))


(define (stream-add-zero s)
  (letrec ([f (lambda (s) (cons (cons 0 (car (s)))
                                (lambda () (f (cdr (s))))))])
    (lambda () (f s))))


(define (cycle-lists xs ys)
  (letrec ([generate-nth (lambda (n) (cons (list-nth-mod xs n) (list-nth-mod ys n)))]
           [f (lambda (n) (cons (generate-nth n) (lambda () (f (+ n 1)))))])
    (lambda () (f 0)))) 


(define (vector-assoc v vec)
  (define (search v vec i)
    (if (>= i (vector-length vec))
       #f
       (let ([ith-elem (vector-ref vec i)])
         (if (or (not (pair? ith-elem))
                  (not (equal? (car ith-elem) v)))
           (search v vec (+ 1 i))
           ith-elem))))
  (search v vec 0))


(define (cached-assoc xs n)
  (define cache (make-vector n #f))
  (define slot 0)
  (lambda (v)
    (define cache-hit (vector-assoc v cache))
    (if cache-hit
        cache-hit
        (let
            ([result (assoc v xs)])
        (if result
            (begin
              [vector-set! cache slot result]
              [set! slot (remainder (+ slot 1) n)]
              result)
            result)))))

