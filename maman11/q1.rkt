#lang racket

    (define (flatten lst)
    (if (list? lst)
       (if (= 1 (length lst))
        (flatten (car lst))
        (append (flatten (car lst))
            (flatten (cdr lst))))
        (list lst)))
;flattens a given lst using sub-function flatten

(define (my_flatten lst)
  (if (= 0 (length lst))
      '()
      (flatten lst)))
  
(equal? (my_flatten '(1 2 3 (4 5 ( 6 7) (8 9 10 (11 12)) 13) 14))
        '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(equal? (my_flatten '()) '())
(equal? (my_flatten '(1 (1 (1 (1 (1 (1))))))) '(1 1 1 1 1 1))
