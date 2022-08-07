#lang racket

(define (my_count x lst)
  ; Helper recursive function, with a counter ('value') of previous checks
  (define (recursion value x lst)
    (if (x (car lst))
        (set! value (+ 1 value))
        null)
    (if (= 1 (length lst))
        value
        (recursion value x (cdr lst))))
  ;starting 'value' is zero
  (recursion 0 x lst))

(equal? (my_count list? '('(1 2 3) #t '() "false")) 2)
(equal? (my_count boolean? '('(1 2 3) #t '() "false" "true" #f #f)) 3)
(equal? (my_count positive? '(1 3 -7 9 -2)) 3)