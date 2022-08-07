#lang racket

(define (my_count_foldr x lst)
  (define (q llst num)
    (if (x llst)
        (+ num 1)
        num))
  (foldr q 0 lst))

(equal? (my_count_foldr list? '('(1 2 3) #t '() "false")) 2)
(equal? (my_count_foldr boolean? '('(1 2 3) #t '() "false" "true" #f #f)) 3)
(equal? (my_count_foldr positive? '(1 3 -7 9 -2)) 3)

(my_count_foldr list? '('(1 2 3) #t '() "false"))