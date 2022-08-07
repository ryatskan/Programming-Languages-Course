#lang racket

(define (my_partition x lst)
  ;helper function, for every i in lst, if (x i) is as expected (true or false, depending on h value), add it to the output
  (define (helper x lst h)
        (define temp null)
    (for ([i lst])
      (if (eq? (x i) h)
          null
          (set! temp (cons i temp))))
    temp)
  (list (reverse (helper x lst #f)) (reverse (helper x lst #t))))

(equal? (my_partition positive? '(-7 -3 1 4 3 -3 -6 -5 0) ) '((1 4 3) (-7 -3 -3 -6 -5 0)))
(equal? (my_partition boolean? '(-2 -3 1 2 3 -3 #f #t) ) '((#f #t) (-2 -3 1 2 3 -3 )))
(equal? (my_partition negative? '(1 2 3)) '(() (1 2 3)))