
#lang racket
(define (insert_in_all_places lst x)
  (define output null)
  (for ([i lst])  
    (set! output (cons (cons x i ) output)))

  output)
     
(define (my_permutations lst)
  (define output null)
  (for ([i lst])
    (define per (my_permutations (remove i lst)))
    (set! output (append output (insert_in_all_places per i))))
  (cond
    [(= 1 (length lst)) (cons lst null)]
    [(= 0 (length lst)) '()]
  (else output)))