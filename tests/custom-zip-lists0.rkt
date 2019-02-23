#lang racket

(require "../p3.rkt")



(if (equal? (zip-lists '(1) '(2 3 4) '(5)) '((1 2 5)))
    (exit 0)
    (exit 1))


