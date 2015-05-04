#lang racket

(define (db-access in)
  (define (read-info in)
    (string-split (read-line in) "="))
  (sql-connect (read-info in)
               (read-info in)
               (read-info in)))