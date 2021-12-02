#lang racket

(define (increases m)
  (define increased
    (for/stream ([i m] [j (stream-rest m)] #:when (< i j)) 1))
  (stream-fold + 0 increased))

(define (count-larger-measurements-in file)
  (define m (stream-map string->number (sequence->stream (in-lines file))))
  (increases m))

(call-with-input-file "01-input.txt" count-larger-measurements-in)
