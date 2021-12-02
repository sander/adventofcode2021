#lang racket

(define (larger-increases m)
  (define increased
    (for/stream ([i m] [j (stream-rest m)] #:when (< i j)) 1))
  (stream-fold + 0 increased))

(define (larger-sums m)
  (define sums
    (for/stream ([i m] [j (stream-rest m)] [k (stream-rest (stream-rest m))])
      (+ i j k)))
  (larger-increases sums))

(define (count-larger-measurements-in file)
  (define m (stream-map string->number (sequence->stream (in-lines file))))
  (printf  "~a measurements are larger than the previous measurement~n" (larger-increases m))
  (printf  "~a sums are larger than the previous sum~n" (larger-sums m)))

(call-with-input-file "01-input.txt" count-larger-measurements-in)
