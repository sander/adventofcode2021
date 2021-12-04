#lang racket

(define (bit-vector->number s)
  (define length (vector-length s))
  (for/fold ([total 0]) ([i length])
    (+ total (* (expt 2 i) (vector-ref s (- length i 1))))))

(define (occurrences sequence-of-bit-vectors)
  (define width (vector-length (sequence-ref sequence-of-bit-vectors 0)))
  (for/fold ([totals (make-vector width 0)])
            ([measurement sequence-of-bit-vectors])
    (vector-map + totals measurement)))

(define (frequencies sequence-of-bit-vectors)
  (define length (sequence-length sequence-of-bit-vectors))
  (vector-map (lambda (x) (/ x length)) (occurrences sequence-of-bit-vectors)))

(define (gamma-rate diagnostic-report)
  (bit-vector->number (vector-map round (frequencies diagnostic-report))))

(define (epsilon-rate diagnostic-report)
  (bit-vector->number (vector-map (lambda (x) (- 1 (round x)))
                                  (frequencies diagnostic-report))))

(define (power-consumption diagnostic-report)
  (* (gamma-rate diagnostic-report) (epsilon-rate diagnostic-report)))

(define (parse-report-line s)
  (define (parse c) (case c [(#\0) 0] [(#\1) 1] [else #f]))
  (define parsed (map parse (string->list s)))
  (if (member #f parsed) #f (list->vector parsed)))

(module+ test
  (require rackunit)

  (define example-diagnostic-report
    (list '#(0 0 1 0 0)
          '#(1 1 1 1 0)
          '#(1 0 1 1 0)
          '#(1 0 1 1 1)
          '#(1 0 1 0 1)
          '#(0 1 1 1 1)
          '#(0 0 1 1 1)
          '#(1 1 1 0 0)
          '#(1 0 0 0 0)
          '#(1 1 0 0 1)
          '#(0 0 0 1 0)
          '#(0 1 0 1 0)))

  (check-equal? (gamma-rate example-diagnostic-report) 22)
  (check-equal? (epsilon-rate example-diagnostic-report) 9)
  (check-equal? (power-consumption example-diagnostic-report) 198)

  (check-equal? (parse-report-line "00100") '#(0 0 1 0 0))
  (check-equal? (parse-report-line "00200") #f))

(define (power-consumption-in file)
  (define m (sequence-map parse-report-line (sequence->stream (in-lines file))))
  (define result (power-consumption m))
  (printf "~a is the power consumption of the submarine~n" result))

(call-with-input-file "03-input.txt" power-consumption-in)
