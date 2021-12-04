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

(define (rating type diagnostic-report)
  (define report-width (vector-length (sequence-ref diagnostic-report 0)))
  (bit-vector->number
   (sequence-ref
    (for/fold ([candidates (sequence->stream diagnostic-report)])
              ([index report-width]
               #:break (eq? (sequence-length candidates) 1))
      (define frequency (/ (for/sum ([v candidates]) (sequence-ref v index))
                           (sequence-length candidates)))
      (define comparator (case type [(oxygen-generator) >=] [(co2-scrubber) <]))
      (define bit-to-keep (if (comparator frequency 1/2) 1 0))
      (sequence-filter (lambda (entry)
                         (eq? (sequence-ref entry index) bit-to-keep))
                       candidates))
    0)))

(define (life-support-rating diagnostic-report)
  (for/product ([t '(oxygen-generator co2-scrubber)])
    (rating t diagnostic-report)))

(module+ test
  (check-equal? (rating 'oxygen-generator example-diagnostic-report) 23)
  (check-equal? (rating 'co2-scrubber example-diagnostic-report) 10)
  (check-equal? (life-support-rating example-diagnostic-report) 230))

(define (power-consumption-and-life-support-rating-in file)
  (define m (sequence-map parse-report-line (sequence->stream (in-lines file))))
  (define result1 (power-consumption m))
  (define result2 (life-support-rating m))
  (printf "~a is the power consumption of the submarine~n" result1)
  (printf "~a is the life support rating of the submarine~n" result2))

(call-with-input-file "03-input.txt"
  power-consumption-and-life-support-rating-in)