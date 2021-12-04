#lang racket

(define (position [horizontal 0] [depth 0] [aim 0])
  `((horizontal . ,horizontal) (depth . ,depth) (aim . ,aim)))

(define (position-multiplied position)
  (* (dict-ref position 'horizontal)
     [dict-ref position 'depth]))

(define (move command start)
  (define h (dict-ref start 'horizontal))
  (define d (dict-ref start 'depth))
  (define v (cadr command))
  (case (car command)
    [(forward) (dict-set start 'horizontal (+ h v))]
    [(down) (dict-set start 'depth (+ d v))]
    [(up) (dict-set start 'depth (- d v))]))

(define (parse-command string)
  (define match (regexp-match #rx"(forward|down|up) ([1-9][0-9]*)" string))
  (if (pair? match)
      (list (string->symbol (cadr match)) (string->number (caddr match)))
      #f))

(module+ test
  (require rackunit)

  (define example-input '([forward 5]
                          [down 5]
                          [forward 8]
                          [up 3]
                          [down 8]
                          [forward 2]))

  (define example-output (position 15 10))

  (check-equal? (foldl move (position) example-input) example-output)
  [check-equal? (position-multiplied example-output) 150]

  (check-equal? (parse-command "forward 10") '(forward 10))
  (check-equal? (parse-command "forward") #f))

(define (move-with-aim command position)
  (define h (dict-ref position 'horizontal))
  (define d (dict-ref position 'depth))
  (define a (dict-ref position 'aim))
  (define v (cadr command))
  (case (car command)
    [(forward) (dict-set* position 'horizontal (+ h v) 'depth (+ d (* a v)))]
    [(down) (dict-set position 'aim (+ a v))]
    [(up) (dict-set position 'aim (- a v))]))

(module+ test
  (define example-output-with-aim (position 15 60))

  (define actual-output-moved-with-aim (foldl move-with-aim (position) example-input))

  (define (check-equal-ref? d1 d2 k)
    (check-equal? (dict-ref d1 k) (dict-ref d2 k)))

  (check-equal-ref? actual-output-moved-with-aim example-output-with-aim 'horizontal)
  (check-equal-ref? actual-output-moved-with-aim example-output-with-aim 'depth)
  (check-equal? (position-multiplied example-output-with-aim) 900))

(define (follow-planned-course-in file)
  (define m (map parse-command (sequence->list (in-lines file))))
  (define result (position-multiplied (foldl move (position) m)))
  (define result-with-aim (position-multiplied (foldl move-with-aim (position) m)))
  (printf "~a is the final horizontal position multipled by the final depth~n" result)
  (printf "~a is the final horizontal position when moved with aim multipled by the final depth" result-with-aim))

(call-with-input-file "02-input.txt" follow-planned-course-in)
