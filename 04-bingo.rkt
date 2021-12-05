#lang racket

(define (parse-game s)
  (define s-split (string-split s "\n\n"))
  (values (map string->number (string-split (car s-split) ","))
          (map (lambda (s) (map string->number (string-split s #px"\\s+")))
               (cdr s-split))))

(define (draw n s)
  (cond [(list? s) (map (curry draw n) s)]
        [(eq? s n) 'marked]
        [else s]))

(define (winning? board)
  (define size (sqrt (length board)))
  (define (row-iter entries)
    (cond [(empty? entries) #f]
          [(empty? (filter-not (curry eq? 'marked) (take entries size))) #t]
          [else (row-iter (drop entries size))]))
  (define (col-iter entries [col size])
    (cond [(zero? col) #f]
          [(empty? (for/list ([i (range (- col 1) (* size size) size)]
                              #:when (not (eq? (sequence-ref entries i)
                                               'marked))) #t)) #t]
          [else (col-iter entries (- col 1))]))
  (or (row-iter board) (col-iter board)))

(define (first-winner-with-score numbers-to-draw boards)
  (for*/fold ([marked-boards boards]
              [winner #f]
              [score 0]
              #:result (values winner score))
             ([n numbers-to-draw]
              [(bs i) (in-indexed marked-boards)]
              #:break winner)
    (define result (draw n bs))
    (if (winning? result)
        (values '() i (* n (for/sum ([k result] #:when (number? k)) k)))
        (values (list-set marked-boards i result) #f 0))))

(module+ test
  (require rackunit)

  (define example-game "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

  (define-values (numbers-to-draw boards) (parse-game example-game))
  
  (check-true (winning? '(marked marked 1 2)))
  (check-true (winning? '(marked 1 marked 2)))

  (let-values ([(winner score) (first-winner-with-score numbers-to-draw boards)])
    (check-eq? winner 2)
    (check-eq? score 4512)))

(define (first-winner-with-score-in file)
  (define-values (numbers boards) (parse-game (port->string file)))
  (define-values (winner score) (first-winner-with-score numbers boards))
  (printf "~a is the final winning score if you choose board ~a~n" score winner))

(call-with-input-file "04-input.txt" first-winner-with-score-in)
