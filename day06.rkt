#lang racket/base

(require racket/match
         racket/math
         racket/string)

(define (read-integers in start)
  (map string->number
       (string-split
        (substring (read-line in) start))))

(define races
  (call-with-input-file "day06.txt"
    (lambda (in)
      (define times (read-integers in (string-length "Time:")))
      (define distances (read-integers in (string-length "Distance:")))
      (map cons times distances))))

(define (win? r hold-time)
  (match-define (cons race-time distance) r)
  (define travel-time
    (- race-time hold-time))
  (> (* hold-time travel-time) distance))

(define (winning-hold-times r)
  (for/sum ([i (in-range (add1 (car r)))] #:when (win? r i))
    1))

(define part1
  (for/fold ([res 1])
            ([r (in-list races)])
    (* res (winning-hold-times r))))

(module+ test
  (require rackunit)
  (check-equal? part1 140220))

(define one-race
  (let ([m (λ (n) (expt 10 (exact-ceiling (log n 10))))])
    (for/fold ([t 0] [d 0] #:result (cons t d))
              ([r (in-list races)])
      (match-define (cons r-t r-d) r)
      (values (+ (* t (m r-t)) r-t)
              (+ (* d (m r-d)) r-d)))))

(define part2
  (winning-hold-times one-race))

(define (winning-hold-times* r)
  (match-define (cons t d) r)
  (define discriminant (sqrt (- (* t t) (* 4 d))))
  (define hi (exact-ceiling (/ (+ t discriminant) 2)))
  (define lo (exact-floor (/ (- t discriminant) 2)))
  (- hi lo 1))

(define part2*
  (winning-hold-times* one-race))

(module+ test
  (check-equal? part2 39570185)
  (check-equal? part2 part2*))
