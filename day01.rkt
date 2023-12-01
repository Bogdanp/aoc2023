#lang racket/base

(define (get-decimal-digit s i)
  (define c (string-ref s i))
  (and (char-numeric? c)
       (- (char->integer c)
          (char->integer #\0))))

(define digit-rxs
  (for/list ([s (in-list '(zero one two three four five six seven eight nine))])
    (regexp (format "^~a" s))))

(define (get-spelled-out-digit s i)
  (or
   (for/first ([(rx n) (in-indexed (in-list digit-rxs))]
               #:when (regexp-match? rx s i))
     n)
   (get-decimal-digit s i)))

(define (calibration-value s [get-digit get-decimal-digit])
  (define-values (d0 d1)
    (for/fold ([d0 #f]
               [d1 #f])
              ([i (in-range 0 (string-length s))])
      (define digit (get-digit s i))
      (values (or d0 digit)
              (or digit d1))))
  (+ (* d0 10) d1))

(define part1
  (call-with-input-file "day01.txt"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (calibration-value line)))))

(module+ test
  (require rackunit)
  (check-equal? part1 56042))

(define part2
  (call-with-input-file "day01.txt"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (calibration-value line get-spelled-out-digit)))))

(module+ test
  (require rackunit)
  (check-equal? part2 55358))
