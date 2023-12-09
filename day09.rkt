#lang racket/base

(require racket/list
         racket/string)

(define (differences xs)
  (for/list ([x (in-list xs)]
             [y (in-list (cdr xs))])
    (- y x)))

(define (history xs)
  (let loop ([xs xs]
             [ds null])
    (if (andmap zero? xs)
        (cons xs ds)
        (loop
         (differences xs)
         (cons xs ds)))))

(define report
  (call-with-input-file "day09.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (map string->number (string-split line))))))

(define part1
  (for/sum ([xs (in-list report)])
    (for/sum ([ys (in-list (history xs))])
      (if (null? ys) 0 (last ys)))))

(module+ test
  (require rackunit)
  (check-equal? part1 1757008019))

(define part2
  (for/sum ([xs (in-list report)])
    (for/fold ([res 0])
              ([ys (in-list (history xs))])
      (- (if (null? ys) 0 (car ys)) res))))

(module+ test
  (check-equal? part2 995))
