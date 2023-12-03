#lang racket/base

(define table
  (call-with-input-file "day03.txt"
    (lambda (in)
      (for*/vector ([line (in-lines in)]
                    [char (in-string line)])
        char))))
(define s (sqrt (vector-length table)))
(define -s (- s))

(define (engine-symbol? c)
  (and (not (eqv? c #\.))
       (not (char-numeric? c))))

(define-syntax-rule (define-finder id for*-id)
  (define (id t pos [ok? engine-symbol?])
    (for*-id ([d (in-list (list (- -s 1) -s (+ -s 1)
                                     -1           1
                                (-  s 1)  s (+  s 1)))]
              [idx (in-value (+ pos d))]
              #:when (and (>= idx 0)
                          (<  idx (vector-length t))
                          (ok? (vector-ref t idx))))
      idx)))

(define-finder find-adjacent for*/list)
(define-finder has-adjacent? for*/first)

(define (char->decimal c)
  (- (char->integer c)
     (char->integer #\0)))

(define part1
  (for/fold ([num 0]
             [ok? #f]
             [total 0]
             #:result (if ok? (+ num total) total))
            ([(c idx) (in-indexed (in-vector table))])
    (if (char-numeric? c)
        (values (+ (* num 10) (char->decimal c))
                (or ok? (has-adjacent? table idx))
                total)
        (values 0 #f (if ok? (+ num total) total)))))

(module+ test
  (require rackunit)
  (check-equal? part1 528799))

; invariant: indexes always start out as valid digit positions in the table
(define (get-numbers t is)
  (let loop ([is is]
             [nums null])
    (cond
      [(null? is) nums]
      [else
       (define-values (num rem-is)
         (let get-number ([i (car is)])
           (if (or (< i 0)
                   (not (char-numeric? (vector-ref t i))))
               (for/fold ([n 0] [rem-is is])
                         ([(c idx) (in-indexed (in-vector t (add1 i)))])
                 #:break (not (char-numeric? c))
                 (values
                  (+ (* n 10) (char->decimal c))
                  (remq (+ (add1 i) idx) rem-is)))
               (get-number (sub1 i)))))
       (loop rem-is (cons num nums))])))

(define part2
  (for/fold ([total 0])
            ([(c idx) (in-indexed (in-vector table))]
             #:when (eqv? c #\*))
    (define adjacent-numbers
      (get-numbers table (find-adjacent table idx char-numeric?)))
    (if (= (length adjacent-numbers) 2)
        (+ total (apply * adjacent-numbers))
        total)))

(module+ test
  (check-equal? part2 84907174))
