#lang racket/base

(require racket/match
         racket/string)

(define-values (instrs network)
  (call-with-input-file "day08.txt"
    (lambda (in)
      (define instrs (list->vector (string->list (read-line in))))
      (void (read-line in))
      (define network
        (for/fold ([m (hash)])
                  ([line (in-lines in)])
          (match-define (regexp #rx"([^ ]+) = \\(([^,]+), (.+)\\)"
                                (list _ src l r))
            line)
          (hash-set m src (cons l r))))
      (values instrs network))))

(define (get-accessor instrs step)
  (define instr
    (vector-ref instrs (modulo step (vector-length instrs))))
  (case instr
    [(#\L) car]
    [(#\R) cdr]))

(define (find-solution instrs network start [done? (λ (k) (equal? k "ZZZ"))])
  (let loop ([step 0]
             [node start])
    (if (done? node)
        step
        (loop
         (add1 step)
         ((get-accessor instrs step)
          (hash-ref network node))))))

(define part1
  (find-solution instrs network "AAA"))

(module+ test
  (require rackunit)
  (check-equal? part1 13301))

(define (A? k) (string-suffix? k "A"))
(define (Z? k) (string-suffix? k "Z"))

(define part2
  (apply lcm (for/list ([node (in-list (hash-keys network))] #:when (A? node))
               (find-solution instrs network node Z?))))

(module+ test
  (check-equal? part2 7309459565207))
