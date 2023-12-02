#lang racket/base

(require racket/match
         racket/string)

(struct game (id sets)
  #:transparent)

(define (parse-set set-str)
  (for/hasheq ([reveal-str (in-list (string-split set-str ","))])
    (match-define (regexp #rx"([0-9]+) ([a-z]+)"
                          (list _
                                (app string->number n)
                                (app string->symbol color)))
      reveal-str)
    (values color n)))

(define (parse-game line)
  (match-define (regexp #rx"Game ([^:]+): (.+)"
                        (list _ (app string->number id) sets-str))
    line)
  (define sets
    (map parse-set (string-split sets-str ";")))
  (game id sets))

(define (game-ok? g proc)
  (for/and ([s (in-list (game-sets g))])
    (proc s)))

(define (part1-ok? s)
  (and
   (<= (hash-ref s 'red 0) 12)
   (<= (hash-ref s 'green 0) 13)
   (<= (hash-ref s 'blue 0) 14)))

(define (game-minimums g)
  (for/fold ([minimums (hasheq 'red 0 'green 0 'blue 0)])
            ([s (in-list (game-sets g))])
    (for/fold ([minimums minimums])
              ([color (in-list '(red green blue))])
      (hash-update minimums color (λ (n) (max n (hash-ref s color 0)))))))

(define (game-power g)
  (apply * (hash-values (game-minimums g))))

(define part1
  (call-with-input-file "day02.txt"
    (lambda (in)
      (for*/sum ([line (in-lines in)]
                 [game (in-value (parse-game line))]
                 #:when (game-ok? game part1-ok?))
        (game-id game)))))

(module+ test
  (require rackunit)
  (check-equal? part1 2771))

(define part2
  (call-with-input-file "day02.txt"
    (lambda (in)
      (for*/sum ([line (in-lines in)]
                 [game (in-value (parse-game line))])
        (game-power game)))))

(module+ test
  (check-equal? part2 70924))
