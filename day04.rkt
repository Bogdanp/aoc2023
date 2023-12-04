#lang racket/base

(require racket/hash
         racket/match
         racket/string)

(struct card (id winning have)
  #:transparent)

(define cards
  (call-with-input-file "day04-example.txt"
    (lambda (in)
      (for/vector ([line (in-lines in)])
        (match-define (regexp #rx"Card +([0-9]+): ([^|]+) \\| (.+)"
                              (list _ (app string->number id) winning-str have-str))
          line)
        (card
         id
         (map string->number (string-split winning-str))
         (map string->number (string-split have-str)))))))

(define (card-matches c)
  (for/sum ([n (in-list (card-have c))]
            #:when (memv n (card-winning c)))
    1))

(define (card-score c)
  (define matches
    (card-matches c))
  (cond
    [(zero? matches) 0]
    [else (expt 2 (sub1 matches))]))

(define (card-wins c)
  (match-define (card id _winning _have) c)
  (for/list ([i (in-range 0 (card-matches c))])
    (vector-ref cards (+ id i))))

(define part1
  (for/sum ([c (in-vector cards)])
    (card-score c)))

(module+ test
  (require rackunit)
  (check-equal? part1 21105))

(define (add-counts cs)
  (apply hash-union cs #:combine +))

(define card-counts
  (let ([memo (make-hasheqv)])
    (lambda (c)
      (hash-ref!
       memo (card-id c)
       (lambda ()
         (add-counts
          (list*
           (hasheqv (card-id c) 1)
           (map card-counts (card-wins c)))))))))

(define part2
  (apply + (hash-values
            (add-counts
             (for/list ([c (in-vector cards)])
               (card-counts c))))))

(module+ test
  (check-equal? part2 5329815))
