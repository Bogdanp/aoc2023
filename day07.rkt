#lang racket/base

(require racket/list
         racket/match
         racket/port
         racket/vector)

(define hands
  (call-with-input-file "day07.txt"
    (lambda (in)
      (for/vector ([line (in-lines in)])
        (match-define (regexp #rx"([^ ]+) (.+)"
                              (list _ hand (app string->number bid)))
          line)
        (cons hand bid)))))

(define (hand-counts h)
  (for/fold ([counts (hasheqv)])
            ([c (in-string h)])
    (hash-update counts c add1 0)))

(define (hand-type h)
  (define counts
    (sort
     (hash->list
      (hand-counts h))
     #:key cdr >))
  (match counts
    [`(,_) 'five-of-a-kind]
    [`((,_ . 4) ,_) 'four-of-a-kind]
    [`((,_ . 3) ,_) 'full-house]
    [`((,_ . 3) ,_ ,_) 'three-of-a-kind]
    [`((,_ . 2) (,_ . 2) ,_) 'two-pair]
    [`((,_ . 2) ,_ ,_ ,_) 'one-pair]
    [`(,_ ,_ ,_ ,_ ,_) 'high-card]))

(define (hand-type-numeric h)
  (case (hand-type h)
    [(five-of-a-kind) 6]
    [(four-of-a-kind) 5]
    [(full-house) 4]
    [(three-of-a-kind) 3]
    [(two-pair) 2]
    [(one-pair) 1]
    [(high-card) 0]))

(define (card-score c [j-score 11])
  (match c
    [#\2 2]
    [#\3 3]
    [#\4 4]
    [#\5 5]
    [#\6 6]
    [#\7 7]
    [#\8 8]
    [#\9 9]
    [#\T 10]
    [#\J j-score]
    [#\Q 12]
    [#\K 13]
    [#\A 14]))

(define (hand> a b
               #:hand-type-numeric-proc [hand-type-numeric hand-type-numeric]
               #:card-score-proc [card-score card-score])
  (define an (hand-type-numeric a))
  (define bn (hand-type-numeric b))
  (if (= an bn)
      (for/fold ([ok? #t])
                ([ca (in-string a)]
                 [cb (in-string b)])
        (define cas (card-score ca))
        (define cbs (card-score cb))
        #:break (or (> cas cbs)
                    (not ok?))
        (and ok? (= cas cbs)))
      (> an bn)))

(define (compute-winnings hands [hand> hand>])
  (let ([hands (vector-copy hands)])
    (vector-sort! hands hand> #:key car)
    (for/sum ([(h idx) (in-indexed (in-vector hands))])
      (define rank (- (vector-length hands) idx))
      (* (cdr h) rank))))

(define part1
  (compute-winnings hands))

(module+ test
  (require rackunit)
  (check-equal? part1 252295678))

(define (find-strongest h)
  (define counts
    (hand-counts h))
  (cond
    [(hash-has-key? counts #\J)
     (define non-jokers
       (remv #\J (hash-keys counts)))
     (define combinations
       (remove-duplicates
        (map (λ (cards) (sort cards char>?))
             (apply cartesian-product
                    (for/list ([_ (in-range (hash-ref counts #\J))])
                      non-jokers)))))
     (for/fold ([res #f] #:result (or res h))
               ([combination (in-list combinations)])
       (define replacement-hand
         (call-with-output-string
          (lambda (out)
            (for/fold ([replacements combination])
                      ([c (in-string h)])
              (cond
                [(char=? c #\J)
                 (display (car replacements) out)
                 (cdr replacements)]
                [else
                 (display c out)
                 replacements])))))
       (if (or (not res)
               (> (hand-type-numeric replacement-hand)
                  (hand-type-numeric res)))
           replacement-hand
           res))]
    [else h]))

(define part2
  (compute-winnings
   hands
   (lambda (a b)
     (hand>
      #:hand-type-numeric-proc (compose1 hand-type-numeric find-strongest)
      #:card-score-proc (λ (c) (card-score c 1))
      a b))))

(module+ test
  (check-equal? part2 250577259))
