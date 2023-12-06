#lang racket/base

(require racket/match
         racket/string)

(struct mapping (dst src len)
  #:transparent)

(define (mapping-map m v)
  (match-define (mapping dst src len) m)
  (and (>= v src)
       (<= v (+ src len))
       (+  v (- dst src))))

(define (parse-mapping str)
  (apply mapping (map string->number (string-split str))))

(define (read-map in)
  (void (read-line in))
  (void (read-line in))
  (let loop ([mappings null])
    (define c (peek-char in))
    (cond
      [(or (eof-object? c)
           (eqv? c #\newline))
       (reverse mappings)]
      [else
       (loop
        (cons
         (parse-mapping (read-line in))
         mappings))])))

(define-values (seeds maps)
  (call-with-input-file "day05.txt"
    (lambda (in)
      (define seeds
        (map
         string->number
         (string-split
          (substring (read-line in) 6))))
      (values
       seeds
       (for/list ([_ (in-range 7)])
         (read-map in))))))

(define (look-up mappings v)
  (or
   (for*/first ([m (in-list mappings)]
                [mapped-v (in-value (mapping-map m v))]
                #:when mapped-v)
     mapped-v)
   v))

(define (find-seed-location maps seed)
  (for/fold ([value seed])
            ([mappings (in-list maps)])
    (look-up mappings value)))

(define part1
  (for/fold ([res +inf.0])
            ([s (in-list seeds)])
    (define loc
      (find-seed-location maps s))
    (if (< loc res) loc res)))

(module+ test
  (require rackunit)
  (check-equal? part1 510109797))

(define (mapping-split-range m r)
  (match-define (mapping _dst src len) m)
  (match-define (cons lo hi) r)
  (define src-lo src)
  (define src-hi (+ src len))
  (cond
    [(and (< lo src-lo)
          (< hi src-lo))
     (list r)]
    [(and (> lo src-hi)
          (> hi src-hi))
     (list r)]
    [(and (< lo src-lo)
          (> hi src-hi))
     (list
      (cons lo (sub1 src-lo))
      (cons src-lo src-hi)
      (cons (add1 src-hi) hi))]
    [(< lo src-lo)
     (list
      (cons lo (sub1 src-lo))
      (cons src-lo hi))]
    [(> hi src-hi)
     (list
      (cons lo src-hi)
      (cons (add1 src-hi) hi))]
    [else
     (list
      (cons lo hi))]))

(define (mapping-map-range m r)
  (define m-lo (mapping-map m (car r)))
  (define m-hi (mapping-map m (cdr r)))
  (and m-lo m-hi (cons m-lo m-hi)))

(define ranges
  (let loop ([pairs null]
             [seeds seeds])
    (cond
      [(null? seeds)
       (reverse pairs)]
      [else
       (loop
        (cons
         (cons (car seeds)
               (+ (car seeds)
                  (cadr seeds)))
         pairs)
        (cddr seeds))])))

(define (map-ranges mappings ranges)
  ;; Split the ranges against all the mappings.
  (define split-ranges
    (let loop ([ranges ranges]
               [mappings mappings])
      (if (null? mappings)
          ranges
          (loop
           (apply append
                  (for/list ([r (in-list ranges)])
                    (mapping-split-range (car mappings) r)))
           (cdr mappings)))))
  ;; Then map the split ranges.
  (for/list ([r (in-list split-ranges)])
    (or
     (for*/first ([m (in-list mappings)]
                  [m-r (in-value (mapping-map-range m r))]
                  #:when m-r)
       m-r)
     r)))

(define (find-location-ranges maps ranges)
  (for/fold ([ranges ranges])
            ([mappings (in-list maps)])
    (map-ranges mappings ranges)))

(define part2
  (for*/fold ([res +inf.0])
             ([r (in-list (find-location-ranges maps ranges))])
    (define loc (car r))
    (if (< loc res) loc res)))

(module+ test
  (check-equal? part2 9622622))
