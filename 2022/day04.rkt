(require "utils.rkt")

(define input (read-in "day04.txt"))

(define (make-range x y)
  (cons x y))
(define (range-fst r)
  (car r))
(define (range-snd r)
  (cdr r))

(define (range-contains r1 r2)
  (cond ((and (>= (range-fst r1) (range-fst r2)) (<= (range-snd r1) (range-snd r2))) #t)
        ((and (>= (range-fst r2) (range-fst r1)) (<= (range-snd r2) (range-snd r1))) #t)
        (else #f)))

(define (parse-line line)
  (let* ((ranges (string-split line ","))
        (r1 (string-split (car ranges) "-"))
        (r2 (string-split (cadr ranges) "-")))
    (list (make-range (string->number (car r1))
                      (string->number (cadr r1)))
          (make-range (string->number (car r2))
                      (string->number (cadr r2))))))

(count (curry eq? #t) (map (compose (curry apply range-contains) parse-line) input))

(define (range-overlap r1 r2)
  (cond ((and (<= (range-fst r1) (range-fst r2)) (>= (range-snd r1) (range-fst r2))) #t)
        ((and (<= (range-fst r2) (range-fst r1)) (>= (range-snd r2) (range-fst r1))) #t)
        (else #f)))

(count (curry eq? #t) (map (compose (curry apply range-overlap) parse-line) input))
(count (lambda (x) x) (map (compose (curry apply range-overlap) parse-line) input))
(count (compose not not) (map (compose (curry apply range-overlap) parse-line) input))
