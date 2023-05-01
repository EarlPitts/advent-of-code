(require math/base)
(require "utils.rkt")

(define input-list (read-in "day01.txt"))

(define (dropf-exclude l p)
  (cond ((null? l) l)
        ((p (car l)) (cdr l))
        (else (dropf-exclude (cdr l) p))))

(define (group l)
  (cond ((null? l) l)
        (else (cons (takef l (lambda (line) (not (or (null? line) (string=? line "")))))
                    (group (dropf-exclude l (lambda (line) (or (null? line) (string=? line "")))))))))

(apply max (map sum (map (lambda (l) (map string->number l)) (group input-list))))

#| Part 2 |#

(sum (take (sort (map sum (map (lambda (l) (map string->number l))
                               (group input-list))) >) 3))
