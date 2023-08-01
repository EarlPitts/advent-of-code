(define (group-by-n l n)
  (if (null? l)
    '()
    (cons (take l n) (group-by-n (drop l n) n))))

(define (parse-tables l)
  (group-by-n 
    (group-by-n l 5)
    5))
