; First Part

(define mat '((1 2) (3 4)))

(define (transpose mat)
  (if (null? (car mat))
    '()
    (cons (map car mat)
          (transpose (map cdr mat)))))

(define (matrix-map f mat)
  (append (row-map f mat)
          (col-map f mat)))

(define (row-map f mat)
  (map f mat))

(define (col-map f mat)
  (map f (transpose mat)))

(define (member? x l)
  (cond ((null? l) #f)
        ((eq? x (car l)) #t)
        (else (member? x (cdr l)))))

(define (check-nums nums l)
  (cond ((null? l) #t)
        ((member? (car l) nums) (check-nums nums (cdr l)))
        (else #f)))

(define (check-board nums board)
    (any (lambda (x) (eq? x #t)) ; I know this is ugly, but I couldn't reduce with or, as it's apparently a macto
         (matrix-map (lambda (l) (check-nums nums l)) board)))

(define (check-all nums boards)
  (map (lambda (board) (check-board nums board)) boards))

(define (make-boards data)
  (fiveify (fiveify data)))

(define (find-true l)
  (let loop ((index 0)
             (rem l))
    (cond ((null? rem) -1)
          ((car rem) index)
          (else (loop (1+ index) (cdr rem))))))

(define (fiveify l)
  (if (null? l)
    '()
    (cons (list-head l 5)
            (fiveify (list-tail l 5)))))

(define (find-bingo numbers boards)
  (let loop ((nums 5))
    (if (not (eq? (find-true (check-all (list-head numbers nums) boards)) -1))
      (calculate-board (list-ref boards (find-true (check-all (list-head numbers nums) boards))) (list-head numbers nums))
      (loop (1+ nums)))))

(define (flatten matrix)
  (reduce append '() matrix))

(define (calculate-board board numbers)
  (* (car (reverse numbers))
     (reduce + 0 (filter (lambda (x) (not (member? x numbers))) (flatten board)))))

(find-bingo numbers-raw (make-boards tables-raw))

; Second Part
; ------------------------
