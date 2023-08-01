(define (most-frequent column)
  (let ((count (length column)))
    (if (< (reduce + 0 (map digit-value column)) (/ count 2))
      0
      1)))

(define (rows->columns rows)
  (apply map list rows))

(define (flip numlist)
  (cond ((null? numlist) '())
        ((eq? 0 (car numlist)) (cons 1 (flip (cdr numlist))))
        ((eq? 1 (car numlist)) (cons 0 (flip (cdr numlist))))))

(define (most-frequent-list data)
  (map most-frequent  
       (rows->columns 
         (map 
           string->list 
           ((string-splitter) data)))))

(*
  (string->number (string* (most-frequent-list data)) 2)
  (string->number (string* (flip (most-frequent-list data))) 2))

;; Gets the most frequent number in a column (list of chars)
(define (most-frequent column)
  (let ((count (length column)))
    (cond ((< (reduce + 0 column) (/ count 2)) 0)
          ((> (reduce + 0 column) (/ count 2)) 1)
          ((= (reduce + 0 column) (/ count 2)) 1))))

(define (least-frequent column)
  (let ((count (length column)))
    (cond ((< (reduce + 0 column) (/ count 2)) 1)
          ((> (reduce + 0 column) (/ count 2)) 0)
          ((= (reduce + 0 column) (/ count 2)) 0))))

(define (rows->columns rows)
  (apply map list rows))

;; Gets the most frequent digit for given column number in the list of rows
(define (most-frequent-column rows column)
  (most-frequent (list-ref (rows->columns rows) column)))

(define (least-frequent-column rows column)
  (least-frequent (list-ref (rows->columns rows) column)))

;; Returns only the rows that have the given digit in the given column
(define (filter-by-digit rows digit column)
  (define (iter rows out)
    (cond ((null? rows) out)
          ((not (eq? (list-ref (car rows) column) digit))
           (iter (cdr rows) (cons (car rows) out)))
          (else (iter (cdr rows) out))))
(iter rows '()))

(string->number (list->string (map digit->char '(1 2 3))))

(define (solve data type)
  (define (iter rows column)
    (if (null? (cdr rows))
      (string->number (list->string (map digit->char (car rows))) 2)
      (iter
        (filter-by-digit
          rows
          (if (eq? type 'co2) 
            (least-frequent-column rows column)
            (most-frequent-column rows column))
          column)
        (+ column 1))))
  (iter (parse data) 0))

(*
  (solve data 'co2)
  (solve data 'oxygen))

;; Converts the data into rows
(define (parse data)
  (map (lambda (row) (map digit-value row))
       (map string->list ((string-splitter) data))))
