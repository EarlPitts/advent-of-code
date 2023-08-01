(define (parse data)
  (if (eq? (string-length data) 0)
    '()
    (cons (cons
            (substring data 0 1)
            (substring data 1 2))
          (parse (substring data 4)))))

(parse data)

(define (move data position direction)
  (if (null? data)
    position
    (let ((new-dir )
          (amount (cdar)))
      (cond ((string=? "R" rotate)
             (move (cdr data)
                   (cons ()
                   ()))
            ((string=? "L" rotate)
             (move (cdr data)
                   ()
                   ()))))))
