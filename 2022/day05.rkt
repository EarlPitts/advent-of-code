(define input "
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"
)

(define a "    [D]    ")

(define crates (string-split (car (string-split input "\n\n")) "\n"))
(define cmds (string-split (cadr (string-split input "\n\n")) "\n"))

(define (make-empty-stack) '())
(define (make-stack l) l)
(define (push i s) (cons i s))
(define (pop s)
  (let ((new-stack (cdr s))
        (i (car s)))
    (list i new-stack)))

(define (move num s1 s2)
  (cond ((eq? num 0) (list s1 s2))
        (else (move (- num 1)
                    (cadr (pop s1))
                    (push (car (pop s1)) s2)))))

(define (make-state) '())
(define (add-stack stack state)
  (cons stack state))
(define (step cmd state)
  (let* ((num (car cmd))
        (from (cadr cmd))
        (to (caddr cmd))
        (s1 (list-ref state from))
        (s2 (list-ref state to))
        (new-stacks (move num s1 s2)))
    (list-update (list-update state from (lambda (x) (car new-stacks))) to (lambda (x) (cadr new-stacks)))))

(step '(2 0 2) '((1 2 3) (4 5 6) (1 2 3)))

(define s (make-stack))
(define s2 (push #\A (push #\B s)))
(define s3 (push #\C (push #\D s)))

(cadr (pop s2))
(push 2 s2)
(cadr (push 2 s2))

(move 2 s2 s3)


(define (parse-crates input)

)

(define (parse-moves input)
)

