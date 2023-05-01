(require "utils.rkt")

(define input-list (read-in "day02.txt"))

(define results
  '(("A X" . 4)
    ("A Y" . 8)
    ("A Z" . 3)
    ("B X" . 1)
    ("B Y" . 5)
    ("B Z" . 9)
    ("C X" . 7)
    ("C Y" . 2)
    ("C Z" . 6)))

(foldr + 0 (map (lambda (x) (dict-ref results x)) input-list))

(define results2
  '(("A X" . 3)
    ("A Y" . 4)
    ("A Z" . 8)
    ("B X" . 1)
    ("B Y" . 5)
    ("B Z" . 9)
    ("C X" . 2)
    ("C Y" . 6)
    ("C Z" . 7)))

(foldr + 0 (map (lambda (x) (dict-ref results2 x)) input-list))
