(require srfi/1)
(require "utils.rkt")

(define bags (read-in "day03.txt"))

(define sum (curry foldr + 0))

(define (duplicate-item b1 b2)
  (if (member (car b1) b2)
    (car b1)
    (duplicate-item (cdr b1) b2)))

(define (get-item bag)
  (let* ((mid (/ (length bag) 2))
        (fst-half (take bag mid))
        (snd-half (drop bag mid)))
    (duplicate-item fst-half snd-half)))

#| I'm sure this can be done somehow with streams or sequences (I'm thinking about something like `zip [a..Z] [1..]` in haskell) |#
(define priorities
  (zip '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
         #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z)
        (range 1 53)))

(define (get-prio item)
  (dict-ref priorities item))

(sum (map (compose car get-prio get-item string->list) bags))

(define (group l)
  (if (null? l)
    '()
    (cons (take l 3) (group (drop l 3)))))

(define (get-badge bags)
  (let ((b1 (car bags))
        (b2 (cadr bags))
        (b3 (caddr bags)))
    (if (and (member (car b1) b2) (member (car b1) b3))
      (car b1)
      (get-badge (list (cdr b1) b2 b3)))))

(sum (map (compose car get-prio get-badge) (group (map string->list bags))))
