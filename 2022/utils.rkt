#lang racket

(provide read-in)

(define (read-in filename)
  (define (reading input)
    (let ((line (read-line input)))
      (cond ((eq? line eof) '())
            (else (cons line (reading input))))))
  (reading (open-input-file filename)))
