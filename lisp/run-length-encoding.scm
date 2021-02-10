(import (scheme base) (scheme write))

(define (writeln x) (write x) (newline))

(define (map-cars f xs) (map (lambda (x) (cons (f (car x)) (cdr x))) xs))

(define (run-length-encoding s)
  (let loop ((i 0) (pairs '()))
    (if (= i (string-length s)) (reverse pairs)
        (let* ((last-pair  (and (not (null? pairs)) (car pairs)))
               (last-char  (and last-pair (car last-pair)))
               (last-count (and last-pair (cdr last-pair)))
               (this-char  (string-ref s i))
               (this-same? (eqv? last-char this-char))
               (this-count (if this-same? (+ last-count 1) 1))
               (this-pair  (cons this-char this-count)))
          (loop (+ i 1)
                (cons this-pair (if this-same? (cdr pairs) pairs)))))))

(writeln '(("a" . 4) ("b" . 3) ("c" . 2) ("a" . 1)))
(writeln (map-cars string (run-length-encoding "aaaabbbcca")))
