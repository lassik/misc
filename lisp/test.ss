;; Are parentheses balanced?

(define (balanced? s)
  (let ((n (string-length s)))
    (let loop ((i 0) (depth 0))
      (cond ((= i n)
             (= depth 0))
            ((char=? #\) (string-ref s i))
             (and (> depth 0) (loop (+ i 1) (- depth 1))))
            ((char=? #\( (string-ref s i))
             (loop (+ i 1) (+ depth 1)))
            (else
             (loop (+ i 1) depth))))))

(define (try s)
  (write s)
  (display " ")
  (display (if (balanced? s) "balanced" "unbalanced"))
  (newline))

(try "")
(try "()")
(try "(foo)")
(try "(")
(try ")")
(try ")(")
(try ")foo(")

(try "(foo (bar))")
(try "(foo (bar)")
