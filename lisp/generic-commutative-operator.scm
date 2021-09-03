(import (scheme base) (scheme case-lambda) (scheme write))

(define (identity x) x)

(define (repeater make-empty append)
  (lambda (n part)
    (let loop ((n n) (whole (make-empty)))
      (if (<= n 0) whole (loop (- n 1) (append whole part))))))

(define (commutative-case type-a? type-b? operator next-case)
  (lambda (a b)
    (cond ((type-a? a)
           ((if (type-b? b) operator next-case) a b))
          ((type-b? a)
           ((if (type-a? b) operator next-case) b a))
          (else
           (next-case a b)))))

(define (commutative-case-no-match operator-name)
  (lambda (a b)
    (error "No match for" operator-name a b)))

(define (commutative-operator case-0 case-1 case-2)
  (case-lambda
    (()
     case-0)
    ((a)
     (case-1 a))
    ((a b)
     (case-2 a b))
    ((a b . rest)
     (let loop ((result (case-2 a b)) (rest rest))
       (if (null? rest) result
           (loop (case-2 result (car rest)) (cdr rest)))))))

(define dispatch*
  (commutative-case
   number? number? *
   (commutative-case
    integer? list? (repeater list append)
    (commutative-case
     integer? vector? (repeater vector vector-append)
     (commutative-case
      integer? string? (repeater string string-append)
      (commutative-case-no-match '*))))))

(define generic* (commutative-operator 1 identity dispatch*))

(define-syntax test
  (syntax-rules ()
    ((test x) (begin (write 'x) (display " => ") (write x) (newline)))))

(test (generic*))
(test (generic* 5))
(test (generic* 3 5))
(test (generic* "Ab" 3))
(test (generic* 3 "Ab"))
(test (generic* 3 "Ab" 3))
