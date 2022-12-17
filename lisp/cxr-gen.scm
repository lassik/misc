(import (scheme base) (scheme write))

(define (writeln x) (write x) (newline))

(define (accessor-letter accessor)
  (case accessor
    ((car) "a")
    ((cdr) "d")))

(define (cxr-procedure-name accessors)
  (let loop ((letters "") (accessors accessors))
    (if (null? accessors) (string->symbol (string-append "c" letters "r"))
        (loop (string-append letters (accessor-letter (car accessors)))
              (cdr accessors)))))

(define (cxr-procedure-definition accessors)
  `(define (,(cxr-procedure-name accessors) x)
     ,(let recurse ((accessors accessors))
        (if (null? accessors) 'x
            `(,(car accessors) ,(recurse (cdr accessors)))))))

(define (cxr-procedure-gen what depth)
  (let recurse ((depth depth) (accessors '()))
    (if (= 0 depth)
        (list (what (reverse accessors)))
        (append (recurse (- depth 1) (cons 'car accessors))
                (recurse (- depth 1) (cons 'cdr accessors))))))

(define (write-each what max-depth)
  (let loop ((depth 2))
    (when (<= depth max-depth)
      (for-each writeln (cxr-procedure-gen what depth))
      (newline)
      (loop (+ depth 1)))))

(write-each cxr-procedure-name 4)
(write-each cxr-procedure-definition 4)
