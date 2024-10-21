(import (scheme base)
        (scheme write)
        (only (srfi 1) iota))

(define (writeln x) (write x) (newline))

;; Not TABA
(define (convolve as bs)
  (if (or (null? as)
          (null? bs))
      '()
      (cons (cons (car as)
                  (car bs))
            (convolve (cdr as)
                      (cdr bs)))))

;; TABA (there and back again)
(define (taba-fold combine initial-state a-list b-list)
  (letrec ((rec (lambda (a-tail)
                  (if (null? a-tail)
                      (values initial-state
                              b-list)
                      (let-values (((state b-tail) (rec (cdr a-tail))))
                        (values (combine state
                                         (car a-tail)
                                         (car b-tail))
                                (cdr b-tail)))))))
    (let-values (((state b-tail) (rec a-list)))
      ;; Any leftover items in b-tail are silently ignored.
      state)))

(define (convolve-reverse a-list b-list)
  (taba-fold (lambda (result a b)
               (cons (cons a b)
                     result))
             '()
             a-list
             b-list))

(define (convolve-reverse-1 list)
  (convolve-reverse list list))

(define (palindrome? list)
  (taba-fold (lambda (result a b)
               (and result (equal? a b)))
             #t
             list
             list))

(for-each writeln (convolve (iota 10) (reverse (iota 10))))
(newline)
(for-each writeln (convolve-reverse (iota 10) (iota 10)))
(newline)
(for-each writeln (convolve-reverse-1 (iota 10)))
(newline)
(writeln (palindrome? '()))
(writeln (palindrome? '(1)))
(writeln (palindrome? '(2 1 2)))
(writeln (palindrome? '(2 1 1 2)))
(writeln (palindrome? '(1 2 3)))
