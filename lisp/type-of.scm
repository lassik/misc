(import (scheme base) (scheme write))

(cond-expand
 (r7rs
  (define type-of-alist-extra (list (cons bytevector? 'bytevector))))
 (else
  (define type-of-alist-extra '())))

(define type-of-alist
  (append type-of-alist-extra
          (list (cons boolean?    'boolean)
                (cons char?       'character)
                (cons eof-object? 'eof-object)
                (cons null?       'null)
                (cons number?     'number)
                (cons pair?       'pair)
                (cons port?       'port)
                (cons procedure?  'procedure)
                (cons string?     'string)
                (cons symbol?     'symbol)
                (cons vector?     'vector))))

(define (type-of obj)
  (let loop ((alist type-of-alist))
    (and (not (null? alist))
         (if ((caar alist) obj) (cdar alist) (loop (cdr alist))))))

(define (writeln x) (write x) (newline))

(writeln (type-of 123))
(writeln (type-of "abc"))
(writeln (type-of #\x))
(writeln (type-of (lambda (x) x)))
(writeln (type-of 'abc))
(writeln (type-of #(1 2 3)))
(writeln (type-of '(1 2 3)))
(writeln (type-of '()))
(writeln (type-of (eof-object)))
(writeln (type-of (current-input-port)))
(writeln (type-of (bytevector 1 2 3)))
