(import (scheme base) (scheme write) (srfi 1))

(define (assert x) (unless x (error "Internal error")))

(define (writeln x) (write x) (newline))

(define (written x)
  (call-with-port (open-output-string)
                  (lambda (out) (write x out) (get-output-string out))))


(define-record-type <word>
  (make-word name types-in types-out)
  word?
  (name      word-name)
  (types-in  word-types-in)
  (types-out word-types-out))

(define dictionary '())

(define (lookup-word name)
  (or (find (lambda (word) (eq? name (word-name word))) dictionary)
      (error "Not defined:" name)))

(define-syntax define-primitive
  (syntax-rules ()
    ((_ name (types-in ...) -> (types-out ...))
     (set! dictionary (cons (make-word 'name
                                       '(types-in ...)
                                       '(types-out ...))
                            dictionary)))))

(define-primitive +      (number number)       -> (number))
(define-primitive -      (number number)       -> (number))
(define-primitive *      (number number)       -> (number))

(define-primitive number->string (number)        -> (string))
(define-primitive string-append  (string string) -> (string))

(define-primitive null?   ((list t))            -> (boolean))
(define-primitive append1 ((list t) t)          -> ((list t)))
(define-primitive append  ((list t) (list t))   -> ((list t)))

(define (parse-list-type type)
  (and (list? type)
       (= 2 (length type))
       (symbol? (cadr type))
       (cadr type)))

(define (match-simple-subtype type want-type t-type)
  ;;(writeln `(match-simple-subtype ,type ,want-type))
  (assert (symbol? type))
  (assert (symbol? want-type))
  (cond ((eq? want-type type)
         (values type t-type))
        ((and (eq? want-type 'number)
              (or (eq? type 'integer)
                  (eq? type 'real)))
         (values type t-type))
        ((eq? want-type 't)
         (if (or (eq? t-type 't)
                 (eq? t-type type))
             (values type type)
             (error "Type variable resolves to two different types:"
                    type t-type)))
        (else
         (error "Type mismatch:" type want-type))))

(define (specialize types t-type)
  ;;(writeln `(specialize ,types ,t-type))
  (map (lambda (type)
         (cond ((eq? 't type) t-type)
               ((symbol? type) type)
               ((parse-list-type type)
                (let ((type (parse-list-type type)))
                  `(list ,(cond ((eq? 't type) t-type)
                                ((symbol? type) type)
                                (else (error "Simple type expected" type))))))
               (else (error "What type is this?" want-type))))
       types))

(define (match-types types want-types)
  (assert (= (length types) (length want-types)))
  (let* ((t-type 't)
         (new-types
          (map (lambda (type want-type)
                 (cond ((symbol? want-type)
                        (let-values (((new-type new-t-type)
                                      (match-simple-subtype
                                       type
                                       want-type
                                       t-type)))
                          (set! t-type new-t-type)
                          new-type))
                       ((parse-list-type want-type)
                        (let-values (((new-elem-type new-t-type)
                                      (match-simple-subtype
                                       (parse-list-type type)
                                       (parse-list-type want-type)
                                       t-type)))
                          (set! t-type new-t-type)
                          `(list ,new-elem-type)))
                       (else
                        (error "What type is this?" want-type))))
               types want-types)))
    (values new-types t-type)))

(define (infer body)
  (let loop ((body body) (miss-types '()) (types '()))
    (if (null? body) (append miss-types '(->) types)
        (let ((part (car body))
              (body (cdr body)))
          (cond ((boolean? part)
                 (loop body miss-types (append types '(boolean))))
                ((string? part)
                 (loop body miss-types (append types '(string))))
                ((and (integer? part) (exact-integer? part))
                 (loop body miss-types (append types '(integer))))
                ((real? part)
                 (loop body miss-types (append types '(real))))
                ((symbol? part)
                 (let* ((word (lookup-word part))
                        (want-in (word-types-in word))
                        (got-out (word-types-out word))
                        (n-types (length types))
                        (n-in (length want-in))
                        (n-out (length got-out)))
                   (if (<= n-in n-types)
                       (let ((types-in (take-right types n-in))
                             (remaining-types (drop-right types n-in)))
                         (match-types types-in want-in)
                         (loop body
                               miss-types
                               (append remaining-types got-out)))
                       (let* ((types-in types)
                              (got-in (take-right want-in n-types))
                              (miss-in (drop-right want-in n-types)))
                         (let-values (((_ t-type)
                                       (match-types types-in got-in)))
                           (loop body
                                 (append (specialize miss-in t-type)
                                         miss-types)
                                 (specialize got-out t-type)))))))
                (else (error "What?")))))))

(define (example body)
  (define width 50)
  (let* ((body-s (written body))
         (type-s (written (infer body)))
         (gap (max 0 (- width (string-length body-s)))))
    (write-string body-s)
    (write-string (make-string gap #\space))
    (write-string type-s)
    (newline)))

(example '())
(example '(1))
(example '(1 2))
(example '(1 2 +))
(example '(1 2 + number->string "foo" string-append))
(example '("foo" append1))
