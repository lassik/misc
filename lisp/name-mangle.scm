(import (scheme base) (scheme char) (scheme file) (scheme write))

;;; Utilities

(define (disp . xs) (for-each display xs) (newline))

(define (append-map f xs) (apply append (map f xs)))

(define (generator->list g)
  (let loop ((xs '()))
    (let ((x (g)))
      (if (eof-object? x) (reverse xs) (loop (cons x xs))))))

(define (read-file-lines file)
  (with-input-from-file file (lambda () (generator->list read-line))))

(define (string-join ss sep)
  (if (null? ss) ""
      (let loop ((so-far (car ss)) (ss (cdr ss)))
        (if (null? ss) so-far
            (loop (string-append so-far sep (car ss)) (cdr ss))))))

;;; Library

(define (split-lisp-identifier str)
  (define (char-alphanumeric? char)
    (or (char-alphabetic? char) (char-numeric? char)))
  (define (split-off run runs)
    (if (null? run) runs (cons (list->string (reverse run)) runs)))
  (let loop ((runs '()) (run '()) (chars (string->list str)))
    (if (null? chars) (reverse (split-off run runs))
        (let ((char (car chars)))
          (cond ((and (char=? #\- char)
                      (or (and (null? runs) (null? run))
                          (null? (cdr chars))))
                 (loop runs (cons char run) (cdr chars)))
                ((and (char=? #\- char)
                      (not (null? (cdr chars)))
                      (char=? #\> (cadr chars)))
                 (loop (cons "->" (split-off run runs)) '() (cddr chars)))
                ((char=? #\- char)
                 (loop (split-off run runs) '() (cdr chars)))
                ((or (char=? #\! char) (char=? #\? char))
                 (loop (cons (string char) (split-off run runs))
                       '() (cdr chars)))
                ((and (not (null? run))
                      (not (eqv? (char-alphanumeric? char)
                                 (char-alphanumeric? (car run)))))
                 (loop (split-off run runs) (list char) (cdr chars)))
                (else
                 (loop runs (cons char run) (cdr chars))))))))

(define special-cases
  '(

    ("*"    "star")
    ("+"    "plus")
    ("-"    "minus")
    ("/"    "slash")

    ("<"    "less")
    ("<="   "less" "than" "or" "equal")
    ("="    "equal")
    (">"    "greater")
    (">="   "greater" "than" "or" "equal")

    ("!"    "bang")
    ("?"    "p")

    ("->"   "to")
    ("=>"   "into")

    ("."    "dot")
    ("..."  "dot" "dot" "dot")

    ))

(define (expand-special-cases ss)
  (append-map (lambda (s)
                (let ((entry (assoc s special-cases)))
                  (if entry (cdr entry) (list s))))
              ss))

(define (name-mangle str)
  (string-join (expand-special-cases (split-lisp-identifier str))
               "_"))

;;; Main program

(for-each disp (map name-mangle (read-file-lines "name-mangle-r7rs.text")))
