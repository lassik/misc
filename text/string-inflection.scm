(import (scheme base) (scheme char) (scheme write))

(define (string-inflection-split string)
  (define (split-off run runs)
    (if (null? run) runs (cons (list->string (reverse run)) runs)))
  (let loop ((runs '()) (run '()) (chars (string->list string)))
    (if (null? chars) (reverse (split-off run runs))
        (let ((char (car chars)))
          (cond ((or (char=? #\- char) (char=? #\_ char))
                 (loop (split-off run runs) '() (cdr chars)))
                ((and (char-upper-case? char)
                      (not (null? run))
                      (not (char-upper-case? (car run))))
                 (loop (split-off run runs) (list char) (cdr chars)))
                ((and (not (char-upper-case? char))
                      (not (null? run))
                      (not (null? (cdr run)))
                      (char-upper-case? (car run))
                      (char-upper-case? (cadr run)))
                 (loop (split-off (cdr run) runs)
                       (list char (car run))
                       (cdr chars)))
                (else
                 (loop runs (cons char run) (cdr chars))))))))

(define (try s)
  (write `(string-inflection-split ,s))
  (display " => ")
  (write (string-inflection-split s))
  (newline))

(try "command_output_to_string")
(try "command-output-to_string")

(try "ASingleLine")
(try "PHPMode")
(try "EndsWithPHP")
(try "PHPAndXMLToo")
(try "phpAndXmlToo")
(try "PHPandXMLtoo")  ; This one does not have the expected split.
