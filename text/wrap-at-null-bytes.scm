#!

(import (scheme base))

(define tab-width 8)
(define screen-width 40)

(define (string-index str char start end)
  (let loop ((i start))
    (cond ((>= i end) #f)
          ((char=? char (string-ref str i)) i)
          (else (loop (+ i 1))))))

(define (string-index-not str char-match? start end)
  (let loop ((i start))
    (cond ((>= i end) #f)
          ((not (char-match? (string-ref str i))) i)
          (else (loop (+ i 1))))))

(define (advance-column column str start end)
  (let loop ((i start) (column column))
    (if (>= i end)
        column
        (loop (+ i 1)
              (+ column
                 (if (char=? #\tab (string-ref str i))
                     (- tab-width (modulo column tab-width))
                     1))))))

(define (render str)
  (define (horz-whitespace? char)
    (or (char=? char #\space) (char=? char #\tab)))
  (let ((str-end (string-length str)))
    (let loop-lines ((line-start 0))
      (if (>= line-start str-end)
          (values)
          (let ((line-end
                 (let ((e (string-index str #\newline line-start str-end)))
                   (if e (+ e 1) str-end))))
            (let loop-chunks ((chunk-start line-start) (column 0))
              (if (>= chunk-start line-end)
                  (loop-lines line-end)
                  (let ((chunk-end (or (string-index str (integer->char 0)
                                                     chunk-start line-end)
                                       line-end)))
                    (let ((column-after-chunk
                           (advance-column column str chunk-start chunk-end)))
                      (cond ((< column-after-chunk screen-width)
                             (write-string
                              (string-copy str chunk-start chunk-end))
                             (if (= chunk-end line-end)
                                 (loop-lines line-end)
                                 (loop-chunks (+ chunk-end 1)
                                              column-after-chunk)))
                            (else
                             (let ((left-margin 0)  ; TODO
                                   (chunk-start
                                    (or (string-index-not str
                                                          horz-whitespace?
                                                          chunk-start
                                                          chunk-end)
                                        chunk-end)))
                               (newline)
                               (write-string
                                (string-copy str chunk-start chunk-end))
                               (loop-chunks
                                chunk-end
                                (advance-column
                                 left-margin
                                 str chunk-start chunk-end))))))))))))))

(render
 (string-append
  "In\x0; its\x0;\t\t\t first\x0; form,\x0; the\x0; mv\x0; utility\x0;"
  " renames\x0; the\x0; file\x0; named\x0; by\x0; the\x0; source\x0;"
  " operand\x0; to\x0; the\x0; destination\x0; path\x0; named\x0; by\x0;"
  " the\x0; target\x0; operand.\x0; This\x0; form is\x0; assumed\x0;"
  " when\x0; the\x0; last\x0; operand\x0; does\x0; not\x0; name\x0; an\x0;"
  " already\x0; existing\x0; directory."
  "\n\n"
  "In\x0; its\x0; second\x0; form,\x0; mv\x0; moves\x0; each\x0; file\x0;"
  " named\x0; by\x0; a\x0; source\x0; operand\x0; to\x0; a destination\x0;"
  " file\x0; in\x0; the\x0; existing\x0; directory\x0; named\x0; by\x0;"
  " the\x0; directory\x0; operand. The\x0; destination\x0; path\x0; for\x0;"
  " each\x0; operand\x0; is\x0; the\x0; pathname\x0; produced\x0; by the\x0;"
  " concatenation\x0; of\x0; the\x0; last\x0; operand,\x0; a\x0; slash,\x0;"
  " and\x0; the\x0; final\x0; pathname component\x0; of\x0; the\x0; named\x0;"
  " file."))
