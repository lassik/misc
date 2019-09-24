;; Variable-length binary-coded decimal (non-negative integers only)
;;
;; Exploits the fact that 7 bits can store two decimal digits, i.e.
;; [0,100[. The high bit is set for all but the last byte.

(import (rnrs))

(define (write-bcd out val)
  (let loop ((val val))
    (let-values (((val below-hundred) (div-and-mod val 100)))
      (cond ((= 0 val)
             (put-u8 out below-hundred))
            (else
             (put-u8 out (bitwise-ior 128 below-hundred))
             (loop val))))))

(define (read-bcd in)
  (let loop ((val #f) (mul 1))
    (let ((byte (get-u8 in)))
      (cond ((eof-object? byte)
             val)
            (else
             (let ((below-hundred (bitwise-and byte 127)))
               (unless (< below-hundred 100)
                 (error #f "Bad BCD encoding"))
               (let ((val (+ (or val 0) (* mul below-hundred))))
                 (if (= 0 (bitwise-and byte 128)) val
                     (loop val (* mul 100))))))))))

(define tempfile "deleteme")

(define (writeln x)
  (write x)
  (newline))

(let ((old-val 12345678))
  (call-with-port (open-file-output-port tempfile (file-options no-fail))
                  (lambda (out) (write-bcd out old-val)))
  (let ((new-val (call-with-port (open-file-input-port tempfile) read-bcd)))
    (writeln (list old-val new-val (= old-val new-val)))))
