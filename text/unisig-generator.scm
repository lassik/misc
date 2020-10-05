(import (scheme base) (scheme char))

(define (safe-ascii-byte? byte)
  (let ((char (integer->char byte)))
    (or (char<=? #\0 char #\9)
        (char<=? #\A char #\Z)
        (char<=? #\a char #\z)
        (case char ((#\. #\/ #\- #\#) #t) (else #f)))))

(define (hex-byte hex-prefix byte hex-suffix)
  (string-append hex-prefix
                 (if (< byte 16) "0" "")
                 (string-downcase (number->string byte 16))
                 hex-suffix))

(define (bytes->lines bytes between byte->string)
  (define max-width 60)
  (let loop ((i 0) (lines '()) (line ""))
    (let ((blank? (string=? "" line)))
      (if (= i (bytevector-length bytes))
          (reverse (if blank? lines (cons line lines)))
          (let ((enc (byte->string (bytevector-u8-ref bytes i))))
            (if (< max-width (+ (string-length line)
                                (if blank? 0 (string-length between))
                                (string-length enc)))
                (loop (+ i 1) (cons line lines) enc)
                (loop (+ i 1) lines (string-append line
                                                   (if blank? "" between)
                                                   enc))))))))

(define (hexify bytes hex-prefix hex-suffix between)
  (bytes->lines bytes between
                (lambda (byte) (hex-byte hex-prefix byte hex-suffix))))

(define (ascify bytes hex-prefix hex-suffix between)
  (bytes->lines bytes between
                (lambda (byte)
                  (if (safe-ascii-byte? byte) (string (integer->char byte))
                      (hex-byte hex-prefix byte hex-suffix)))))

(define (linify lines
                first-line-prefix non-last-line-suffix
                non-first-line-prefix last-line-suffix)
  (let loop ((first? #t) (lines lines) (prior ""))
    (let ((line (car lines)) (last? (null? (cdr lines))))
      (let ((prior (string-append
                    prior
                    (if first? first-line-prefix non-first-line-prefix)
                    line
                    (if last? last-line-suffix non-last-line-suffix))))
        (if last? prior (loop #f (cdr lines) prior))))))

;;

(define unisig-magic (bytevector #xDC #xDC #x0D #x0A #x1A #x0A #x00))

(define (unisig-bytes sig)
  (let ((sig (string->utf8 sig)))
    (bytevector-append unisig-magic
                       (bytevector (bytevector-length sig))
                       sig)))

(define (unisig-head+body-bytes sig)
  (let ((sig (string->utf8 sig)))
    (values (bytevector-append unisig-magic
                               (bytevector (bytevector-length sig)))
            sig)))

;;

(define (common-lisp sig)
  (let ((bytes (unisig-bytes sig)))
    (string-append "(defconstant +unisig+\n"
                   "  (make-array\n"
                   "   " (number->string (bytevector-length bytes)) "\n"
                   "   :element-type '(unsigned-byte 8)\n"
                   "   :initial-contents\n"
                   (linify (hexify bytes "#x" "" " ")
                           "   '(" "\n"
                           "     " ")))\n"))))

(define (scheme-r6rs sig)
  (let ((bytes (unisig-bytes sig)))
    (string-append "(define unisig\n"
                   (linify (hexify bytes "#x" "" " ")
                           "  #vu8(" "\n"
                           "       " "))\n"))))

(define (scheme-r7rs sig)
  (let ((bytes (unisig-bytes sig)))
    (string-append "(define unisig\n"
                   (linify (hexify bytes "#x" "" " ")
                           "  #u8(" "\n"
                           "      " "))\n"))))

(define (cee sig)
  (let-values (((head body) (unisig-head+body-bytes sig)))
    (string-append "static const unsigned char unisig["
                   (number->string (+ (bytevector-length head)
                                      (bytevector-length body)))
                   "] =\n"
                   (linify (append (hexify head "\\x" "" "")
                                   (ascify body "\\x" "" ""))
                           "    \"" "\"\n"
                           "    \"" "\";\n"))))

(define (go sig)
  (let-values (((head body) (unisig-head+body-bytes sig)))
    (string-append "const unisig =\n"
                   (linify (append (hexify head "\\x" "" "")
                                   (ascify body "\\x" "" ""))
                           "\t\"" "\" +\n"
                           "\t\"" "\"\n"))))

(define (python-3 sig)
  (let-values (((head body) (unisig-head+body-bytes sig)))
    (string-append "UNISIG = \\\n"
                   (linify (append (hexify head "\\x" "" "")
                                   (ascify body "\\x" "" ""))
                           "    b\"" "\" \\\n"
                           "    b\"" "\"\n"))))

(define (ruby sig)
  (let-values (((head body) (unisig-head+body-bytes sig)))
    (string-append "UNISIG = \\\n"
                   (linify (append (hexify head "\\x" "" "")
                                   (ascify body "\\x" "" ""))
                           "  \"" "\" \\\n"
                           "  \"" "\" \\\n")
                   "  .force_encoding('binary')\n")))

;;

(let ((them  (list common-lisp scheme-r6rs scheme-r7rs cee go python-3 ruby))
      (sig "github.com/example/format#2020-04-01"))
  (set! sig (string-append sig sig))
  (let loop ((them them))
    (let* ((that (car them)) (them (cdr them)) (last? (null? them)))
      (write-string (that sig))
      (unless last? (newline) (loop them)))))
