(import (scheme base) (scheme file) (gauche base) (rfc zlib))

(define (open-gzip-input-port filename)
  (define autodetect-gzip (+ 15 32))
  (open-inflating-port (open-binary-input-file filename)
                       :window-bits autodetect-gzip))

(let* ((gz-filename "emacs-22.1.tar.gz")
       (decompressed-filename (string-append gz-filename ".decompressed")))
  (call-with-port
   (open-gzip-input-port gz-filename)
   (lambda (in)
     (call-with-port
      (open-binary-output-file decompressed-filename)
      (lambda (out)
        (let ((buffer (make-bytevector (* 512 1024) 0)))
          (let loop ()
            (let ((nread (read-bytevector! buffer in)))
              (unless (eof-object? nread)
                (write-bytevector buffer out 0 nread)
                (loop))))))))))
