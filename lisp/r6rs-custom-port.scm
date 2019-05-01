(define (make-hello-world-input-port)
  (let ((src (string->utf8 "hello world"))
        (src-start 0))
    (make-custom-binary-input-port
     "hello-world"
     (lambda (dst dst-start count)
       (if (>= src-start (bytevector-length src))
           0
           (let* ((remain (- (bytevector-length src) src-start))
                  (count (min remain count)))
             (begin (bytevector-copy!
                     src src-start
                     dst dst-start count)
                    (set! src-start (+ src-start count))
                    count))))
     #f #f #f)))

(get-bytevector-all (make-hello-world-input-port))
