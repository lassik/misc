;; Implement the inequality operator from Common Lisp.

;; The value of /= is true if no two numbers are the same in value;
;; otherwise it is false.

(define /=
  (lambda args
    (if (null? args)
        (error "Too few arguments given to /=")
        (let outer ((args args))
          (let ((this (car args))
                (args (cdr args)))
            (or (null? args)
                (let inner ((tail args))
                  (and (not (= this (car tail)))
                       (if (null? (cdr tail))
                           (outer args)
                           (inner (cdr tail)))))))))))
