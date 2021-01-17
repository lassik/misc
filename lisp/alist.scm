(define (alist-update!/default alist key update default)
  (let loop ((seen? #f) (old alist) (new '()))
    (cond ((null? old)
           (reverse (if seen? new
                        (cons (cons key (update default))
                              new))))
          ((equal? key (caar old))
           (loop #t (cdr old)
                 (if seen? new
                     (cons (cons (caar old) (update (cdar old)))
                           new))))
          (else
           (loop seen? (cdr old) (cons (car old) new))))))
