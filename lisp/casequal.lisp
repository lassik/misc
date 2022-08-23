(defmacro casequal (key &body clauses)
  "Like CASE, but the key is tested with EQUAL instead of EQL."
  (let ((gkey (gensym)))
    `(let ((,gkey ,key))
       (cond ,@(mapcar (lambda (clause)
                         (destructuring-bind (head &body body) clause
                           (let ((head (if (listp head) head (list head))))
                             (if (or (member 't head) (member 'otherwise head))
                                 `(t ,@body)
                                 `((or ,@(mapcar (lambda (h) `(equal ,gkey ,h))
                                                 head))
                                   ,@body)))))
                       clauses)))))
