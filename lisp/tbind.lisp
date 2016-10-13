;;; tree bind (recursive serialized destructuring-bind)

(defun tpeel (template)
  "peel the topmost layer of subtrees off `template'."
  (let ((acc ()))
    (labels
        ((symbol-suffix-p (string symbol)
           (let ((name (symbol-name symbol)))
             (and (<= (length string) (length name))
                  (not (null (search string
                                     name
                                     :start2 (- (length name)
                                                (length string))))))))
         (remove-symbol-suffix (string symbol)
           (let ((name (symbol-name symbol)))
             (intern (subseq name 0 (max 0 (- (length name) (length string))))
                     (symbol-package symbol))))
         (rec (template)
           (cond ((atom template)
                  template)
                 ((and (symbolp (car template))
                       (symbol-suffix-p "=" (car template)))
                  (let ((pretty (remove-symbol-suffix "=" (car template))))
                    (when (or (null (cdr template))
                              (atom (cadr template)))
                      (error "tbind: ~a not followed by tree" (car template)))
                    (push (cons pretty (cadr template)) acc)
                    (cons pretty (rec (cddr template)))))
                 (t
                  (cons (rec (car template))
                        (rec (cdr template)))))))
      (let ((destr (rec template)))
        (values acc destr)))))

(defmacro tbind (template tree &body body &environment env)
  (multiple-value-bind (*tbind* destr) (tpeel template)
    (declare (special *tbind*))
    `(destructuring-bind ,destr ,tree
       ,@(mapcar #'ext:expand-form body))))  ; xxx: clisp specific

(defmacro dot (subtree-symbol &body body)
  (unless (boundp '*tbind*) (error "dot: outside tbind"))
  (let ((%item (gensym))
        (subtree-template
         (or (cdr (assoc subtree-symbol *tbind*))
             (error "dot: no such subtree in scope: ~a" subtree-symbol))))
    `(dolist (,%item ,subtree-symbol)
       (tbind ,subtree-template ,%item
         ,@body))))

;;; xxx: shouldn't be able to (return) from within the progn.
(defmacro tmap (subtree-symbol &body body)
  (let ((%list (gensym)))
    `(let ((,%list ()))
       (dot ,subtree-symbol
         (push (progn ,@body) ,%list))
       (nreverse ,%list))))

(defmacro tlambda (template &body body)
  (let ((%tree (gensym)))
    `(lambda (,%tree)
       (tbind ,template ,%tree
         ,@body))))
