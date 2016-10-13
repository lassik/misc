(defmacro defpartial (name params &body body)
  (let ((args (gensym "ARGS")) (newargs (gensym "NEWARGS")) (fun (gensym "FUN")))
    `(defun ,name (&rest ,newargs)
      (let (,args)
        (labels ((,fun (&rest ,newargs)
                   (setf ,args (nconc ,args ,newargs))
                   (if (< (length ,args) ,(length params))
                       #',fun
                       (destructuring-bind ,params ,args
                         ,@body))))
          (apply #',fun ,newargs))))))

(defpartial sum (a b c d)
  (+ a b c d))
