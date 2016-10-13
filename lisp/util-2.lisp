(defun deb-aux (expr-form expr-value)
  (format *error-output* ";;;DEBUG: ~S => ~S" expr-form expr-value)
  expr-value)

(defmacro deb (expr)
  `(deb-aux ',expr ,expr))

#+clisp (shadow 'with-gensyms)

(defmacro with-gensyms (symbols &body body)
  `(let ,(mapcar (lambda (s) `(,s (gensym ,(symbol-name s))))
                 symbols)
     ,@body))

(defmacro do-map ((var list &key) &body body)
  `(mapcar (lambda (,var) ,@body)
           ,list))

(defmacro do-toplevel-forms ((var stream) &body body)
  (with-gensyms (%stream %eof)
    `(let ((,%stream ,stream) ,var)
       (loop (setq ,var (with-standard-io-syntax (read ,%stream nil ',%eof)))
             (when (eql ,var ',%eof) (return))
             (tagbody ,@body)))))

(defun list1 (x)
  (if (listp x) x (list x)))

(defun cons1 (x)
  (if (consp x) x (list x)))

(defun car1 (x)
  (if (listp x) (car x) x))

(defun cdr1 (x)
  (if (listp x) (cdr x) nil))

(defmacro with->sb ((symbol) &body body)
  (with-gensyms (%sb %string-likes)
    `(let ((,%sb (make-sb)))
       (macrolet ((,symbol (&rest ,%string-likes)
                    `(sb+= ,',%sb ,@%string-likes)))
         ,@body
         ,%sb))))

(defun << (format-control &rest format-args)
  (apply #'format nil format-control format-args))

(defun get-extension-start (pathstring)
  (let* ((slash (position #\/ pathstring :from-end t))
         (start (cond ((not slash) (if (> (length pathstring) 1) 1 nil))
                      ((< slash (- (length pathstring) 2)) (+ slash 2))
                      (t nil)))
         (dot   (when start (position #\. pathstring :from-end t :start start)))
         (valid (and dot
                     (or (= dot start)
                     (loop for i from start to (1- dot)
                           when (not (char= #\. (char pathstring i)))
                           do (return t))))))
    (if valid
        (values (if dot (1+ dot) nil)
                (or (not (null dot))
                    (> (length pathstring)
                       (if (null slash) 1 (+ slash 2)))))
        (values nil
                nil))))

(defun get-extension (pathstring)
  (let ((start (get-extension-start pathstring)))
    (if start (subseq pathstring start) nil)))

(defun substitute-extension (new-extension pathstring)
  (multiple-value-bind (start may-append-p) (get-extension-start pathstring)
    (let ((rest (if start (subseq pathstring 0 (1- start)) pathstring)))
      (if (null new-extension)
          rest
          (if may-append-p
              (concatenate 'string rest "." new-extension)
              (error "May not append file name extension to ~S" pathstring))))))

(defun check-extension (correct-extension pathstring)
  (unless (equal correct-extension (get-extension pathstring))
    (error "File name extension is not ~S: ~S" correct-extension pathstring)))

(defun shell-quote (string)
  (assert (stringp string))
  (with->sb (result)
    (loop for char across string
          unless (alphanumericp char) do (result #\\)
          do (result char))))

(defun shell-command (&rest strings)
  (let ((command (apply #'concatenate
                        'string
                        (loop for string in strings
                              collect string
                              collect " "))))
    (write-line command) ; xxx if verbose ...
    (or #+clisp
        (if (= 0 (ext:shell command))
            t
            (error "Shell command failed:~%~A" command))
        (error "shell-command not implemented for this Lisp."))))
