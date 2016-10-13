(defun sepsym (sym)
  (let ((sym (symbol-name sym)))
    (let ((sep (position #\/ sym :from-end t)))
      (values (subseq sym 0 sep)
              (when sep (subseq sym (1+ sep)))))))

(defun names (sym)
  (multiple-value-bind (word suffix) (sepsym sym)
    (values (intern (if (equal "IES" suffix)
                        (concatenate 'string word "Y")
                        word)
                    (symbol-package sym))
            (intern (concatenate 'string word suffix)
                    (symbol-package sym)))))

(defmacro do-list (sym &body body)
  (multiple-value-bind (one many) (names sym)
    `(dolist (,one ,many) ,@body)))

(defun test ()
  (let ((persons '("Alice" "Bob" "Cecilia" "Dave")))
    (do-list person/s
      (print person)))
  (let ((currencies '("Dollar" "Euro" "Pound" "Yen")))
    (do-list currenc/ies
      (print currency)))
  (let ((frisbies '("Foo" "Bar" "Baz")))
    (do-list frisbie/s
      (print frisbie)))
  (let ((whiskeys '("Scotch" "American")))
    (do-list whiskey/s
      (print whiskey))))
