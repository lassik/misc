(defclass buffer ()
  ((local-variables
    :reader buffer-local-variables
    :initform (make-hash-table))))

(defvar *default-variables* (make-hash-table))

(defvar *current-buffer* (make-instance 'buffer))

(defun get-buffer-or-error (buffer-or-name)
  (check-type buffer-or-name buffer)
  buffer-or-name)

(defmacro with-current-buffer (buffer-or-name &body body)
  `(let ((*current-buffer* (get-buffer-or-error ,buffer-or-name)))
     ,@body))

;;

(defun default-value (symbol)
  (values (gethash symbol *default-variables*)))

(defun set-default (symbol value)
  (setf (gethash symbol *default-variables*)
        value))

(defsetf default-value set-default)

(defmacro setq-default (variable value)
  `(set-default ',variable ,value))

;;

(defun local-value (symbol)
  (multiple-value-bind (local-value has-local-value?)
      (gethash symbol (buffer-local-variables *current-buffer*))
    (if has-local-value?
        local-value
        (default-value symbol))))

(defun set-local (symbol value)
  (setf (gethash symbol (buffer-local-variables *current-buffer*))
        value))

(defsetf local-value set-local)

(defmacro setq-local (variable value)
  `(set-local ',variable ,value))

;;

(defmacro defvar-local (var val &optional docstring)
  `(progn (set-default ',var ,val)
          (define-symbol-macro ,var (local-value ',var))))
