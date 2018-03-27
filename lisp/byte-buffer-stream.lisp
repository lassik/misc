;;;; Trivial in-memory byte output stream for CLISP. Not optimized.

(defclass byte-buffer-stream (fundamental-binary-output-stream)
  ((buffer :initform (make-array 0 :fill-pointer 0 :adjustable t
                                   :element-type '(unsigned-byte 8)))))

(defmethod stream-write-byte ((stream byte-buffer-stream) byte)
  (vector-push-extend byte (slot-value stream 'buffer))
  byte)

(defmethod stream-write-byte-sequence ((stream byte-buffer-stream) byte-array
                                       &optional start end no-hang interactive)
  (declare (ignore start end no-hang interactive))
  (let ((buffer (slot-value stream 'buffer)))
    (loop for byte across byte-array do (vector-push-extend byte buffer))
    byte-array))

(defmacro with-output-to-byte-vector ((stream) &body body)
  `(let ((,stream (make-instance 'byte-buffer-stream)))
     (progn ,@body)
     (slot-value ,stream 'buffer)))
