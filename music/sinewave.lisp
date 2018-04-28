(defparameter samples/sec 22050)
(defparameter bits/sample 8) ; unsigned samples
(defparameter nseconds 5)
(defparameter notehertz 440) ; note A_0
(defparameter volume 50) ; range 0..127 assuming 8 bits/sample

(define-symbol-macro bytes/sample (/ bits/sample 8))
(define-symbol-macro nsamples (* nseconds samples/sec))
(define-symbol-macro samples/notecycle (/ samples/sec notehertz))

(defvar output)

(defun text (s)
  (loop for c across s do (write-byte (char-code c) output))
  (values))

(defun u16l (x)
  (assert (<= 0 x (1- (expt 2 16))))
  (write-byte (logand 255 x) output) (setf x (ash x -8))
  (write-byte (logand 255 x) output) (setf x (ash x -8))
  (values))

(defun u32l (x)
  (assert (<= 0 x (1- (expt 2 32))))
  (write-byte (logand 255 x) output) (setf x (ash x -8))
  (write-byte (logand 255 x) output) (setf x (ash x -8))
  (write-byte (logand 255 x) output) (setf x (ash x -8))
  (write-byte (logand 255 x) output) (setf x (ash x -8))
  (values))

(let ((ndeb 0))
  (defun deb (x)
    (when (< ndeb samples/notecycle)
      (format t "~S~%" x)
      (incf ndeb 1))
    x))

(with-open-file (output "sinewave.wav" :direction :output
                 :if-exists :supersede :element-type '(unsigned-byte 8))
  (text "RIFF")
  (u32l (+ 36 (* nsamples bytes/sample)))
  (text "WAVE")
  (text "fmt ")
  (u32l 16)
  (u16l 1)
  (u16l 1)
  (u32l samples/sec)
  (u32l (* samples/sec bytes/sample))
  (u16l bytes/sample)
  (u16l bits/sample)
  (text "data")
  (u32l (* nsamples bytes/sample))
  (dotimes (samp nsamples)
    (let ((samp (mod samp samples/notecycle)))
      (write-byte (+ 128 (truncate (* (sin (+ (- pi) (* 2 pi samp (/ 1 samples/notecycle))))
                                      volume)))
                  output))))
