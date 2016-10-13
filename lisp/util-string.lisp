(defun white-char-p (char)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (character char))
  (case char ((#\space #\tab #\newline #\return) t)))

(defun blank-line-p (line comment-starter)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (simple-base-string line)
           (simple-base-string comment-starter)
           (inline white-char-p))
  (let ((nonwhite (position-if-not #'white-char-p line)))
    (or (not nonwhite)
        (eql nonwhite (search comment-starter line :start2 nonwhite)))))

(defun split (elt seq &key (start 0))
  (let ((p1 (position elt seq :start start :test-not #'eql)))
    (when p1
      (let ((p2 (position elt seq :start p1)))
        (cons (subseq seq p1 p2)
              (when p2 (split elt seq :start p2)))))))

(defun split-if (predicate seq &key (start 0))
  (let ((p1 (position-if-not predicate seq :start start)))
    (when p1
      (let ((p2 (position-if predicate seq :start p1)))
        (cons (subseq seq p1 p2)
              (when p2 (split-if predicate seq :start p2)))))))

(defun prefix (pref seq)
  (search pref seq :end2 (length pref)))

(defun suffix (suff seq)
  (search suff seq :start2 (- (length seq) (length suff))))

(defun remove-suffix (suff seq)
  (if (suffix suff seq) ; or "while suffix"?
      (subseq seq 0 (- (length seq) (length suff)))
      seq))

(defmacro << (format-control &rest format-args)
  `(format nil ,format-control ,@format-args))

(defmacro s+ (&rest strings)
  `(concatenate 'string ,@strings))

(defmacro make-string-buffer ()
  `(make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
