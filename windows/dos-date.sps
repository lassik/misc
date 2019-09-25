(import (rnrs))

(define (displayln x)
  (display x)
  (newline))

(define (bits lo hi word)
  (let ((count (+ 1 (- hi lo))))
    (bitwise-and (bitwise-arithmetic-shift-right word lo)
                 (- (bitwise-arithmetic-shift-left 1 count) 1))))

(define (decode-dos-date word)
  (let ((d (bits 0 4 word))
        (m (bits 5 8 word))
        (y (bits 9 15 word)))
    (list (+ 1980 y) m d)))

(displayln (decode-dos-date #x4271))
