(import (scheme base) (srfi 151))

(define width 550)
(define height 400)

(define (bytevector-u16le-set! bv i value)
  (bytevector-u8-set!
   bv (+ i 0) (bitwise-and 255 value))
  (bytevector-u8-set!
   bv (+ i 1) (bitwise-and 255 (arithmetic-shift value -8))))

(define (get-pixels)
  (let ((pixels (make-bytevector (* width height 3) 0)))
    (let loopy ((i 0) (y 0))
      (if (= y height)
          pixels
          (let ((intensity (/ y height)))
            (let loopx ((i i) (x 0))
              (if (= x width)
                  (loopy i (+ y 1))
                  (begin
                    (bytevector-u8-set!
                     pixels (+ i 0) (truncate (* 255 intensity (/ y height))))
                    (bytevector-u8-set!
                     pixels (+ i 1) (truncate (* 255 intensity (/ x width))))
                    (bytevector-u8-set!
                     pixels (+ i 2) (truncate (* 255 intensity (/ y height))))
                    (loopx (+ i 3) (+ x 1))))))))))

(define (get-tga-header)
  (let ((tga (make-bytevector 18 0)))
    (bytevector-u8-set! tga 2 2)
    (bytevector-u16le-set! tga 12 width)
    (bytevector-u16le-set! tga 14 height)
    (bytevector-u8-set! tga 16 24)
    (bytevector-u8-set! tga 17 32)
    tga))

(define (main)
  (write-bytevector (get-tga-header))
  (write-bytevector (get-pixels)))

(main)
