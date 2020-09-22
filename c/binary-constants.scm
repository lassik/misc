;; Compensate for the lack of 0b1010 binary literals in the C language
;; by defining equivalent BITS_1010 pre-processor constants.

(let loop ((last-bits 2) (n 2))
  (when (<= n #b111111)
    (let* ((hex (number->string n 16))
           (bin (number->string n 2))
           (bits (string-length bin)))
      (unless (= last-bits bits) (newline))
      (display "#define BITS_")
      (display bin)
      (display " 0x")
      (display hex)
      (newline)
      (loop bits (+ n 1)))))
