;; https://cisco.github.io/ChezScheme/release_notes/v9.5/release_notes.html
;; https://github.com/cisco/ChezScheme/tree/master/c -- look for Mf-* makefiles

(import (chezscheme))

(define arch-pairs
  '(("a6" . amd64)
    ("arm32" . arm32)
    ("i3" . i386)
    ("ppc32" . ppc32)))

(define os-pairs
  '(("fb" . freebsd)
    ("le" . linux)
    ("nb" . netbsd)
    ("nt" . windows)
    ("ob" . openbsd)
    ("osx" . macos)
    ("qnx" . qnx)
    ("s2" . solaris)))

(define (string-prefix? fix s)
  (and (>= (string-length s) (string-length fix))
       (string=? fix (substring s 0 (string-length fix)))))

(define (find-prefix s prefixes not-found)
  (cond  ((null? prefixes) (error #f not-found))
         ((string-prefix? (caar prefixes) s)
          (values (cdar prefixes)
                  (substring s (string-length (caar prefixes))
                             (string-length s))))
         (else (find-prefix s (cdr prefixes) not-found))))

(define (decode-chez-machine-type mt)
  (let ((mt (symbol->string mt)))
    (let ((threaded? (char=? #\t (string-ref mt 0))))
      (let ((mt (if threaded? (substring mt 1 (string-length mt)) mt)))
        (let-values (((arch mt)
                      (find-prefix mt arch-pairs "Architecture not recognized")))
          (let-values (((os mt)
                        (find-prefix mt os-pairs "OS not recognized")))
            (if (= 0 (string-length mt))
                (list threaded? arch os)
                (error #f "Junk at end of machine type"))))))))

(display (decode-chez-machine-type (machine-type)))
(newline)
