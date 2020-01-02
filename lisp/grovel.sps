(import (chezscheme))

(define (displayln x) (display x) (newline))

(define transcoder (make-transcoder (utf-8-codec) (eol-style lf)))

(define (write-lines-to-file filename lines)
  (call-with-port
   (open-file-output-port filename (file-options no-fail) 'line transcoder)
   (lambda (port) (parameterize ((current-output-port port))
                    (for-each displayln lines)))))

(define (read-lines-from-command command)
  (let-values (((to-process from-process process-stderr process-id)
                (open-process-ports command 'line transcoder)))
    (let loop ((lines '()))
      (let ((line (get-line from-process)))
        (if (eof-object? line) (reverse lines) (loop (cons line lines)))))))

(define (include-header header)
  (string-append "#include <" header ">"))

(define (printf-sizeof type)
  (string-append "  printf(\"%zu\\n\", sizeof(" type "));"))

(define (grovel types headers)
  (write-lines-to-file
   "grovel.c"
   `(,@(map include-header headers)
     "#include <stdio.h>"
     ""
     "int main(void) {"
     ,@(map printf-sizeof types)
     "  return 0;"
     "}"))
  (system "cc -o grovel grovel.c")
  (map cons types (map string->number (read-lines-from-command "./grovel"))))

(pretty-print
 (grovel '("size_t" "time_t" "mode_t" "uid_t" "gid_t")
         '("sys/types.h" "time.h")))
