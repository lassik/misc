#!

(define deb-enabled? #t)

(define-syntax deb
  (syntax-rules ()
    ((_ expr)
     (let ((val expr))
       (when deb-enabled?
         (let ((e (current-error-port)))
           (write 'expr e)
           (display " => " e)
           (write val e)
           (newline e)
           (flush-output-port e)))
       val))))

;;; List

(define (any fun xs)
  (let loop ((xs xs))
    (and (pair? xs) (or (fun (car xs)) (loop (cdr xs))))))

(define (find predicate xs)
  (let loop ((xs xs))
    (and (pair? xs) (if (predicate (car xs)) (car xs) (loop (cdr xs))))))

(define (append-map proc xs)
  (if (null? xs) '() (append (proc (car xs)) (append-map proc (cdr xs)))))

(define (assoc* key alist)
  (let ((pair (assoc key alist)))
    (if pair (cdr pair) '())))

(define (assoc+ key alist)
  (let ((vals (assoc* key alist)))
    (if (null? vals) (error "Not found:" key) vals)))

(define (assoc? key alist)
  (let ((pair (assoc key alist)))
    (and pair
         (or (and (pair? (cdr pair)) (null? (cddr pair)))
             (error "Bad form"))
         (cadr pair))))

(define (assoc1 key alist)
  (or (assoc? key alist) (error "Not found:" key)))

;;; String

(define (string-index-char string char)
  (let ((n (string-length string)))
    (let loop ((i 0))
      (cond ((= i n) #f)
            ((char=? char (string-ref string i)) i)
            (else (loop (+ i 1)))))))

(define (string-index-string string substring)
  (let outer ((i 0))
    (and (<= i (- (string-length string) (string-length substring)))
         (let inner ((j 0))
           (cond ((= j (string-length substring))
                  i)
                 ((char=? (string-ref string (+ i j))
                          (string-ref substring j))
                  (inner (+ j 1)))
                 (else
                  (outer (+ i 1))))))))

(define (string-prefix? fix string)
  (equal? 0 (string-index-string string fix)))

(define (string-split string delim-char)
  (let loop ((a 0) (b 0) (items '()))
    (cond ((= b (string-length string))
           (reverse (if (= a b)
                        items
                        (cons (substring string a b) items))))
          ((char=? delim-char (string-ref string b))
           (loop (+ b 1) (+ b 1) (if (= a b)
                                     items
                                     (cons (substring string a b) items))))
          (else (loop a (+ b 1) items)))))

;;; Port

(define (disp . xs)
  (unless (null? xs)
    (let loop ((x (car xs)) (xs (cdr xs)))
      (display x)
      (unless (null? xs) (display " ") (loop (car xs) (cdr xs))))
    (newline)))

;;; Path

(define (path . xs)
  (cond ((null? xs) "")
        ((null? (cdr xs)) (car xs))
        (else (string-append (car xs) "/" (apply path (cdr xs))))))

(define (current-path) (string-split (getenv "PATH" "") #\:))

(define (lenient-directory-files dirname)
  (if (file-exists? dirname) (directory-files dirname) '()))

(define (directory-contains-file? dirname basename)
  (not (not (member basename (lenient-directory-files dirname)))))

(define (path-search command-name)
  (find (lambda (dirname) (directory-contains-file? dirname command-name))
        (current-path)))

;;; Checksum

(define (fletcher-16->32 port)
  (define (clamp x) (+ (bitwise-and x 65535) (ash x -16)))
  (let loop ((a 0) (b 0))
    (let ((lo (read-u8 port)))
      (if (eof-object? lo)
          (bitwise-or (clamp a) (ash (clamp b) 16))
          (let ((hi (read-u8 port)))
            (set! hi (if (eof-object? hi) 0 hi))
            (set! a (+ a (bitwise-or lo (ash hi 8))))
            (loop (clamp a) (clamp (+ b a))))))))

;;

(define language-parents
  '((r5rs scheme)
    (r6rs scheme)
    (r7rs scheme)))

(define (language-match? user-language impl-language)
  (or (equal? user-language impl-language)
      (member user-language (assoc* impl-language language-parents))))

(define implementations
  '((ccl
     (command-names "ccl" "ccl64")
     (language-flags (common-lisp "--load")))
    (clisp
     (command-names "clisp")
     (language-flags (common-lisp)))
    (chez
     (command-names "chez-scheme" "chezscheme" "chez" "scheme")
     (language-flags (r6rs "--program")))
    (chibi
     (command-names "chibi-scheme")
     (language-flags (r7rs))
     (path-flag "-A"))
    (chicken
     (command-names "chicken-csi" "csi")
     (language-flags (r5rs "-script")
                     (r7rs "-R" "r7rs" "-script")))
    (cyclone
     (command-names "icyc")
     (language-flags (r7rs "-s")))
    (gambit
     (command-names "gsi-script" "gsi")
     (language-flags (scheme)
                     (r5rs "-:r5rs")
                     (r7rs "-:r7rs")))
    (gauche
     (command-names "gosh")
     (language-flags (r5rs)
                     (r7rs "-r" "7"))
     (path-flag "-A"))
    (gerbil
     (command-names "gxi")
     (language-flags (r7rs "--lang" "r7rs")))
    (guile
     (command-names "guile")
     (language-flags (r7rs "--r7rs")))
    (kawa
     (path-flag "-Dkawa.import.path=")
     (path-flag-join? #t)
     (path-flag-suffix "*.sld")
     (command-names "kawa")
     (language-flags (r5rs "--r5rs")
                     (r6rs "--r6rs")
                     (r7rs "--r7rs")))
    (larceny
     (command-names "larceny")
     (language-flags (r6rs "-r6rs")
                     (r7rs "-r7rs")))
    (sagittarius
     (command-names "sagittarius")
     (language-flags (r6rs "-r" "6")
                     (r7rs "-r" "7")))
    (sbcl
     (command-names "sbcl")
     (language-flags (common-lisp "--script")))))

(define (implementation-data name)
  (let ((pair (assoc name implementations)))
    (if pair (cdr pair) (error "Unknown implementation" name))))

(define (implementation-command-names name)
  (assoc* 'command-names (implementation-data name)))

(define (implementation-language-flags name)
  (assoc* 'language-flags (implementation-data name)))

(define (implementation-flags-for user-language name)
  (let loop ((alist (implementation-language-flags name)))
    (and (not (null? alist))
         (let* ((apair (car alist))
                (lang  (car apair))
                (flags (cdr apair)))
           (if (language-match? user-language lang) flags
               (loop (cdr alist)))))))

(define (implementation-first-available-command-name name)
  (let loop ((cnames (implementation-command-names name)))
    (and (not (null? cnames))
         (let* ((cname (car cnames))
                (dirname (path-search cname)))
           (if dirname (path dirname cname) (loop (cdr cnames)))))))

(define (implementation-scheme-path-flags name scheme-path)
  (if (null? scheme-path) '()
      (let ((flag (assoc? 'path-flag (implementation-data name))))
        (unless flag (error "Cannot change library path for" name))
        (let* ((suffix (or (assoc? 'path-flag-suffix
                                   (implementation-data name))
                           ""))
               (scheme-path (map (lambda (dir) (string-append dir suffix))
                                 (map path-normalize scheme-path))))
          (if (assoc? 'path-flag-join? (implementation-data name))
              (map (lambda (dir) (string-append flag dir)) scheme-path)
              (append-map (lambda (dir) (list flag dir)) scheme-path))))))

;;

(define (after substring string)
  (let ((start (string-index-string string substring)))
    (if start
        (+ start (string-length substring))
        (string-length string))))

(define (dotted-after magic-string output)
  (define version-chars
    (string-append "0123456789"
                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   "abcdefghijklmnopqrstuvwxyz"
                   "._"))
  (let ((a (after magic-string output)))
    (let loop ((b a))
      (if (and (< b (string-length output))
               (string-index-char version-chars (string-ref output b)))
          (loop (+ b 1))
          (substring output a b)))))

(define (outrun executable . args)
  (let* ((process  (open-process
                    (list path: executable
                          arguments: args
                          stdin-redirection: #f
                          stdout-redirection: #t
                          stderr-redirection: #t
                          show-console: #f)))
         (status   (process-status process))
         (output   (read-string-all process))
         (success? (= 0 status)))
    (and success? output)))

(define (dotted-aft output symbol output-prefix #!optional version-prefix)
  (let ((version-prefix (or version-prefix output-prefix)))
    (and (string-prefix? output-prefix output)
         (let ((version (dotted-after version-prefix output)))
           (and version (values symbol version))))))

(define (scan-version-flag executable)
  (let ((out (outrun executable "--version")))
    (and out (or (dotted-aft out 'sbcl   "SBCL ")
                 (dotted-aft out 'clisp  "GNU CLISP ")
                 (dotted-aft out 'guile  "guile (GNU Guile) ")
                 (dotted-aft out 'mit    "MIT/GNU Scheme" " Release ")))))

(define (scan-help-flag executable)
  (let ((out (outrun executable "--help")))
    (and out (or (dotted-aft out 'lumo "Lumo ")
                 (dotted-aft out 'planck "Planck ")
                 (and (or (string-index-string
                           out "is a driver program for the CHICKEN compiler")
                          (string-index-string
                           out "is the CHICKEN interpreter"))
                      (let ((out (outrun executable "-version")))
                        (let ((version (dotted-after "Version " out)))
                          (and version (values 'chicken version)))))
                 (and (or (string-index-string
                           out "The clojure script is a runner for Clojure. clj is"))
                      (outrun executable "-e" "(print (clojure-version))"))))))

(define (scan executable)
  (or (scan-version-flag executable)
      (scan-help-flag executable)))

;;

(define (skip-whitespace-and-comments)
  (let loop ()
    (let ((ch (peek-char)))
      (case ch
        ((#\space #\tab #\newline #\return) (read-char) (loop))
        ((#\;) (read-line) (loop))))))

(define (lila-symbol-char? ch)
  (and (not (eof-object? ch))
       (or (char-alphabetic? ch)
           (char-numeric? ch)
           (not (not (string-index-char "+-*/:." ch))))))

(define (lila-read-symbol)
  (let loop ((chars '()))
    (let ((ch (peek-char)))
      (if (not (lila-symbol-char? ch))
          (and (not (null? chars))
               (string->symbol (list->string (reverse chars))))
          (loop (cons (read-char) chars))))))

(define (lila-read-list end-char)
  (read-char)
  (let loop ((items '()))
    (skip-whitespace-and-comments)
    (if (equal? end-char (peek-char))
        (begin (read-char) (reverse items))
        (loop (cons (lila-read) items)))))

(define (lila-read-string end-char)
  (read-char)
  (let loop ((chars '()))
    (if (equal? end-char (peek-char))
        (begin (read-char) (list->string (reverse chars)))
        (begin (when (equal? #\\ (peek-char))
                 (read-char))
               (when (eof-object? (peek-char))
                 (error "eof in string"))
               (loop (cons (read-char) chars))))))

(define (lila-read)
  (skip-whitespace-and-comments)
  (let ((ch (peek-char)))
    (if (eof-object? ch) ch
        (case ch
          ((#\() (lila-read-list #\)))
          ((#\") (lila-read-string #\"))
          (else  (or (lila-read-symbol)
                     (begin (read-char)
                            (lila-read))))))))

(define (declare-file-body filename)
  (with-input-from-file filename
    (lambda ()
      (let ((line (read-line)))
        (let loop ()
          (let ((form (lila-read)))
            (deb form)
            (cond ((eof-object? form)
                   (error "No (declare-file ...) form near the start of"
                          filename))
                  ((and (pair? form) (equal? 'declare-file (car form)))
                   (cdr form))
                  (else (loop)))))))))

;;

(define (find-implementation-command langs impls scheme-path)
  (any (lambda (impl)
         (let ((flags (any (lambda (lang)
                             (implementation-flags-for lang impl))
                           langs)))
           (and flags
                (let ((cname (implementation-first-available-command-name
                              impl)))
                  (and cname (cons cname
                                   (append (implementation-scheme-path-flags
                                            impl scheme-path)
                                           flags)))))))
       impls))

(define (read-version)
  (let ((process
         (open-process
          (list path: command-name
                arguments: '()
                stdin-redirection: #f
                stdout-redirection: #t
                stderr-redirection: #t
                show-console: #f))))
    (disp (dotted-after "SBCL " (read-line process)))))

(define (read-string-all in)
  (let loop ((chars '()))
    (let ((char (read-char in)))
      (if (eof-object? char) (list->string (reverse chars))
          (loop (cons char chars))))))

(define (take-while match? xs)
  (let loop ((xs xs) (acc '()))
    (if (or (null? xs) (not (match? (car xs)))) (reverse acc)
        (loop (cdr xs) (cons (car xs) acc)))))

(define (parse-command-line)
  (define (starts-with-dash? s)
    (and (> (string-length s) 0) (char=? #\- (string-ref s 0))))
  (let* ((args (cdr (command-line)))
         (opts (take-while starts-with-dash? args)))
    (set! args (list-tail args (length opts)))
    (let ((modes '()))
      (for-each (lambda (opt)
                  (cond ((equal? opt "-V") (set! modes (cons 'version modes)))
                        ((equal? opt "-v") (set! modes (cons 'verbose modes)))
                        (else (error "Unknown command line option" opt))))
                opts)
      (unless (<= (length modes) 1)
        (error "More than one mode specified"))
      (let ((mode (if (null? modes) #f (car modes))))
        (values mode args)))))

(define (run-script script-file script-args verbose?)
  (set! deb-enabled? verbose?)
  (let* ((declarations (declare-file-body script-file))
         (langs (assoc+ 'language declarations))
         (impls (assoc+ 'implementations declarations))
         (scheme-path (assoc* 'path (or (assoc* 'scheme declarations)))))
    (deb langs)
    (deb impls)
    (deb scheme-path)
    (let ((command (find-implementation-command langs impls scheme-path)))
      (when command
        (let ((command (append command (list script-file))))
          (deb command)
          (let ((process
                 (open-process
                  (list path: (car command)
                        arguments: (cdr command)
                        stdin-redirection: #f
                        stdout-redirection: #f
                        stderr-redirection: #f
                        show-console: #f))))
            (process-status process)))))))

(define (version)
  (display "lila -- Lisp launcher\n")
  (newline)
  (for-each (lambda (x) (write x) (newline))
            '((command "lila")
              (release "0.0.0")
              (release.date "1970-01-01")
              (languages clojure common-lisp newlisp r5rs r6rs r7rs scheme)
              (scheme.srfi 22))))

(define (main)
  (receive (mode args) (parse-command-line)
    (cond ((equal? mode 'version)
           (version))
          (else
           (run-script (car args) (cdr args) (equal? mode 'verbose))))))

(main)
