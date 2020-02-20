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
    (and (< i (string-length string))
         (let inner ((j 0))
           (or (and (= j (string-length substring)) i)
               (or (and (char=? (string-ref string (+ i j))
                                (string-ref substring j))
                        (inner (+ j 1)))
                   (outer (+ i 1))))))))

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
     (language-flags (r7rs)))
    (chicken
     (command-names "chicken-csi" "csi")
     (language-flags (r5rs "-script")
                     (r7rs "-R" "r7rs" "-script")))
    (gauche
     (command-names "gosh")
     (language-flags (r5rs)
                     (r7rs "-r" "7")))
    (kawa
     (command-names "kawa")
     (language-flags (r5rs "--r5rs")
                     (r6rs "--r6rs")
                     (r7rs "--r7rs")))
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
        (list->string (reverse chars))
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

(define (parse-declare-file filename)
  (with-input-from-file filename
    (lambda ()
      (let ((line (read-line)))
        (let loop ()
          (let ((form (lila-read)))
            (deb form)
            (cond ((eof-object? form) #f)
                  ((and (pair? form) (equal? 'declare-file (car form)))
                   (cdr form))
                  (else (loop)))))))))

;;

(define (find-implementation-command langs impls)
  (any (lambda (impl)
         (let ((flags (any (lambda (lang) (implementation-flags-for lang impl))
                           langs)))
           (and flags
                (let ((cname (implementation-first-available-command-name impl)))
                  (and cname (cons cname flags))))))
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

(let* ((filename (list-ref (command-line) 1))
       (declarations (parse-declare-file filename))
       (langs (assoc+ 'language declarations))
       (impls (assoc+ 'implementations declarations)))
  (deb langs)
  (deb impls)
  (let ((command (find-implementation-command langs impls)))
    (when command
      (let ((command (append command (list filename))))
        (deb command)
        (let ((process
               (open-process
                (list path: (car command)
                      arguments: (cdr command)
                      stdin-redirection: #f
                      stdout-redirection: #f
                      stderr-redirection: #f
                      show-console: #f))))
          (process-status process))))))
