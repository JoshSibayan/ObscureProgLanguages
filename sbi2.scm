#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket-5.1/bin/mzscheme -qr

;; $Id: sbi.scm,v 1.1 2012-01-19 17:13:35-08 - - $
;; ;; 
;; ;;  NAME
;; ;;     sbi.scm - silly basic interpreter
;; ;; 
;; ;;  SYNOPSIS
;; ;;     sbi.scm filename.sbir
;; ;; 
;; ;;  DESCRIPTION
;; ;;     The file mentioned in argv[1] is read and assumed to be an
;; ;;     SBIR program, which is the executed.  Currently it is only
;; ;;     printed.
;; ;; 

;; Enable standard error
(define *stderr* (current-error-port))

;; Search for basename of given file
(define *run-file*
	(let-values
		(((dirpath basepath root?)
			(split-path (find-system-path `run-file))))
		(path->string basepath)))

;; Print error messages to standard error
;; Output message and exit program with exit code 1
(define (die list)
	(for-each (lambda (item) (display item *stderr*)) list)
	(newline *stderr*)
	(exit 1))

;; Format error message
(define (usage-exit)
	(die `("Usage: " ,*run-file* " filename")))

;; Read input file
(define (readlist-from-inputfile filename)
	(let ((inputfile (open-input-file filename)))
		(if (not (input-port? inputfile))
			(die `(,*run-file* ": " ,filename ": open failed"))
			(let ((program (read inputfile)))
				(close-input-port inputfile)
					program))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

;; *function-table* is used to hold all of the functions, which include the operators
;; as well. This is initialized when the program begins using a for-each loop
;; containing a lambda.

;; *label-table* is used to hold addresses of each line, one level up from statements.
;; This is initialized by scanning the list returned by (read) when the
;; program begins.
(define *label-table* (make-hash))

;; *variable-table* which holds the value of all variables. This is initialized
;; witgh pi and e and is updated as needed during interpretation of the program.
;; Arrays are created with make-vector and updated with vector-set!.
(define *variable-table* (make-hash))

(define (show label item)
	(newline)
	(display label) (display " = ") (display item)
	(newline))

(define (eval-label program)
	(map (lambda (line)
		(when (and (not (null? line)) (>= length line) 2)
			   (not (null? (cdr line))) (symbol? (cadr line)))
		(hash-set! *label-table* (cadr line) (caddr line)))) program)


