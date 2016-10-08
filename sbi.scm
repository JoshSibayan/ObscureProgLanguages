#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.1 2012-01-19 17:13:35-08 - - $
;; ;;
;; ;; NAME
;; ;;    sbi.scm - silly basic interpreter
;; ;;
;; ;; SYNOPSIS
;; ;;    sbi.scm filename.sbir
;; ;;
;; ;; DESCRIPTION
;; ;;    The file mentioned in argv[1] is read and assumed to be an
;; ;;    SBIR program, which is the executed.  Currently it is only
;; ;;    printed.
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
(define *function-table* (make-hash))

;; *label-table* is used to hold addresses of each line, one level up from statements.
;; This is initialized by scanning the list returned by (read) when the
;; program begins.
(define *label-table* (make-hash))

;; Referred to symbols.scm example
(define *symbol-table* (make-hash))
(define (symbol-put! key value)
	(hash-set! *symbol-table* key value))

(for-each
	(lambda (pair)
		(symbol-put! (car pair) (cadr pair)))
	`(

		(log10_2 0.301029995663981195213738894724493026768189881)
		(sqrt_2  1.414213562373095048801688724209698078569671875)
		(e	 2.718281828459045235360287471352662497757247093)
		(pi	 3.141592653589793238462643383279502884197169399)
		(div	 ,(lambda (x y) (floor (/ x y))))
		(log10	 ,(lambda (x) (/ (log x) (log 10.0))))
		(mod	 ,(lambda (x y) (- x (* (div x y) y))))
		(quot	 ,(lambda (x y) (truncate (/ x y))))
		(rem	 ,(lambda (x y) (- x (* (quot x y) y))))
		(<>	 ,(lambda (x y) (not (= x y))))
		(+	 ,+)
		(-	 ,-)
		(*	 ,*)
		(/	 ,/)
		(<=	 ,<=)
		(>=	 ,>=)
		(>	 ,>)
		(<	 ,<)
		(=	 ,=)
		(sin	 ,sin)
		(asin	 ,asin)
		(cos	 ,cos)
		(acos	 ,acos)
		(tan	 ,tan)
		(atan	 ,atan)
		(^	 ,expt)
		(ceil	 ,ceiling)
		(exp	 ,exp)
		(floor   ,floor)
		(log	 ,log)
		(sqrt	 ,sqrt)
		(abs	 ,abs)
		(round	 ,round)

	))

;; Note: REMEMBER NOT TO USE IMPERATIVE FUNCTIONS!

;; Determine category of object
(define (expr-eval expr)
	(cond ((string? expr) expr)
	      ((number? expr) expr)
	      ((hash-has-key? *symbol-table* expr)
	              (hash-ref *symbol-table* expr))
	      ((list? expr)
	              (if (hash-has-key? *symbol-table* (car expr))
		              (let ((head (hash-ref *symbol-table* (car expr))))
				      (cond ((number? head) head)
				      	    ((vector? head)
					            (vector-ref head (cadr expr)))
			                    ((procedure? head)
				                    (apply head (map (lambda (x) (expr-eval x)) (cdr expr))))
				      (else (die "Unable to evaluate espression type"))))
			       (die (list (car expr) " not found in table\n"))))))




(define (eval-print expr)
	(map (lambda (x) (display (expr-eval))) expr)
	(newline))

(main (vector->list (current-command-line-arguments)))
