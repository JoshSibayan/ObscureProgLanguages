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
;; Evaluate list recursively
;; Referred to hashexample.scm example
(define (eval-expr expr)
	(cond ((string? expr) expr)
	      ((number? expr) expr)
	      ((hash-has-key? *symbol-table* expr)
	              (hash-ref *symbol-table* expr))
	      ((list? expr)
	              (if (hash-has-key? *symbol-table* (car expr)) 
		              (let ((head (hash-ref *symbol-table* (car expr))))
				      (cond ((number? head) head)
				      	    ((vector? head) (vector-ref head (cadr expr)))
			                    ((procedure? head)
				                    (apply head (map (lambda (x) (eval-expr x)) (cdr expr))))
				      (else (die "Unable to evaluate espression type"))))
			       (die (list (car expr) " not found in table\n"))))))

;; Create array
(define (arr expr)
	(set! expr (car expr))
	(let ((arr (make-vector (eval-expr (cadr expr)) (car expr))))
		(symbol-put! (car expr) (+ (eval-expr (cadr expr)) 1))))

;; Create variable
(define (var expr)
	(symbol-put! (car expr) (eval-expr (cadr expr))))

;; Evaluate when given print argument
(define (eval-print expr)
	(map (lambda (x) (display (eval-expr))) expr)
	(newline))

(define (input-count expr count)
	(if (null? expr) count (let ((input (read))) (if (eof-object? input) -1
		(begin (symbol-put! (car expr) input) 
		(set! count (+ 1 count)) (input-count (cdr expr) count))))))

(define length (lambda (x)
	(if (null? x) 0 (+ (length (cdr x)) 1))))

(define (eval-input expr)
	(symbol-put! 'eval-count 0)
	(if (null? (car expr)) 
		(symbol-put! 'eval-count - 1) (begin (symbol-put! 'eval-count (input-count expr 0)))))

;; No practical loops in scheme, run repeatedly through tail-recursion using accumulator
(define (eval-line program line-num)
	(when (> (length program) line-num)
		(let ((line (list-ref program line-num)))
			(cond ((= (length line) 3)
			              (set! line (cddr line)) (run-line (car line) program line-num))
			      ((and (= (length line) 2) (list? (cadr line)))
				      (set! line (cdr line)) (run-line (car line) program line-num))
			      (else (eval-line program (+ line-num 1)))))))

(define (run-line command program line-num)
	(when (not (hash-has-key? *function-table* (car command)))
		(die "~s invalid command" (car command)))
	(cond ((eq? (car command) 'goto) 
	              (eval-line program (hash-ref *label-table* (cdr command))))
	      ((eq? (car command) 'if)
	              (if (eval-expr (car (cdr command)))
		              (eval-line program (hash-ref *label-table* (cadr (cdr command))))
			      (eval-line program (+ line-num 1))))
	      ((eq? (car command) 'print)
	              (if (null? (cdr command))
		              (newline) (eval-print (cdr command))) 
			      (eval-line program (+ line-num 1)))))

(define (put-label program)
	(map (lambda (line)
		(when (not (null? line))
			(when (or (= 3 (length line)) 
			      (and (= 2 (length line)) 
                              (not (list? (cadr line)))))
			(hash-set! *label-table* (cadr line) (- (car line) 1))))) program))

;; Check validity of arguments passed, else return usage-exit
;; When argument valid, set sbprogfile equal to it
;; Set program equal to inputfile commands
(define (main arglist)
	(if (or (null? arglist) (not (null? (cdr arglist))))
		(usage-exit)
		(let* ((sbprogfile (car arglist))
		       (program (readlist-from-inputfile sbprogfile)))
		      (write-program-by-line sbprogfile program))))

(main (vector->list (current-command-line-arguments)))
