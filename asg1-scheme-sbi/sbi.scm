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

;; *variable-table* which holds the value of all variables. This is initialized
;; with pi and e and is updated as needed during interpretation of the program.
;; Arrays are created with make-vector and updated with vector-set!.
(define *variable-table* (make-hash))

;; Referred to symbols.scm example
;; Create and initialize variable-table
(define (var-put! key value)
	(hash-set! *variable-table* key value))

(for-each
	(lambda (pair)
		(var-put! (car pair) (cadr pair)))
	`(

		(log10_2 ,0.301029995663981195213738894724493026768189881)
		(sqrt_2  ,1.414213562373095048801688724209698078569671875)
		(e	 ,2.718281828459045235360287471352662497757247093)
		(pi	 ,3.141592653589793238462643383279502884197169399)
		(div	 ,(lambda (x y) (floor (/ x y))))
		(log10	 ,(lambda (x)   (/ (log x) (log 10.0))))
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

;; Length helper function
(define length (lambda (x)
	(if (null? x) 0 (+ (length (cdr x)) 1))))

;; Put labels into hash table
;; No need to tokenize input, sbir programs in list form already
(define (put-label program)
	(map (lambda (line)
		(when (not (null? line))
			(when (or (= 3 (length line)) 
			      (and (= 2 (length line)) 
                              (not (list? (cadr line)))))
			(hash-set! *label-table* (cadr line) (- (car line) 1))))) program))

;; No practical loops in scheme, run repeatedly through tail-recursion using accumulator
(define (eval-line program line-num)
	(when (> (length program) line-num)
		(let ((line (list-ref program line-num)))
			(cond ((= (length line) 3)
			              (set! line (cddr line)) (run-line (car line) program line-num))
			      ((and (= (length line) 2) (list? (cadr line)))
				      (set! line (cdr line)) (run-line (car line) program line-num))
			      (else (eval-line program (+ line-num 1)))))))

;; Read in and run each line from sbir input files
(define (run-line command program line-num)
	(when (not (hash-has-key? *function-table* (car command)))
		(die "~s invalid command" (car command)))
	(cond ((eq? (car command) 'goto) 
	              (eval-line program (hash-ref *label-table* (cadr command))))
	      ((eq? (car command) 'if)
	              (if (eval-expr (car (cdr command)))
		              (eval-line program (hash-ref *label-table* (cadr (cdr command))))
			      (eval-line program (+ line-num 1))))
	      ((eq? (car command) 'print)
	              (if (null? (cdr command))
		              (newline) (eval-print (cdr command))) 
			      (eval-line program (+ line-num 1)))
		      (else ((hash-ref *function-table* (car command)) (cdr command))
		              (eval-line program (+ line-num 1)))))

;; Each of the operands is printed in sequence, with a space before Expression
;; values. A newline is output at the end of the print statement.
;; print statements are the only place Strings may occur in SBIR.
;;
;; Translate sbir print statements
;; Evaluate when given print argument
(define (eval-print expr)
	(map (lambda (x) (display (eval-expr x))) expr)
	(newline))

;; Constants are numbers. Note that names of Functions, Arrays, and
;; Variables all look like identifiers and their meaning is given by context.
;; In particular, the syntax of a function call and an array subscript is
;; ambiguous. The code for both is just to send a message to the Symbol
;; table and get backaresult.
;;
;; Determine category of object
;; Evaluate list recursively
;; Referred to hashexample.scm example
(define (eval-expr expr)
	(cond ((string? expr) expr)
	      ((number? expr) expr)
	      ((hash-has-key? *variable-table* expr)
	              (hash-ref *variable-table* expr))
	      ((list? expr)
	              (if (hash-has-key? *variable-table* (car expr)) 
		              (let ((head (hash-ref *variable-table* (car expr))))
				      (cond ((number? head) head)
				      	    ((vector? head) (vector-ref head (cadr expr)))
			                    ((procedure? head)
				                    (apply head (map (lambda (x) (eval-expr x)) (cdr expr))))
				      (else (die "Unable to evaluate espression type"))))
			       (die (list (car expr) " not found in table\n"))))))

;; The dim statement creates an array given by the variable name and
;; inserts it into the Symbol table, replacing any previous variable, array,
;; or function already in the Symbol table. The dimension of the array is
;; given by the expression.
;; Unlike C, the lower bound of the array is 1 and the upper bound is the
;; dimension, which may be an arbitrary expression. The expression is
;; rounded to the nearest integer before being used as the bound, which
;; must be positive.
;;
;; Translate sbir dim statements
;; Declare array
(define (eval-dim expr)
	(set! expr (car expr))
	(let ((arr (make-vector (eval-expr (cadr expr)) (car expr))))
		(var-put! (car expr) (+ (eval-expr (cadr expr)) 1))))

;; A let statement makes an assignment to a variable. The expression is
;; first evaluated. For a Variable, its value is stored into the Symbol table,
;; replacing whatever was there previously. For an Array, the store message
;; is sent to the vector representing the array. If the Symbol table
;; entry is not an array, an error occurs.
;;
;; Translate sbir let statements
;; Create variable
(define (eval-let expr)
	(var-put! (car expr) (eval-expr (cadr expr))))

;; Yet another recursive function to keep track of count variable
(define (input-count expr count)
	(if (null? expr) count (let ((input (read))) (if (eof-object? input) -1
		(begin (var-put! (car expr) input) 
		(set! count (+ 1 count)) (input-count (cdr expr) count))))))

;; Numeric values are read in and assigned to the input variables in
;; sequence. Arguments might be elements of an array. For each value
;; read into a Variable, the value is inserted into the Symbol table under
;; that variable’s key. For arrays, the array must already exist and the
;; subscript not be out of bounds.
;; The variable inputcount is inserted into the symbol table at end of execution
;; of this statement and initialized to the number of values successfully
;; read in. A value of −1 is returned to indicate end of file. If anything
;; other than a number occurs, that token is discarded, an error message
;; is printed, and scanning continues.
;;
;; Take input for variable-table
(define (eval-input expr)
	(var-put! 'eval-count 0)
	(if (null? (car expr)) 
		(var-put! 'eval-count -1) (begin (var-put! 'eval-count (input-count expr 0)))))

;; Check validity of arguments passed, else return usage-exit
;; When argument valid, set sbprogfile equal to it
;; Set program equal to inputfile commands
(define (main arglist)
	(if (or (null? arglist) (not (null? (cdr arglist))))
		(usage-exit)
		(let* ((sbprogfile (car arglist))
		       (program (readlist-from-inputfile sbprogfile)))
		       (put-label program) (eval-line program 0))))
	
;; Create hash table to translate sbir into scheme functions
(for-each
	(lambda (pair)
		(hash-set! *function-table* (car pair) (cadr pair)))
	`(

	  	;; The two Expressions are compared according to the given Relop, and if
		;; the comparison is true, control transfers to the statement, as for the
		;; goto statement. Note : <> is the symbol for not equal. The others
		;; should be obvious.
		(if       (void))
	  	;; Control transfers to the statement referred to by the Label. An error
		;; occurs if the Label is not defined.
		(goto     (void))
		(print    ,eval-print)
		(let      ,eval-let)
		(dim      ,eval-dim)
		(input    ,eval-input)
		
	))
	
(main (vector->list (current-command-line-arguments)))
