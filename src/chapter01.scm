;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; chapter01.scm
;;;;
;;;; This file containers the textbook definitions of the first chapter of
;;;; Christian Quinnec's "Lisp in Small Pieces" (LiSP)
;;;;
;;;; It contains a minimal evaluator for a simple Scheme dialect that leans
;;;; heavily on the host Scheme implementation for the heavy lifting.
;;;;
;;;; To minimize confusion, we use the function names "evaluate" and "invoke"
;;;; instead of the traditional "eval"/"apply" as we will currently be heavily
;;;; reusing the host implementations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Home-made evaluator, leaniing heavily on host Scheme implemntation.
;; expr - Expression to evaluate
;; env - Environment to inherit
(define (evaluate expr env)
  ;; We're not a composite / list-based expression.
  (if (atom? expr)
      (cond
       ;; Are we a variable? Well then, see if we have an associated value.
       ((symbol? expr) (lookup expr env))
       ;; Are we a primitive atom? OK then, auto-unquote to get the real value.
       ((or (number? expr)
	    (string? expr)
	    (char? expr)
	    (boolean? expr)
	    (vector? expr))
	;; Autounquoting is basically returning the expr as-is.
	expr)
       ;; This is an invalid atom expression.
       (else (wrong "Cannot evaluate expression" expr)))
      ;; OK I guess we are a list-based expression.
      ;; So what type is your first element?
      (case (car expr)
	;; We start by looking at all the special forms, with special evaluation
	;; behaviour.
	;; You're a quoted list, so just return the next item.
	((quote) (cadr expr))
	;; You're a conditional, so evaluate the test condition, then evaluate
	;; and return the appropriate branch.
	;;
	;; Note that we match to our language dialect's false value, using
	;; eq? as our false value is a singleton value, so comparing memory
	;; location of the values makes sense.
	((if) (if (not (eq? (evaluate (cadr expr) env) the-false-value))
		  (evaluate (caddr expr) env)
		  (evaluate (cadddr expr) env)))
	;; You're a begin/progn, a sequence of expressions to run.
	((begin) (eprogn (cdr expr) env))
	;; You're a mutation, which is a side-effect.
	((set!) (update! (cadr expr) env (evaluate (caddr expr) env)))
	;; You're an (anonymous) function definition.
	((lambda) (make-function (cadr expr) (cddr expr) env))
	;; Use the standard evaluate form, invoke the function associated with
	;; the first symbol, applied to the arguments that follow.
	(else (invoke (evaluate (car expr) env)
		      (evlis (cdr expr) env))))))


;; An arbitrary value we use to represend false in our home-made Scheme dialect
;; For some reason we want to decouple this from our host scheme implementation
;;
;; Remember our scheme convention that anything that isn't this value is true
(define the-false-value (cons "false" "boolean"))


;; Evaluate a sequence of expressions. If the sequence is empty, return an
;; empty list.
;; exprs - Expresions to evaluate
;; env - Envirornment to inherit
(define (eprogn exprs env)
  ;; Are we a list of expressions?
  (if (pair? exprs)
      ;; Do we have an additional expression after this first one?
      (if (pair? (cdr exprs))
	  ;; Ok, evaluate the first expression and then recursively run the next
	  ;; one as part of the system.
	  ;;
	  ;; Note that the (begin) we use here is part of the host scheme
	  ;; implementation.
	  (begin (evaluate (car exprs) env)
		 (eprogn (cdr exprs) env))
	  ;; We're at the last expression in the list, so return the value of
	  ;; its evaluation.
	  (evaluate (car exprs) env))
      ;; We weren't a list so we must have been an empty list, so return that.
      '()))


;; Utility function that takes a list of expressions and returns the
;; corresponding list of values of those expressions.
;; exprs - List of expressions
;; env ; Environment to inherit
(define (evlis exprs env)
  ;; Does our list contain a value?
  (if (pair? exprs)
      ;; Evaluate the first expression, and then recursively evaluate the rest
      ;; of the list.
      (cons (evaluate (car exprs) env)
	    (evlis (cdr exprs) env))
      ;; Otherwise, just return an empty list.
      '()))


;; Looks up a key in the environment alist, and return the corresponding value.
;; id - key to look up
;; env - environment to look in
(define (lookup id env)
  ;; Does our environment contain a key-value binding?
  (if (pair? env)
      ;; Does the first entry in our environment match the ID?
      (if (eq? (caar env) id)
	  ;; Return the value of that binding.
	  (cdar env)
	  ;; Otherwise, look in the remaining bindings.
	  (lookup id (cdr env)))
      ;; Guess that id's has no value in this enviromment, so error out.
      (wrong "No such variable binding exists: " id)))


;; Update an existing variable binding in an environment to have a new value.
;; Note that we cannot use this to append a new variable binding, for some
;; reason, this will produce an error.
;;
;; id - ID of the variable to update
;; env - environment we are going to modify
;; value - the new value we will associate in this environment with the id
(define (update! id env value)
  ;; Do we have a variable binding in our environment?
  (if (pair? env)
      ;; Does the first variable binding match our desired variable ID?
      (if (eq? (caar env) id)
	  ;; We use the host scheme's (begin) to side-effectfully update the
	  ;; key-value variable binding.
	  (begin (set-cdr! (car env) value)
		 value)
	  ;; Otherwise, recurse over the remaining variable bindings in our env
	  (update! id (cdr env) value))
      ;; The variable binding didn't originally exist, so error out.
      (wrong "No such variable binding exists to update: " id)))

;; Add new variable binding to an environment.
;; NOTE: the number of items in the variables list should match the number of
;; items in the values list, otherwise an error will occur.
;;
;; env - The current environment to be extended
;; variables - The list of variable IDs to add to the environment
;; values - The list of variable values to add to the environment
(define (extend env ids values)
  ;; Is the ID list non-empty?
  (cond ((pair? ids)
	 ;; Is the value list a list?
	 (if (pair? values)
	     ;; Bind the first ID and value in our input lists and follow them
	     ;; by recursively extending the existing environment with the
	     ;; remaining ids and values.
	     (cons (cons (car ids) (car values))
		   (extend env (cdr ids) (cdr values)))
	     ;; There are still IDs but no values, error on mismatching lists
	     (wrong "Too few values compared to variable list"))
	 ;; Is the ID list empty?
	 ((null? ids)
	  ;; Is the value list also empty?
	  (if (null? values)
	      ;; Ok this means we can append the existing environment and return
	      ;; at the end of our recursively appended list.
	      env
	      ;; There are still values but no IDs, error on mismatching lists
	      (wrong "Too many values compared to variable list")))
	 ;; Our ID was a single symbol, assume we can just prepend it to our
	 ;; existing list.
	 ;;
	 ;; The justification seems to be because we might be handed an improper
	 ;; list, one that doesn't have nil/'() as the last value.
	 ((symbol? ids) (cons (cons variables values) env)))))


;; This is the apply-equivalent for our scheme dialiect
;;
;; fn - Function to call
;; args - arguments to apply
(define (invoke fn args)
  ;; Are you a recognized procedure in the host scheme runtime?
  (if (procedure? fn)
      ;; Apply the function to the supplied arguments
      (fn args)
      ;; Not a function, error out.
      (wrong "Attempting to apply non-function " fn)))


;; Create an function object/value or whatever (as compared to invoking an
;; existing function.
;;
;; variables the list of variable expressions to apply the body to
;; body - the expression that is the body of the function
;; env - the enviironment to inherit
(define (make-function variables body env)
  (lambda (values)
    ;; The body of a function is effectively an implicit begin/progn
    (eprogn body (extend env variables values))))


;; The empty environment is just an empty list
(define env.init '())


;; The global environment starts as the empty environment
(define env.global env.init)


;; A macro which I guess sets up initial value bindings
;; for our environment
;;
;; Thera are two forms: One which binds "name" to a magic symbol 'void
;; indicating it simply exists without a real value
;;
;; The other creates a variable binding with a given name and value.
(define-syntax definitial
  (syntax-rules ()
    ((definitial name)
     (begin (set! env.global (cons (cons 'name 'void) env.global))
	    'name))
    ((definitial name value)
     (begin (set! env.global (cons (cons 'name value) env.global))
	    'name))))

;; A macro which I guess sets up variable bindings with specific wrapping
;; of functions, to check its arity and error out if we pass an arguement list
;; that doesn't confirm to that arity.
(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name fn arity)
     (definitial name
       (lambda (values)
	 (if (= arity (length values))
	     (apply fn values)
	     (wrong "Function invoked with arglist of wrong arity: "
		    (list name values))))))))


;; Populate our global environment with basic boolean and nil variables
;; It doesn't really matter what they mean as long as we have lisplike
;; expectations:
;;
;; For example, in Scheme, anything that isn't false is true.
;; #t is a scheme convenience value when all that matters about the value
;; is its truth.
;;
;; We define nil to represent the empty list.
(definitial t #t)
(definitial f the-false-value)
(definitial nil '())


;; Since our language can't define new variables itself, we supply some common
;; variable names with undefined values for our experimentation.
(definitial foo)
(definitial bar)
(definitial fib)
(definitial fact)


;; Populate some functions from our host runtime, providing a value
;; for our arity-checker.
(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive eq? eq? 2)
(defprimitive < < 2)


;; This is our chapter's main entry-point that brings up a really simple REPL.
(define (chapter01-scheme)
  (define (toplevel)
    (display (evaluate (read) env.global))
    (toplevel))
  (toplevel))
