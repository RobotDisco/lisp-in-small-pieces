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

;; Exercise 1.7
;; Trivial continuation - return result as-is
(define (trivial-cont val)
  val)

;; Home-made evaluator, leaniing heavily on host Scheme implemntation.
;; expr - Expression to evaluate
;; env - Environment to inherit
;; Exercise 1.7: Implement call-with-current continuation i.e. `call/cc'.
;;   Continuations are scheme's way of doing things like exception handling,
;;   although in truth they are more generic and almost as powerful as GOTO
;;   without being as unregulated (there's at least _some_ control of how the)
;;   computation jumps work. The way this happens is that the evaluate function
;;   uses something called "continuation-passing-style", which means that
;;   alongside the current environment bindings, we also pass into evaluate a
;;   callback-like function (called the current continuation) that we apply to
;;   the result of our current evaluation. When using a primitive like "call/cc",
;;   we can change what the current continuation is. This allows us to set up
;;   things like exception handlers, or some people use them to implement
;;   generators.
;;
;;   As far as I call tell this is what happens when you have minimal syntax and
;;   need to expicitly extend lambda calculus evaluation rules to implement
;;   something like an exception i.e. breaking out the normal evaluation flow.
;;   Although in this case we literally have to modify our evaluation logic
;;   to support the capability by _always_ passing our evaluation result to
;;   our current continuation and supplying a really trivial starting
;;   continuation (i.e. return the value as is unmodified) to start.
;;
;;   A REALLY WEIRD TWIST about continuations is that we're not relying at all
;;   on a return value ... we're using `cont' as a callback, essentially, and
;;   it is what will be returning the final value ultimately.
;; cont - The current continuation i.e. the remainder of the program after this
;;     point. Basically a callback to apply to our evaluation result thus far.
(define (evaluate expr env cont)
  ;; We're not a composite / list-based expression.
  (if (atom? expr)
      (cond
       ;; Are we a variable? Well then, see if we have an associated value.
       ((symbol? expr) (cont (lookup expr env)))
       ;; Are we a primitive atom? OK then, auto-unquote to get the real value.
       ((or (number? expr)
	    (string? expr)
	    (char? expr)
	    (boolean? expr)
	    (vector? expr))
	;; Autounquoting is basically returning the expr as-is.
	(cont expr))
       ;; This is an invalid atom expression.
       (else (wrong "Cannot evaluate expression" expr)))
      ;; OK I guess we are a list-based expression.
      ;; So what type is your first element?
      (case (car expr)
	;; We start by looking at all the special forms, with special evaluation
	;; behaviour.
	;; You're a quoted list, so just return the next item.
	((quote) (cont (cadr expr)))
	;; You're a conditional, so evaluate the test condition, then evaluate
	;; and return the appropriate branch.
	;;
	;; Note that we match to our language dialect's false value, using
	;; eq? as our false value is a singleton value, so comparing memory
	;; location of the values makes sense.
	((if) (if (not (eq? (evaluate (cadr expr)
				      env
				      trivial-cont) the-false-value))
		  (evaluate (caddr expr) env cont)
		  (evaluate (cadddr expr) env cont)))
	;; You're a begin/progn, a sequence of expressions to run.
	((begin) (eprogn-continuation (cdr expr) env cont))
	;; You're a mutation, which is a side-effect.
	;; ((set!) (update! (cadr expr) env (evaluate (caddr expr) env)))
	;;
	;; Ex 1.7 -- Remember, we're using a continuation callback. So we
	;; can't rely on return values, we have to leverage callbacks. This
	;; is your clue as to whether you need a specialized contuation or
	;; the one currently passed in.
	((set!) (evaluate (caddr expr) env
			  (lambda (res)
			    ;; Update environment with result as a side-effect
			    ;; (cadr expr) still indicates our key.
			    (update! (cadr expr) res env)
			    ;; We return the evaluated value on set! so now
			    ;; apply our current continuation to the evaluated
			    ;; value.
			    (cont res))))
	;; You're an (anonymous) function definition.
	((lambda) (make-function (cadr expr) (cddr expr) env) cont)
	;; Use the standard evaluate form, invoke the function associated with
	;; the first symbol, applied to the arguments that follow.
	(else
	 (let ((fn (car expr))
	       ;; (args (evlis (cdr expr) env))
	       )
	   (begin
	     ;; Ex 1.7 Evaluate the function and apply it to our supplied
	     ;; arguments, done via continuation callback
	     (evlis-continuation-tco
	      (cdr expr)
	      env
	      (lambda (args)
		;; Exercise 1.1: Rudimentary tracer
		;; Print the function name and the arguments
		(display (list "FN-ENTER:" fn "args:" args))
		(newline)
		(evaluate fn env (lambda (fnval)
				   ;; Exercise 1.1: Rudimentary tracer
				   ;; Print the function name and the result
				   (invoke fnval args env
					   (lambda (res)
					     (display (list
						       "FN-EXIT:"
						       fn
						       "result:"
						       res))
					     (newline)
					     (cont res)))))))
	     ;; (let
	     ;; 	 ;; Call the function and supply the arguments
	     ;; 	 ((res (invoke (evaluate fn env)
	     ;; 	  	       args)))
	     ;;   ;; Exercise 1.1: Rudimentary tracer
	     ;;   ;; Print the function name and the result
	     ;;   (display (list "FN-EXIT:" fn "result:" res))
	     ;;   (newline)
	     ;;   res)
	     ))))))


;; An arbitrary value we use to represend false in our home-made Scheme dialect
;; For some reason we want to decouple this from our host scheme implementation
;;
;; Remember our scheme convention that anything that isn't this value is true
(define the-false-value (cons "false" "boolean"))


;; Evaluate a sequence of expressions. If the sequence is empty, return an
;; empty list.
;; exprs - Expressions to evaluate
;; env - Environment to inherit
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
      (cont '())))

;; Evaluate a sequence of expressions. If the sequence is empty, return an
;; empty list.
;; exprs - Expressions to evaluate
;; env - Environment to inherit
;; cont - Ex 1.7: current continuation function
(define (eprogn-continuation exprs env cont)
  ;; Are we a list of expressions?
  (if (pair? exprs)
      ;; Do we have an additional expression after this first one?
      (if (pair? (cdr exprs))
	  ;; Ok, evaluate the first expression and then recursively run the next
	  ;; one as part of the system.
	  ;;
	  ;; Note that the (begin) we use here is part of the host scheme
	  ;; implementation.
	  ;;
	  ;; Ex 1.7 Using continuations makes this easier, because we can
	  ;; evaluate the head expression for its side-effect, then use an
	  ;; explicit continuation that ignores the result and evaluates the
	  ;; next s-expression in the eprogn chain.
	  (evaluate (car exprs) env
		    (lambda _
		      (eprogn-continuation (cdr exprs) env cont)))
	  ;; We're at the last expression in the list, so return the value of
	  ;; its evaluation.
	  ;; Ex 1.7 -- Since this is our final value, apply our current
	  ;; continuation to the result
	  (evaluate (car exprs) env cont))
      ;; We weren't a list so we must have been an empty list, so return that.
      ;; Or for Ex 1.7 -- apply our current continuation to the empty list.
      (cont '())))


;; Utility function that takes a list of expressions and returns the
;; corresponding list of values of those expressions.
;; exprs - List of expressions
;; env ; Environment to inherit
(define (evlis exprs env)
  ;; Does our list contain a value?
  (if (pair? exprs)

      ;; Exercise 1.2 - Remove an unnecessary recursion if there is only one
      ;; item in the list of expressions to evaluate.
      (if (eq? (cdr exprs) '())
	  ;; The cdr of exors is an empty list, we can just return an empty list
	  ;; as the cdr instead of doing a useless recursive call.
	  (cons (evaluate (car exprs) env) '())
	  ;; Evaluate the first expression, and then recursively evaluate the
	  ;; rest of the list.
	  (cons (evaluate (car exprs) env)
		(evlis (cdr exprs) env)))
      ;; Otherwise, just return an empty list.
      '()))
;; Ex 1.7 - Continuation callbacks make this weird, as we have to recurse via
;; callback but also preserve the order. This is going to be hard to follow.
;; exprs - List of expressions
;; env ; Environment to inherit
;; cont - Ex 1.7: current continuation function
(define (evlis-continuation exprs env cont)
  ;; Are we at the end of the list?
  (if (null? exprs)
      ;; Our base case, where the recursion stops.
      ;;
      ;; Pass the empty list (the list end) to the callback, which maybe not be
      ;; the original continuation but the one we create in our our recursion
      ;; (see the lambda below)
      (cont '())
      ;; Otherwise, evaluate the head of the list, then recursively handle
      ;; the rest of the list. We use a custom continuation to handle the
      ;; recursion and keep the proper order.
      (evaluate (car exprs)
		env
		(lambda (first)
		  (evlis-continuation (cdr exprs)
				       env
				       (lambda (rest)
					 ;; This looks weird, but remember that
					 ;; in a recursive call to evlist, so
					 ;; the lambda isn't necessary the one
					 ;; passed in originally. This cont
					 ;; instead will get passed up the chain
					 ;; with the original one only called
					 ;; when the list has been fully consed.
					 (cont (cons first rest))))))))
;; The above function is terrible for tail recursion. Making it handle tail
;; recursion well will make this function even more obtuse.
(define (evlis-continuation-tco exprs env cont)
  ;; We need to use an iterative helper function to handle this, the form we
  ;; currently have doesn't have an accumulation value we can lead on for
  ;; tail-call-optimization
  ;;
  ;; So call it, providing an empty list we will extend.
  (evlis-cont-tco-acc exprs env '() cont))
;; Accmumlating helper function for an evlist based on continuations.
;; This relies on Scheme's tail-call-optimization because, as you can see
;; we can lean on the recursive callback repeatedly resolving to
;; a straight call to ev-list-const-tco-acc, which means we can just iterating
;; on the last value in the stack rather than growing the call stack for each
;; recursion. Remember the last expression in our continuation-using evaluate
;; is basically (k val) which will become (evlis-cont-tco-acc ...).
;;
;; exprs - List of expressions
;; env - Environment to inherit
;; acc - Accumulator which gathers our list (in reverse)
;; cont - Ex 1.7: current continuation function
(define (evlis-cont-tco-acc exprs env acc cont)
  ;; Are we at the end of the list?
  (if (null? exprs)
      ;; Our base case, where the recursion stops.
      ;;
      ;; Our accumulated list is in the wrong order because we can only add to
      ;; the front of the but we processed values from first-to-last, so our
      ;; accumulator is currently in last-to-first sequence.
      ;; Reverse that before passing it to our original call-back.
      (cont (reverse acc))
      ;; Otherwise, evaluate the head of the list
      (evaluate (car exprs)
		env
		(lambda (first)
		  ;; Iterative recursion
		  (evlis-cont-tco-acc
		   ;; Our remaining expr list keeps srinking
		   (cdr exprs)
		   env
		   ;; And our accumulator keeps growing
		   (cons first acc)
		   ;; And pass the original continuation
		   cont)))))


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

;;; Ex 1.3
;;; Given a extend definition of:
(define (extend env names values)
  (cons (cons names values) env))
;;; Lookup would have to look like:
(define (lookup id env)
  ;; does our env list have another entry?
  (if (pair? env)
      ;; Yes, the car contains the variable IDs and the rest of the list
      ;; contains the variable values.
      (let curr ((ids (caar env))
		 (values (cdar env)))
	;; Is the id list just a symbol (i.e. last item in an improper list?)
	(cond ((symbol? ids)
	       (if (eq? ids id)
		   ;; if so, if the input ID equals it, return the value.
		   values
		   ;; if not, move to the next entry in the environment list.
		   (lookup id (cdr env))))
	      ;; If the current ID list is null, move to the next entry in the
	      ;; environment list.
	      ((null? ids) (lookup id (cdr env)))
	      ;; If the id list isn't a symbol or null, it must be a list
	      ;; Is the first entry of the ID list the one we want?
	      ((eq? (car ids) id)
	       ;; If so, make sure we have a corresponding value.
	       (if (pair? values)
		   ;; And return it
		   (car values)
		   ;; Otherwise, we have an invalid env list entry.
		   (wrong "Too few values compared to variable list." env)))
	      ;; If we haven't found our ID we still have values left
	      (else (if (pair? values)
			;; Look up next id and value in our current env entry
			(curr (cdr ids) (cdr values))
			;; If we don't have any values left, our env is invalid
			(wrong "Too few values compared to variable list." env)))))
      ;; If our env list is empty, there was no such binding.
      (wrong "No such variable binding exists: " env)))
;;; update! would have to look like
;;; TODO handle inproper lists
(define (update! id env value)
  ;; does our env list have another entry?
  (if (pair? env)
      ;; Yes, the car contains the variable IDs and the rest of the list
      ;; contains the variable values.
      (let curr ((ids (caar env))
		 (values (cdar env)))
	;; Is the id list just a symbol (i.e. last item in an improper list?)
	;; If the current ID list is null, move to the next entry in the
	;; environment list.
	(cond ((null? ids) (update! id (cdr env) value))
	      ;; If the id list isn't a symbol or null, it must be a list
	      ;; Is the first entry of the ID list the one we want?
	      ((eq? (car ids) id)
	       ;; If so, make sure we have a corresponding value.
	       (if (pair? values)
		   ;; And update it
		   (begin
		     (set-car! values value)
		     env)
		   ;; Otherwise, we have an invalid env list entry.
		   (wrong "Too few values compared to variable list.")))
	      ;; If we haven't found our ID we still have values left
	      (else (if (pair? values)
			;; This is a bit of a hack; if our environment entry
			;; ends with an improper list, then convert it into a
			;; proper list as I can't figure out how to do a mutation
			;; otherwise.
			(if (symbol? (cdr ids))
			    (begin
			      (set-cdr! ids (list (cdr ids)))
			      (set-cdr! values (list (cdr values)))
			      (curr (cdr ids) (cdr values)))
			    ;; Otherwise, just look up next id and value in
			    ;; current env entry
			    (curr (cdr ids) (cdr values)))
			;; If we don't have any values left, our env is invalid
			(wrong "Too few values compared to variable list.")))))
      ;; If our env list is empty, there was no such binding.
      (wrong "No such variable binding exists: " id)))


;; This is the apply-equivalent for our scheme dialiect
;;
;; fn - Function to call
;; args - arguments to apply
(define (invoke fn args env cont)
  (display (list "FN-IN:" fn "args:" args))
  ;; Are you a recognized procedure in the host scheme runtime?
  (if (procedure? fn)
      ;; Apply the function to the supplied arguments
      (fn args env cont)
      ;; Not a function, error out.
      (wrong "Attempting to apply non-function " fn)))


;; Create an function object/value or whatever (as compared to invoking an
;; existing function.
;;
;; variables the list of variable expressions to apply the body to
;; body - the expression that is the body of the function
;; env - the enviironment to inherit
;; cont - Ex 1.7: current continuation function
;;
(define (make-function variables body env)
  (lambda (values)
    ;; The body of a function is effectively an implicit begin/progn
    (eprogn body (extend env variables values) cont)))


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
    ((defprimitive name arity fn)
     (definitial name
       ;; Ex 1.7 Continuations wind up infecting here as well
       (lambda (args env cont)
	 (if (= arity (length args))
	     (fn args env cont)
	     (wrong "Function invoked with arglist of wrong arity: "
		    (list name args))))))))


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
;; (defprimitive cons 2 cons 2)
;; (defprimitive car car 1)
;; (defprimitive set-cdr! set-cdr! 2)
;; (defprimitive + + 2)
;; (defprimitive eq? eq? 2)
;;
;; Ex 1.7 This becomes more annoying when we use continuations, we can't rely on
;; just passing to the hosting runtime's function as is. It's going to be a lot
;; of busywork reconstituting the argument and applying it to the hosting
;; runtime's functions.
(defprimitive cons 2
  (lambda (args env cont)
    (cont (cons (car args) (cdr args)))))
(defprimitive car 1
  (lambda (args env cont)
    (cont (car args))))
(defprimitive set-cdr! 2
  (lambda (args env cont)
    (set-cdr! (car args) (cadr args))))
(defprimitive + 2
  (lambda (args env cont)
    (cont (+ (car args) (cadr args)))))
(defprimitive eq? 2
  (lambda (args env cont)
    (cont (eq? (car args) (cadr args)))))


;; Ex 1.5 - Don't just return the host platform's boolean value, use
;; the custom boolean values we have invented.
;; Ye original defprimitive
;; (defprimitive < < 2)
(defprimitive < 2
  (lambda (args env cont)
    (if (< (car args) (cadr args))
	(evaluate 't env.global cont)
	(evaluate 'f env.global cont))))
;; Ex 1.6 - Implement (list)
;; This relies on "implicit dot notation":
;; (lambda arg arg) returns the whole argument list, compared to
;; (lambda (arg) arg) which assumes a single value
;;
;; We're using definitial instead of defprimitive to not deal with the arity
;; checking; I don't know how to check a variable-arity primitive.
;;
;; Notice how I'm not using the straightforward function definitino but an apply
;; This is because ... say we evaluate `(list 1 2 3 foo 5)'
;; Our evaluate line will take the car of expr, which is 'list. It will evaluate
;; it and get back a lambda value to invoke.
;;
;; It will pass the remaining values in the list `(1 2 3 foo 5)' to (evlis)
;; which will run through the list and substitute foo for a real value ... let's
;; say we get an argument list back which is `(1 2 3 4 5)' based on the
;; environment.
;; OK, so now we invoke ... (invoke (lambda () ...) (1 2 3 4 5))
;; -> ((lambda args args) (1 2 3 4 5))
;; -> ((lambda ((1 2 3 4 5)) ((1 2 3 4 5))))
;; -> ((1 2 3 4 5))
;;
;; By using 'apply, we use a macro to flatten the single list argument into
;; the list of arguments itself rather than treating it like a single element
;; in the argument list that is itself a list.
;; -> (apply (lambda args args) (1 2 3 4 5))
;; -> ((lambda (1 2 3 4 5) (1 2 3 4 5)))
;; -> (1 2 3 4 5)
(definitial list
  (lambda (args env cont)
    (cont (apply (lambda lst lst) args))))

;; This is our chapter's main entry-point that brings up a really simple REPL.
(define (chapter01-scheme)
  (define (toplevel)
    ;; A nice prompt, just to make me feel better than an empty line,
    (display "LiSP> ")
    ;; We are going to do some preprocessing.
    ;; The Read of the recursive REPL loop.
    (let ((input (read)))
      ;; What's the simplest way to recognize a sequence so I can quit before
      ;; evaluating?
      (if (equal? input '(unquote quit))
	  (begin (display "Bye.")
		 (newline))
	  (begin
	    ;; The Evaluate and Print section of the REPL loop.
	    ;; Note that we supply a starting continuation to just return the
	    ;; evaluated value as-is.
	    (display (evaluate input env.global trivial-cont))
	    (newline)
	    ;; Here our REPL leans on tail recursion to Loop.
	    (toplevel)))))
  (display "Welcome to Gaelan's Lisp. Use ,quit to quit.")
  (newline)
  (toplevel))
