;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; gambit.scm
;;;;
;;;; Implementation-specific code for the gambit scheme runtime, implementing
;;;; functions that are not standard in R4RS, or functions that are
;;;; implementation-specific.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An atom is that which is not a pair.
(define (atom? x)
  (not (pair? x)))

;; I honestly don't know what wait is, but apparently it is the right function
;; to use in Gambit.
(define wrong 'wait)
