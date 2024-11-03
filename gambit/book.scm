;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; gambit.scm
;;;;
;;;; Implementation-specific code for the gambit scheme runtime, implementing
;;;; functions that are not standard in R4RS, or functions that are
;;;; implementation-specific.

;;;; This file is inspired by files that accompany the book:
;;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;;; Newest version may be retrieved from:
;;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file customizes the Gambit interpreter from Marc Feeley in order
;;; to run the source and exercises of the book.
;;; Under Unix, start gsi as:
;;;          gsi '(include "gambit/book.scm")'

;; An atom is that which is not a pair.
(define (atom? x)
  (not (pair? x)))

;; I honestly don't know what wait is, but apparently it is the right function
;; to use in Gambit.
(define wrong 'wait)
