

; -----
; a single forward quote (') in front of a name stops lisp from evaluating it as a variable

; ----
; A back quote (`) before an expression stops evaluation just like a forward quote.
; Any subexpression that's preceded by a comma is evaluated:
`(1 2 (+ 1 2))        ; ==> (1 2 (+ 1 2))
`(1 2 ,(+ 1 2))       ; ==> (1 2 3)

; -----
; ,@ "splices" the value of the following expression--which must evaluate to a list--into the enclosing list.
`(and ,(list 1 2 3))   ; ==> (AND (1 2 3))
`(and ,@(list 1 2 3))  ; ==> (AND 1 2 3)

; -----
; #' is shorthand for "Get me the function with the following name." 
; Without the #', Lisp would treat the symbol as the name of a variable and look up the value of the variable, not the function.

; -----
; &key - keyword parameters
 (defun foo (&key a b c) (list a b c))
 ; ==>
 (foo :c 3 :b 2 :a 1)  ; ==> (1 2 3)
 (foo)                 ; ==> (NIL NIL NIL)

; -----
; &rest - arbitrary number of arguments that get collected into a list

; -----
; &optional - optional parameters to a function
(defun foo (a b &optional c d) (list a b c d))
(defun make-rectangle (width &optional (height width)) ())


; -----
; Setting default values
(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))
(foo :a 1 :c 3)       ; ==> (1 20 3 T)
(foo)                 ; ==> (NIL 20 30 NIL)

; -----
; simple macro example:
(defmacro backwards (expr) (reverse expr))
(backwards ("hello, world" t format)) ; => compiler will produce: (format t "hello, world")


; -----
; Ways of defining variables
(let ((x 10))) 
; global variables:
(defvar *count* 0
  "Count of widgets made so far.") ; defvar assigns the value to the variable only if the variable is undefined

(setf *count* (+ 1 *count*)) ; setf changes the value of a global variable

(defparameter *gap-tolerance* 0.001
  "Tolerance to be allowed in widget gaps.")
;; defining a constant   
(defconstant name initial-value-form [ documentation-string ])

; asignment:
(setf x 10) ;  Simple variable:    
(setf (aref a 0) 10);  Array:              
(setf (gethash 'key hash) 10);  Hash table:         
(setf (field o) 10);  Slot named 'field': 

; ---- variable scope when using let
(defun foo (x)
  (format t "Parameter: ~a~%" x)      ; |<------ x is argument 
  (let ((x 2))                        ; |
    (format t "Outer LET: ~a~%" x)    ; | |<---- x is 2
    (let ((x 3))                      ; | |
      (format t "Inner LET: ~a~%" x)) ; | | |<-- x is 3
    (format t "Outer LET: ~a~%" x))   ; | |
  (format t "Parameter: ~a~%" x))     ; |


; --- with let* you can use variables declare earlier
 (let* ((x 10) (y (+ x 10))) (list x y))


 ; -- closure example:
(defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))
(funcall *fn*) ; => 1
(funcall *fn*) ; => 2
(funcall *fn*) ; => 3