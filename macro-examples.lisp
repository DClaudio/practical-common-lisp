
;; prime numbers macro example
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))
    
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
      ((> ,var ,end))
      ,@body))

;; usage example:  (do-primes (p 0 19) (format t "~d " p))


;; improving the macro to avoid abstraction leaks
;; using gensym to make sure "ending-value-name" variable name is unique
;; making sure "start" and "end" are evaluated just once 
(defmacro do-primes-no-leaks ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))) (,ending-value-name ,end))
        ((> ,var ,ending-value-name))
      ,@body)))

;; Advanced macro example :
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defmacro do-primes-with-once-only ((var start end) &body body)
  (once-only (start end)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))

