;verision 1 doesn't show which test fails
(defun test-+ ()
    (and 
        (= (+ 1 2) 3)
        (= (+ 1 2 3) 6)
        (= (+ -1 -3) -4)))

; version 2 - prety print
(defun test-+ ()
    (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
    (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
    (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

; version 3 - improving code
(defun report-result (result form)
    (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defun test-+ ()
    (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
    (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
    (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

; version 4 - add a macro
(defun report-result (result form)
    (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defmacro check (form)
    `(report-result ,form ',form))

; version 5
(defmacro check (&body forms)
    `(progn
        ,@(loop for f in forms collect `(report-result ,f ',f))))

; version 6 - combine results
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defmacro check (&body forms)
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun test-+ ()
    (check  
        (= (+ 1 2) 3)
        (= (+ -1 -3) -4)
        (= (+ 1 2 3) 6)))

(defun test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(defun test-arithmetic ()
  (combine-results
   (test-+)
   (test-*)))