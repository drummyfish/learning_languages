;;; Show off the power of LISP with only the standard library.
;;;
;;; Miloslav Ciz, 2017
;;; WTFPL license

(defconstant FACTORIAL_OF 10)
(defconstant SORT_SIZE 10)

(defun factorial (x)
  (if (< x 2)
    1
    (* x (factorial (- x 1)))
  )
)

;--------------------------------------

(write-line (write-to-string (factorial FACTORIAL_OF)))

(write-line (write-to-string (get-decoded-time)))


(setf sort-array (make-array SORT_SIZE))

(dotimes (i (array-dimension sort-array 0))
  (setf (aref sort-array i) i)
)

(write-line (write-to-string sort-array))
