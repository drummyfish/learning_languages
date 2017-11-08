;;; Show off the power of LISP with only the standard library.
;;;
;;; Miloslav Ciz, 2017
;;; WTFPL license

(defconstant FACTORIAL_OF 10)
(defconstant SORT_SIZE 100)

(defun factorial (x)
  (if (< x 2)
    1
    (* x (factorial (- x 1)))
  )
)

(defun bubble-sort (data)
  (dotimes (i (array-dimension data 0))
    (dotimes (j (array-dimension data 0))
      (if (> (aref data j) (aref data i))
        (rotatef (aref data j) (aref data i)) ; swap
      )
    )
  )
)

;--------------------------------------

(write-line (write-to-string (factorial FACTORIAL_OF)))

(write-line (write-to-string (get-decoded-time)))


(setf sort-array (make-array SORT_SIZE))

(dotimes (i (array-dimension sort-array 0))   ; init the array
  (setf (aref sort-array i) (mod (- SORT_SIZE i) (floor SORT_SIZE 4)))
)

(write-line (write-to-string sort-array))
(bubble-sort sort-array)
(write-line (write-to-string sort-array))
