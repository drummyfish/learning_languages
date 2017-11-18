;;; Show off the power of LISP with only the standard library.
;;;
;;; Miloslav Ciz, 2017
;;; WTFPL license

(defconstant FACTORIAL_OF 10)
(defconstant SORT_SIZE 100)

(defun print-header (title delim)
  (write-line "")

  (format t "~A~%" title)

  (dotimes (i (length title))
    (format t "~A" delim)
  )

  (write-line "")
)

(defun factorial-recursive (x)
  (if (< x 2)
    1
    (* x (factorial-recursive (- x 1)))
  )
)

(defun factorial-iterative (x)
  (setq result 1)
  
  (dotimes (i x)
    (setq result (* result (+ i 1))) 
  )

  result
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

(print-header "Showing off the power of LISP!" #\~)
(print-header "general" #\-)

(write-line (write-to-string (factorial-recursive FACTORIAL_OF)))
(write-line (write-to-string (factorial-iterative FACTORIAL_OF)))

(write-line (write-to-string (get-decoded-time)))

(format t "some random numbers from 1 to 10: ")

(dotimes (i 10)
  (format t "~D " (+ (random 10) 1))
)

(write-line (write-to-string (random 10)))

(setf sort-array (make-array SORT_SIZE))

(dotimes (i (array-dimension sort-array 0))   ; init the array
  (setf (aref sort-array i) (mod (- SORT_SIZE i) (floor SORT_SIZE 4)))
)

(write-line (write-to-string sort-array))
(bubble-sort sort-array)
(write-line (write-to-string sort-array))
