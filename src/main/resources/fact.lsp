;; Factorial function, recursive implementation.
;;
(defun fact (n)
    (if (> $n 0)
        (* $n (fact (- $n 1)))
        1))

;; Factorial function, iterative version.
;;
(defun fact-iter (n)
   (fact-iter* 1 1 $n))

(defun fact-iter* (product counter n)
   (progn
      (while (<~ $counter $n)
         (progn
            (set product (* $product $counter))
            (set counter (+ $counter 1))))
      $product))
      