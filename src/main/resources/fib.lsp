;; Fibonacci numbers.
;; Inefficient, recursive implementation.
;;
(defun fib (n)
    (if (= $n 0)
        1
        (if (= $n 1 )
            1
            (+ (fib (- $n 1))
               (fib (- $n 2))))))

;; Faster iterative implementation. 
;;
(defun fib2 (n)
   (let (i=$n f1=1 f2=1)
      (while (> $i 1)
          (let (f1old=$f1 f2old=$f2)
             (progn
                (set f1 (+ $f1old $f2old))
                (set f2 $f1old)
                (set i (- $i 1))
                $f1)))))

