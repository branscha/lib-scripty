;; Greatest common divisor.
;;
(defun gcd (a b)
   (if (zero? $b)
      $a
      (if (= $a $b)
         $a
         (if (> $a $b)
            (gcd (- $a $b) $b)
            (gcd $a (- $b $a))))))
