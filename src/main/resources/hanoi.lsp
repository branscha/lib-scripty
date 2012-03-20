;; The Towers Of Hanoi
;;
(defun dohanoi (n to from u)
  (if (> $n 0)
     (progn
         (dohanoi (- $n 1) $u $from $to)
         (println  move $from --> $to)
         (dohanoi (- $n 1) $to $u $from))))

(defun hanoi (n)
    (dohanoi $n 3 1 2))