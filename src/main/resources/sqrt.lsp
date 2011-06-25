;; Square roots approximation with Newtons method.
;; Example: (sqrt 100) ==> 10.00000000013990
;;
(defun sqrt (x)
  (sqrt-iter 1.0 $x))

; The iteration step.
(defun sqrt-iter (guess x)
    (if (good-enough? $guess $x)
      $guess
      (sqrt-iter (improve $guess $x)
                 $x)))
; Improve step.
(defun improve (guess x)
   (average $guess (/ $x $guess)))
   
; Test if we're done.
(defun good-enough? (guess x)
   (< (abs (- (square $guess) $x)) 0.001))

(defun square (x)
   (* $x $x))

(defun sum-of-squares (x y)
   (+ (square $x) (square $y)))

(defun average (x y)
   (/ (+ $x $y) 2))