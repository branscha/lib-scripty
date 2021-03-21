;; N-Queens Problem
;; Example: (queens 8)
;;
(defun queens (size)
   (queens* $size () 0 0))

; Internal recursive version.
(defun queens* (size board n m)
    (if (not (= $m $size))
        (progn
            ; First we check if we have a good position.
            (if (not (conflict $n $m $board))
                ; Yes we have a good position.
                (if (= (+ 1 $n) $size)
                    ; The board is full, we have a solution!
                    ; We have to dup the board because cons is destructive.
                    (print (cons (list $n $m) (dup $board)))
                    ; The board is not full, we can extend it more.
                    ; We have to dup the board because cons is destructive.
                    (queens* $size  (cons (list $n $m) (dup $board)) (+ 1 $n) 0)))
            ; In any case, good position or bad, we vary the m
            ; in order to try all combinations.
            ; We have to dup the board, because queens could modify the board.
            (queens* $size (dup $board) $n (+ 1 $m)))))

; Discovers if a piece threatens another.
(defun threat (i j a b)
  (or (= $i $a)
      (=  $j $b)
      (= (- $i $j) (- $a $b))
      (= (+ $i $j) (+ $a $b))))

; Discovers if a placement is OK.
(defun conflict (n m board)
   (if (empty? $board)
      false
      (if (threat $n $m (first (first $board)) (second (first $board)))
         true
         (conflict $n $m (cdr $board)))))

; First element of the list or the empty list.
(defun first (lst)
   (if (empty? $lst)
       nil
       (car $lst)))

; Second element of the list or the empty list.
(defun second (lst)
   (if (< (size $lst) 2)
        nil
        (car (cdr $lst))))