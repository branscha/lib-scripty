;; Filter function.
;; Example: (filter
;;             '(1 2 3 d f 4 5 x y z 6 t u v 7 8 9)
;;             (lambda (x) (number? $x)))
;;          ==>  [1, 2, 3, 4, 5, 6, 7, 8, 9]
;;
(defun filter (list predicate)
   (if (empty? $list)
      ()
      (let (the-rest=(filter (cdr $list) $predicate))
         (if (funcall $predicate (car $list))
            (cons (car $list) $the-rest)
            $the-rest))))

;; Mapping function.
;; Example: (mapcar
;;             '(1 2 3)
;;             (lambda (x) (* $x 2)))
;;          ==> [2, 4, 6]
;;
(defun mapcar (list f)
   (if (empty? $list)
      ()
      (let (the-rest=(mapcar (cdr $list) $f))
         (cons ($f (car $list)) $the-rest))))