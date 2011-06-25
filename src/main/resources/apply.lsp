;; Apply defined using funcall.
;; Example: (appy fact '(10))  ==> (fact 10) ==> 3628800
;;
(defun apply (f parms)
   (eval  (append '(funcall $f) $parms)))