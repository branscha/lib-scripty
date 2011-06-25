;; Memoization of functions. 
;;
;; Example 1: defvar fac2 (memoize fac)
;;            We define a new function fac2, so recursive
;;            calls will not be memoized. 
;;            Only end results are cached.
;;
;; Example 2: defvar fac (memoize $fac)
;;            Note we fetch the original lambda using $fac
;;            and we redefine the fac function so that all
;;            intermediate results are now cached.
;;
(defun memoize (f)
   (let (map=(map-create) forig=$f)
      (lambda (x)
         (if (map-key? $map $x)
            (map-get $map $x)
            (let (val=($forig $x))
               (progn
                  (map-set $map $x $val)
                  $val))))))
                  