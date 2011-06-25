;;; Compress & Uncompress functions.
;;; ANSI Common Lisp, Paul Graham
;;;
;;; Depenencies: LispCmd, MathCmd.

;; Compress a list with run-length-encoding.
;; Example: (compress '(1 1 1 0 1 0 0 0 0 1)) ==> ((3 1) 0 1 (4 0) 1)
;;
(defun compress (x)
  (if (list? $x)
     (compr (car $x) 1 (cdr $x))
     $x))

; The body of the compression, it takes the current head element
; and the number of times the element was encountered.
(defun compr (elt n lst)
 (if (empty? $lst)
     (list (n-elts $elt $n))
     (let (next=(car $lst))
        (if (eq $next $elt)
            (compr $elt (+ $n 1) (cdr $lst))
            (unshift (compr $next 1 (cdr $lst))
                     (n-elts $elt $n))))))

; Create a tuple if the element occurs more then once.
; Otherwise just the element.
(defun n-elts (elt n)
     (if (> $n 1)
         (list $n $elt)
         $elt))

;; Uncompress a compressed list.
;; Example: (uncompress '((3 1) 0 1 (4 0) 1)) ==> (1 1 1 0 1 0 0 0 0 1)
;;
(defun uncompress (lst)
   (if (empty? $lst)
       ()
       (let (elt=(car $lst) rest=(uncompress (cdr $lst)))
          (if (list? $elt)
              (append (apply list-of (car $elt) (car (cdr $elt))) $rest)
              (cons $elt $rest)))))

; Expand to a list of n times elt.
(defun list-of (n elt)
    (if (zero? $n)
        ()
        (cons $elt (list-of (- $n 1) $elt))))