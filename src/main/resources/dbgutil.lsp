;;; Common DbgCmd combinations.
;;;
;;; e '<expr>    Start debugging an expression.
;;; t            Terminate the debug session.
;;; x            Stack dump.
;;; s            Step + stack.
;;; b            Backstep + stack.
;;; sover        Step over (the parameter) + stack.
;;; sout         Step out (of the expression) + stack.
;;; r            Run until finished.
;;; rready       Run until the parmeters of the topmost expression are evaluated and the expression itself can be executed.
;;; rresult      Run until a result is reached.
;;; result       Print the result.
;;; ctx          Print the current context (of the topmost expression).
;;; v '<expr>    View an expression, evaluated in the topmost context.
;;; restart      Restart evaluation, start from the beginning.
;;; df           Drop the topmost frame + stack.

;; Start debugging an expression.
;; Don't forget to quote the expression.
;; Example: e '(+ 1 2 3)
;;
(defun e (expr)
   (dbg-expr-x $expr))

;; Show the stack.
;;
(defun x ()
   (dbg-stack))

;; Single step.
;;
(defun s ()
   (if (dbg-moresteps?)
	   (progn (dbg-stepin)
	          (dbg-stack))
	   (print "No more steps.")))

(defun b ()
   (if (dbg-moresteps?)
	   (progn (dbg-back)
	          (dbg-stack))
	   (print "No more steps.")))

;; Step over.
;;
(defun sover ()
   (if (dbg-moresteps?)
	   (progn (dbg-stepover)
	          (dbg-stack))
	   (print "No more steps.")))

;; Step over.
;;
(defun sout ()
   (if (dbg-moresteps?)
	   (progn (dbg-stepout)
	          (dbg-stack))
	   (print "No more steps.")))

;; Run.
;;
(defun r ()
   (progn
      (if (dbg-moresteps?) (dbg-run) (print "No more steps."))
      (if (dbg-result?) (result))))

;; Terminate.
;;
(defun t ()
   (dbg-terminate))

;; Run ready.
;;
(defun rready ()
   (if (dbg-moresteps?)
	   (progn
	      (dbg-runready)
	      (dbg-stack))
	   (print "No more steps.")))

;; Run result.
;;
(defun rresult ()
   (progn
      (if (dbg-moresteps?) (dbg-runresult))
      (if (dbg-result?) (result))))

;; Show the result.
;;
(defun result ()
   (if (dbg-result?)
      (print (dbg-result))
      (print "No result available.")))

;; Context info.
;;
(defun ctx ()
   (dbg-ctx))

;; View in the context of the stack.
;; Don't forget to quote your expression.
;;
(defun v (expr)
   (print (dbg-eval-x $expr)))

;; Restart evaluation.
;;
(defun restart ()
   (dbg-restart))

;; Restart evaluation.
;;
(defun df ()
   (progn
      (dbg-dropframe)
      (dbg-stack)))

