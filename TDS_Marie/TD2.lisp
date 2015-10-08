;TD 2 de IA01

;definition de u et v
;dans le cas où l'expression est sous la forme (opérateur a b) (u expr) et (v expr) vont respectivement renvoyer a et b.
(defun u (expr)
  (cadr expr)
  )
(defun v (expr)
  (caddr expr)
  )

;ud et vd ont un fonctionnement analogue à u et v sauf qu'ils appelleront en plus la fonction dériv sur a et b (ud pour u dérivé)
(defun ud (expr var)
  (deriv (cadr expr) var)
  )
(defun vd (expr var)
  (deriv (caddr expr) var)
  )

;cette fonction dérive un terme par rapport à une variable.
(defun derivTerm (expr var)
  (if (eq expr var)
    1
    0
    )
  )
;cette fonction permet de dériver une addition. En effet, la dérivée d'une somme est la somme des dérivées.
(defun deriv+ (expr var)
  (list '+ 
	(ud expr var) 
	(vd expr var))
  )

;cete fonction permet de dériver un produit. on a : (uv)' = u'v + v'u
(defun deriv* (expr var)
  (list '+ 
	(list '* (ud expr var) (v expr)) 
	(list '* (vd expr var) (u expr)))
  )

;cette fonction dérive une expression. Seuls les opérateurs + et * sont implémentés, mais on pourrait très simplement en rajouter d'autres en rajoutant une ligne dans le case.
(defun deriv (expr var)
  (if (listp expr)
    (case (car expr) ((+) (deriv+ expr var))
      ((*) (deriv* expr var))
      (oterwise (print "ERREUR - OPERATEUR NON IMPLEMENTE")))
    (derivTerm expr var)))

;cette fonction dérive une expression et la simplifie.
(defun derivSimple (expr var)
  (simpl (if (listp expr)
	   (case (car expr) ((+) (deriv+ expr var))
	     ((*) (deriv* expr var))
	     (oterwise (print "ERREUR - OPERATEUR NON IMPLEMENTE")))
	   (derivTerm expr var))))

;partie simplification
; définition de us et vs. ces fonctions ont un comportement analogue à u et v sauf qu'ils simplifient l'expression renvoyée.
(defun us (expr)
  (simpl (cadr expr))
  )
(defun vs (expr)
  (simpl (caddr expr))
  )

;simplifie une addition.
(defun simpl+ (expr)
  (cond ((and (numberp (us expr)) (numberp (vs expr))) (+ (us expr) (vs expr)))
	((eq (us expr) 0) (vs expr))
	((eq (vs expr) 0) (us expr))
	(T (list '+ (us expr) (vs expr)))))
;simplifie une multiplication.
(defun simpl* (expr)
  (cond ((and (numberp (us expr)) (numberp (vs expr))) (* (us expr) (vs expr)))
	((or (eq (us expr) 0) (eq (vs expr) 0)) 0)
	((eq (us expr) 1) (vs expr))
	((eq (vs expr) 0) (us expr))
	(T (list '* (us expr) (vs expr)))))

;simplifie une expression, fonctionnement analgue à deriv.
(defun simpl (expr)
  (if (listp expr)
    (case (car expr) ((+) (simpl+ expr))
      ((*) (simpl* expr))
      (oterwise (print "ERREUR - OPERATEUR NON IMPLEMENTE")))
    expr))
