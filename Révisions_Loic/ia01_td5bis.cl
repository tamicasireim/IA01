(defun prem_regle (regle)
  (cadr regle)
  )

(defun conc_regle (regle)
  (last regle)
  )

(prem_regle '(1 (B D E) F))
(conc_regle '(1 (B D E) F))

(defun regles_cand (but regle)
  (let (basereg '())
    (dolist (x regle basereg)
      (if (eq but (car(conc_regle x)))
	(push x basereg)
	)
      )
    )
  )

(regles_cand 'A '((1 (B D) F)(2 (D G) A)(3 (C F) A)))

(defun verif_ou (but regles faits)
  (let ((RC '())(bverifet nil))
    (progn
      (if (member but faits)
	T
	(progn
	  (setq RC (regles_cand but regles))
	  (loop
	    (if (AND (not (null RC)) (equal bverifet nil))
	      (if (verif_et (car RC) regles faits)
		(progn
		  (setq bverifet T)
		  (format t "La regle ~A valide ~A avec la premisse ~A. ~%" (caar RC) (caddr (car RC)) (cadr (car RC)))
		  )
		(pop RC)
		)
	      (return nil)
	      )
	    )
	  bverifet
	  )
	)
      )
    )
  )

(defun verif_et (r regles faits)
  (let ((prem '()) (regle_verifiee t))
    (progn
      (setq prem (prem_regle r))
      (loop 
	(if (AND (not (null (car prem))) (equal regle_verifiee (not nil)))
	  (if (not (verif_ou (car prem) regles faits))
	    (setq regle_verifiee nil)
	    (pop prem)
	    )
	  (return nil)
	  )
	)
      regle_verifiee
      )
    )
  )

(setq *regles '((1 (B D G) F)
		(2 (D G) A)
		(3 (C F) A)
		(4 (C) D)
		(5 (D) E)
		(6 (A) H)
		(7 (B) X)
		(8 (X C) A)
		)
      )

(verif_ou 'H *regles '(B C))
