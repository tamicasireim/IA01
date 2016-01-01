;Base de fait
(setq faits '(B C))

;Base de règles
(setq regles '( (R1 (B D E) (F))
	        (R2 (D G) (A))
		(R3 (C F) (A))
		(R4 (C) (D))
		(R5 (D) (E))
		(R6 (A) (H))
		(R7 (B) (X))
		(R8 (X C) (A))
		)
      )

(defun verifier (but)
  (or (appartient but *faits*)
      (let ((EC (regles_candidates but *regles*)) OK)
	(loop
	  (if (or OK (null EC)) (return nil))
	  (setq OK (verifier_et (pop EC)))
	  )
	)
      )
  )

(defun verifier_et (regle)
  (let ((OK t) (premisses (termes-condition regle)))
    (loop
      (if (or (not OK) (null premisses)) (return nil))
      (setq OK (verifier (pop premisses)))
      )
    OK
    )
  )

(defun appartient (but faits)
  (when (member but faits) t))
(defun candidats (but regles)
  (cond ((null regles) nil)
	((eq but (terme-conclusion (car regles)))
	 (cons (car regles) (regles-candidates but (cdr regles))))
	(t (regles-candidates but (cdr regles)))
	)
  )
(defun termes-conclusion (regle) (cadr regle))
(defun terme-condition (regle) (caddr regle))

