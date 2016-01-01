;****************************************************************************************
;************************************ CHAINAGE AVANT ************************************
;****************************************************************************************

; évalue la valeur d'une expression Algébrique où tous les symboles sont remplacés par leurs valeurs dans la base de fait 
(defun expressionAlgebrique (expression BF)
	(cond															; on analyse la forme de l'expression
		((null expression) nil)										; si l'expression est nulle, on renvoie nil.
		((symbolp expression) (valeur_fait_fromBase expression BF))	; si c'est un symbole, on remplace son expression par sa valeur dans la BF
		((and (listp expression))									; si c'est une liste, on considère qu'elle est de forme (opérateur argument1 argument2...)
			(let (suite)
				(dolist (x (cdr expression))
					(push (expressionAlgebrique x BF) suite))			; on appelle donc récursivement expressionAlgebrique pour chaque argument
				(eval (cons (car expression) (reverse suite)))
				)
			)
		(T expression)												; sinon, il s'agit d'une constante (un nombre notamment), expression est donc évaluée à elle-même.
		)
	)


; Détermine si une règle peut-être appliquée ou non (si toutes ses prémisses sont valides) (renvoie T si oui, nil sinon)
(defun règle_applicable? (règle BF)
	(let ((premisses (premisses_r règle)))
		(loop
			(let ((premisse (pop premisses)))									; pour chacune des prémisses de la règle
				; (print premisse)
				(cond
					((null premisse) (return T))								; si toutes les prémisses sont valides, la règle peut-être appliquée
					((null (expressionAlgebrique premisse BF)) (return nil))	; si l'une d'entre elle n'est pas valide (vaut nil), la règle n'est pas applicable
					)
				)
			)
		)
	)

; renvoie la liste de toutes les règles applicables de BR sachant BF.
(defun règles_applicables (BF BR)
	(if (null BR)												; on parcourt récursivement la BR
		nil
		(if (règle_applicable? (car BR) BF) 					; si la règle est applicable
			(cons (car BR) (règles_applicables BF (cdr BR)))		; on l'ajoute à la liste retournée
			(règles_applicables BF (cdr BR))						; sinon on teste la règle suivante
			)
		)
	)

; applique une règle à la base de fait
(defun appliquer_règle (règle BF)
	(dolist (conclusion (conclusions_r règle) BF)											; pour chacune des conclusions de la règle
		(let ((valeurs_conclusion (valeurs_fait_conclusion conclusion))
			   (valeurs nil))
			(if (null (cdr valeurs_conclusion))													; si on ne doit donner qu'une valeur au nouveau fait
				(setq valeurs (expressionAlgebrique (car valeurs_conclusion) BF))					; on calcule tout simplement sa valeur.
				(progn
					(dolist (valeur valeurs_conclusion)											; sinon, on concatène toutes les valeurs du nouveau fait dans une liste
						(setq valeurs (append (list (expressionAlgebrique valeur BF)) valeurs)))	
					(push 'list valeurs)															; le fait sera sous la forme (nom_fait (list v1 v2...))
					)
				)
			;(print (nom_fait_conclusion conclusion))

			(setq BF (ajouter_fait BF (nom_fait_conclusion conclusion) valeurs))				; on insère le fait dans la nouvelle BF sous la forme (num_fait (fait))
			)
		)
	)

(defun ajouter_fait (BF nom_fait valeur_fait)
	(push (list (gentemp "fait") (list nom_fait valeur_fait)) BF))

(defun chainage_avant (but BF BR)
	(let ((valeur_but (valeur_fait_fromBase but BF))
		  (ensemble_conflits (règles_applicables BF BR))
		  (nouvelle_BR BR)
		  (nouvelle_BF BF)
		  (sizeBF (length BF)))
		(cond
			((not_null valeur_but) valeur_but)									; si le but est dans la base de fait, on arrête l'algorithme
			((null ensemble_conflits) nil)													; s'il n'y a plus de règles à appliquer, le but n'est pas vérifiable.
			(T (dolist (règle ensemble_conflits nouvelle_BF)								; sinon, pour chaque règle de l'ensemble des règles applicables
					(setq nouvelle_BF (appliquer_règle règle nouvelle_BF))					; on  l'applique à la base de faits
					; (print règle)
					(format_fromBase (texte_contenu règle) nouvelle_BF (texte_variables règle))	; on affiche le texte associé à la règle
					(setq nouvelle_BR (remove règle nouvelle_BR))								; on la retire de la base de règle 
					)
				; (print nouvelle_BF)
				(chainage_avant but nouvelle_BF nouvelle_BR)									; on relance la recherche
			))
		)
	)

; (chainage_avant 'gagnant *basefaits1* *baserègles* )