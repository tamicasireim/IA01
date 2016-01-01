;****************************************************************************************
;************************************ BASE DE FAITS *************************************
;****************************************************************************************
;	   (1 (pk1 "salamèche"))
;	   (2 (pk2 "carapuce"))
;	   (3 (pk1level 5))
;	   (4 (pk2level 10))

(setq *baseFaits*
	'( (type_fact1 (faiblesseFeu (list "sol" "eau" "roche")))
	   (type_fact2 (resistanceFeu (list "feu" "plante" "insecte" "acier" "glace")))
	   (type_fact3 (faiblesseEau (list "plante" "electrique")))
	   (type_fact4 (resistanceEau (list "eau" "feu" "sol" "roche")))
	   (type_fact5 (faiblessePlante (list "feu" "poison" "glace" "vol" "insecte")))
	   (type_fact6 (resistancePlante (list "sol" "roche" "eau")))
	   (type_fact7 (faiblesseElectrique (list "sol")))
	   (type_fact8 (resistanceElectrique (list "vol" "eau")))
	)
)

; FONCTION D'ACCES (pour rendre le reste du code plus indépendant de la forme de la base de règle)
(defun nom_fait (fait)
	(caadr fait))
(defun valeur_fait (fait)
	(cadadr fait))

; renvoie la valeur d'un fait de la base, ou bien nil si ce dernier n'est pas dans cette dernière
(defun valeur_fait_fromBase (nomfait BF &optional (warning nil))
	(loop																					; on parcourt l'ensemble de la base de fait
		(let ((fait (pop BF)))
			(cond
				((equal (nom_fait fait) nomfait) (return (valeur_fait fait)))				; jusqu'à arriver au fait dont on cherche la valeur, dont on retourne la valeur
				((null fait)																; si on est arrivé au bout de la BF sans succès
					(when (eq warning T) (format t "Fait ~S non initialisé ~&" nomfait))		; on prévient l'utilisateur si ce dernier l'a souhaité
					(return nil))																; on retourne nil
				)
			)
		)
	)

(defun ajouter_fait (BF nom_fait valeur_fait)
	(push (list (gentemp "fait") (list nom_fait valeur_fait)) BF))
