;****************************************************************************************
;************************************ BASE DE RÈGLES ************************************
;****************************************************************************************

(defun générer_BR ()
	(let (BR)
		(setq BR (ajouterListePokemon *baseRègles* *pokemons*))
		(setq BR (ajouterListeType BR *règlesTypes*))
		)
	)

(setq *baseRègles* ; règle = (id ((prémisses) (conclusions))) avec conclusion sous la forme (nouveau_valeur valeur(s))
	'( 
		; RÈGLES DÉFINISSANT LES MODIFICATEURS DE DÉGATS
	   (5 ((my_member pk2Type faiblesse1)) ((modificateurPk2 2))
	   		("Les attaques du pokémon 2 (~A) sont très efficaces ! ~%" (pk2)))
	   (6 ((my_member pk1Type faiblesse2)) ((modificateurPk1 2))
	   		("les attaques du pokémon 1 (~A) sont très efficaces ! ~%" (pk1)))

	   (7 ((my_member pk2Type resistance1)) ((modificateurPk2 0.5))
	   		("Les attaques du pokémon 2 (~A) ne sont pas très efficaces... ~%" (pk2)))
	   (8 ((my_member pk1Type resistance2)) ((modificateurPk1 0.5))
	   		("Les attaques du pokémon 1(~A) ne sont pas très efficaces... ~%" (pk1)))

	   (9 ((not_null faiblesse1) (not_member pk2Type faiblesse1) (not_null resistance1) (not_member pk2Type resistance1)) ((modificateurPk2 1))
	   		("Les attaques du pokémon 2 (~A) n'ont rien de particulier ~%" (pk2)))
	   (10 ((not_null faiblesse2) (not_member pk1Type faiblesse2) (not_null resistance2) (not_member pk1Type resistance2)) ((modificateurPk1 1))
	   		("Les attaques du pokémon 1 (~A) n'ont rien de particulier ~%" (pk1)))

	   	; RÈGLES RÉGISSANT QUI COMMENCE LORS DU COMBAT
	   (11 ((not_null pk1Speed) (not_null pk2Speed) (> pk1Speed pk2Speed)) ((pk1Commence 1))
	   		("Le Pokémon 1 (~A) commence ! ~%" (pk1)))
	   (12 ((not_null pk1Speed) (not_null pk2Speed) (> pk2Speed pk1Speed)) ((pk2Commence 1))
	   		("Le Pokémon 2 (~A) commence ! ~%" (pk2)))
	   
	   	; RÈGLES CALCULANT LES DÉGATS
	   (17 ((not_null modificateurPk1)) ((pk1degats (truncate (* (+ (/ pk1attaque pk2defense) 2) modificateurPk1))))
	   		("Le Pokémon 1 (~A) va infliger ~A dégat(s) par tour ! ~%" (pk1 pk1degats)))
	   (18 ((not_null modificateurPk2)) ((pk2degats (truncate (* (+ (/ pk2attaque pk1defense) 2) modificateurPk2))))
	   		("Le Pokémon 2 (~A) va infliger ~A dégat(s) par tour ! ~%" (pk2 pk2degats)))

	   	; RÈGLES DÉFINISSANT QUI GAGNE
	   (19 ((equal pk1Commence 1) (not_null pk1degats) (<= (winInXTurns pk1degats pk2pv) (winInXTurns pk2degats pk1pv))) ((gagnant pk1))
	   		("Le pokémon 1 (~A) a mis KO le pokémon 2 (~A) ! ~%" (pk1 pk2)))
	   (20 ((equal pk1Commence 1) (not_null pk1degats) (> (winInXTurns pk1degats pk2pv) (winInXTurns pk2degats pk1pv))) ((gagnant pk2))
	   		("Le pokémon 2 (~A) a mis KO le pokémon 1 (~A) ! ~%" (pk2 pk1)))
	   (21 ((equal pk2Commence 1) (not_null pk1degats) (>= (winInXTurns pk1degats pk2pv) (winInXTurns pk2degats pk1pv))) ((gagnant pk2))
	   		("Le pokémon 2 (~A) a mis KO le pokémon 1 (~A) ! ~%" (pk2 pk1)))
	   (22 ((equal pk2Commence 1) (not_null pk1degats) (< (winInXTurns pk1degats pk2pv) (winInXTurns pk2degats pk1pv))) ((gagnant pk1))
	   		("Le pokémon 1 (~A) a mis KO le pokémon 2 (~A) ! ~%" (pk1 pk2)))
	)
)
; FONCTION D'ACCES (pour rendre le reste du code plus indépendant de la forme de la base de règle)
(defun premisses_r (règle)
	(cadr règle))
(defun conclusions_r (règle)
	(caddr règle))
(defun nom_fait_conclusion (conclusion)
	(car conclusion))
(defun valeurs_fait_conclusion (conclusion)
	(cdr conclusion))

; FONCTIONS OUTILS POUR LA BASE DE RÈGLE
(defun texte (règle)
	(car (last règle)))
(defun texte_contenu (règle)
	(car (texte règle)))
(defun texte_variables (règle)
	(cadr (texte règle)))

; nouveaux opérateurs créés afin d'alléger la lecture de la base de règle
(defun not_null (x)
	(not (null x)))
(defun not_equal (x y)
	(not (equal x y)))
(defun my_member (x _list)
	(cond
		((null _list) nil)
		((equal x (car _list)) T)
		(T (my_member x (cdr _list)))
		)
	)
(defun not_member (x _list)
	(not (my_member x _list)))

;renvoie en combien de tours un pokémon mettra ko l'ennemi.
(defun winInXTurns (pkdegats adversePVs)
	(if (eq (mod adversePVs pkdegats) 0)
		(/ adversePVs pkdegats)
		(+ 1 (truncate (/ adversePVs pkdegats)))
		)
	)
(defun format_fromBase (texte BF arguments)
	(let (arguments_evalués)
		(dolist (argument arguments)
			(push (expressionAlgebrique argument BF) arguments_evalués)
			)
		(eval (append (list 'format 't texte) (reverse arguments_evalués)))
		)
	)


;****************************************************************************************
;GENERATION DES RÈGLES CALCULANT LES STATISTIQUES DES POKÉMONS
;****************************************************************************************

; EXEMPLE GENERE
;(pokémon_rule1 ((equal pk1 "salamèche")) ((pk1Type "feu") (pk1pv (calcul_pv 39 pk1level)) (pk1attaque (calcul_stat 52 pk1level))
;												(pk1defense (calcul_stat 43 pk1level)) (pk1Speed (calcul_stat 65 pk1level)))
;		("Pokémon 1 = ~A (lvl ~A) | PV ~A | ATT ~A | DEF ~A | SPD ~A ~%" (pk1 pk1level pk1pv pk1attaque pk1defense pk1Speed)))
;(pokémon_rule2 ((equal pk2 "salamèche")) ((pk2Type "feu") (pk2pv (calcul_pv 39 pk2level)) (pk2attaque (calcul_stat 52 pk2level))
;												(pk2defense (calcul_stat 43 pk2level)) (pk2Speed (calcul_stat 65 pk2level)))
;		("Pokémon 2 = ~A (lvl ~A) | PV ~A | ATT ~A | DEF ~A | SPD ~A ~%" (pk2 pk2level pk2pv pk2attaque pk2defense pk2Speed)))

(setq *pokemons* '( ("salamèche" 39 52 43 65 "feu")
					("carapuce" 44 48 65 43 "eau")
					("bulbizare" 45 49 49 65 "plante")
					("pikachu" 35 55 40 90 "electrique")
					("aquali" 130 65 60 65 "eau")
					("voltali" 65 65 60 130 "electrique")
					("pyroli" 65 130 60 65 "feu")
					)
	)

; pokemon = ("nom" pv attaque défense speed type)
(defun PKnom (pokemon)
	(car pokemon))
(defun PKpv (pokemon)
	(cadr pokemon))
(defun PKattaque (pokemon)
	(caddr pokemon))
(defun PKdefense (pokemon)
	(cadddr pokemon))
(defun PKspeed (pokemon)
	(cadddr (cdr pokemon)))
(defun PKtype (pokemon)
	(cadddr (cddr pokemon)))
(defun calcul_pv (baseStat level)
	(truncate (+ (+ level 10) (* (* 2 baseStat) (/ level 100)))))
(defun calcul_stat (baseStat level)
	(truncate (+ 5 (* (* 2 baseStat) (/ level 100)))))

(defun ajouterPokemon (BR pokemon)
	(let ((id (gentemp "pokemon-rule"))
		  (nouvelle_BR BR))
		(push 
			`(,id ((equal pk1 ,(PKnom pokemon) ))
					((pk1Type ,(PKtype pokemon) )
					(pk1pv (calcul_pv ,(PKpv pokemon) pk1level))
					(pk1attaque (calcul_stat ,(PKattaque pokemon) pk1level))
	   				(pk1defense (calcul_stat ,(PKdefense pokemon) pk1level))
	   				(pk1Speed (calcul_stat ,(PKspeed pokemon) pk1level)))
	   			("Pokémon 1 = ~A (type ~A, lvl ~A) | PV ~A | ATT ~A | DEF ~A | SPD ~A ~%" (pk1 pk1Type pk1level pk1pv pk1attaque pk1defense pk1Speed)))
			nouvelle_BR)
		(setq id (gentemp "pokemon-rule"))
		(push 
			`(,id ((equal pk2 ,(PKnom pokemon) ))
					((pk2Type ,(PKtype pokemon) )
					(pk2pv (calcul_pv ,(PKpv pokemon) pk1level))
					(pk2attaque (calcul_stat ,(PKattaque pokemon) pk2level))
	   				(pk2defense (calcul_stat ,(PKdefense pokemon) pk2level))
	   				(pk2Speed (calcul_stat ,(PKspeed pokemon) pk2level)))
	   			("Pokémon 2 = ~A (type ~A, lvl ~A) | PV ~A | ATT ~A | DEF ~A | SPD ~A ~%" (pk2 pk2Type pk2level pk2pv pk2attaque pk2defense pk2Speed)))
			nouvelle_BR)

		)
	)

(defun ajouterListePokemon (BR listePokemon)
	(let ((nouvelle_BR BR))
		(dolist (pokemon listePokemon nouvelle_BR)
			(setq nouvelle_BR (ajouterPokemon nouvelle_BR pokemon) )
			)
		)
	)

;****************************************************************************************
;GENERATION DES RÈGLES POUR DÉTERMINER LES TYPES & FAIBLESSES DES POKÉMONS
;****************************************************************************************

;EXEMPLE
;	   (type_rule1 ((equal pk1Type "feu")) ((faiblesse1 faiblesseFeu))
;	   		("Le pokémon 1 (~A) est de type feu et donc a pour faiblesses ~A ~%" (pk1 faiblesseFeu)))
;	   (type_rule2 ((equal pk1Type "feu")) ((resistance1 resistancesFeu))
;	   		("Le pokémon 1 (~A) est de type feu et donc a pour résistances ~A ~%" (pk1 resistanceFeu)))

(setq *règlesTypes* '( ("feu" faiblesseFeu resistanceFeu)
					   ("eau" faiblesseEau resistanceEau)
					   ("plante" faiblessePlante resistancePlante)
					   ("electrique" faiblesseElectrique resistanceElectrique)
					   )
	)
(defun type_nom (type)
	(car type))
(defun type_faiblesse (type)
	(cadr type))
(defun type_resistance (type)
	(caddr type))


(defun ajouterType (BR type)
	(let ((id (gentemp "type-rule"))
		  (nouvelle_BR BR))
		(push 
			`(,id ((equal pk1Type ,(type_nom type))) ((faiblesse1 ,(type_faiblesse type)))
	   		  	("Le pokémon 1 (~A) est de type ~A et donc a pour faiblesses ~A ~%" (pk1 pk1Type ,(type_faiblesse type))))
			nouvelle_BR)
		(setq id (gentemp "type-rule"))
		(push 
			`(,id ((equal pk1Type ,(type_nom type))) ((resistance1 ,(type_resistance type)))
	   		  	("Le pokémon 1 (~A) est de type ~A et donc a pour résistances ~A ~%" (pk1 pk1Type ,(type_resistance type))))
			nouvelle_BR)
		(setq id (gentemp "type-rule"))

		(push 
			`(,id ((equal pk2Type ,(type_nom type))) ((faiblesse2 ,(type_faiblesse type)))
	   		  	("Le pokémon 2 (~A) est de type ~A et donc a pour faiblesses ~A ~%" (pk2 pk2Type ,(type_faiblesse type))))
			nouvelle_BR)
		(setq id (gentemp "type-rule"))
		(push 
			`(,id ((equal pk2Type ,(type_nom type))) ((resistance2 ,(type_resistance type)))
	   		  	("Le pokémon 2 (~A) est de type ~A et donc a pour résistances ~A ~%" (pk2 pk2Type ,(type_resistance type))))
			nouvelle_BR)
		)
	)
(defun ajouterListeType (BR listeType)
	(let ((nouvelle_BR BR))
		(dolist (type listeType nouvelle_BR)
			(setq nouvelle_BR (ajouterType nouvelle_BR type) )
			)
		)
	)